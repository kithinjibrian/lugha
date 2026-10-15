//! The server's state: open documents, settings, the checks waiting to run,
//! which files each main loaded, and what has been published.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::{Duration, Instant};

use lsp_server::{Message, Notification};
use lsp_types::notification::{LogMessage, Notification as _, PublishDiagnostics};
use lsp_types::{
    CompletionItem, CompletionParams, DidChangeConfigurationParams, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    GotoDefinitionParams, Hover, HoverParams, InitializeParams, Location, LogMessageParams,
    MessageType, PublishDiagnosticsParams, SignatureHelp, SignatureHelpParams,
};
use serde_json::Value;

use super::check;
use super::completion;
use super::definition;
use super::documents::{Documents, uri_to_path};
use super::hover::{self, Snapshot};
use super::publish::{self, Store};
use super::signature;
use super::targets::{self, Target};

/// How long typing must pause before a changed document is checked.
const DELAY: Duration = Duration::from_millis(200);

/// Everything the server knows between messages.
pub(super) struct Server {
    docs: Documents,
    folders: Vec<PathBuf>,
    entries: Vec<String>,
    /// Changed files and when they may be checked.
    pending: HashMap<PathBuf, Instant>,
    /// For each main file checked, every file its last check loaded.
    seen: HashMap<PathBuf, Vec<PathBuf>>,
    /// Each main file's last check, for hover.
    snapshots: HashMap<PathBuf, Rc<Snapshot>>,
    /// Each main file's last check that had a tree, for completion and
    /// signature help while the current text doesn't lex (decisions 29, 39).
    last_good: HashMap<PathBuf, Rc<Snapshot>>,
    store: Store,
}

impl Server {
    /// A server for the workspace and options of `params`; returns log lines too.
    pub(super) fn new(params: &InitializeParams) -> (Server, Vec<Message>) {
        let mut folders: Vec<PathBuf> = params
            .workspace_folders
            .iter()
            .flatten()
            .filter_map(|f| uri_to_path(f.uri.as_str()))
            .collect();
        // `rootUri` is deprecated in favour of `workspaceFolders`, but older
        // clients send only it.
        #[allow(deprecated)]
        if let Some(root) = params
            .root_uri
            .as_ref()
            .and_then(|u| uri_to_path(u.as_str()))
            && !folders.contains(&root)
        {
            folders.push(root);
        }
        let mut server = Server {
            docs: Documents::default(),
            folders,
            entries: Vec::new(),
            pending: HashMap::new(),
            seen: HashMap::new(),
            snapshots: HashMap::new(),
            last_good: HashMap::new(),
            store: Store::default(),
        };
        let mut out = Vec::new();
        if let Some(options) = &params.initialization_options {
            server.set_entries(options, &mut out);
        }
        (server, out)
    }

    /// Handles a notification; returns messages to send now (log lines).
    pub(super) fn notify(&mut self, n: Notification, now: Instant) -> Vec<Message> {
        let mut out = Vec::new();
        match n.method.as_str() {
            "textDocument/didOpen" => {
                if let Ok(p) = serde_json::from_value::<DidOpenTextDocumentParams>(n.params)
                    && let Some(path) = uri_to_path(p.text_document.uri.as_str())
                {
                    self.docs.set(&path, p.text_document.text);
                    self.pending.insert(path, now);
                }
            }
            "textDocument/didChange" => {
                if let Ok(p) = serde_json::from_value::<DidChangeTextDocumentParams>(n.params) {
                    let path = uri_to_path(p.text_document.uri.as_str());
                    // Full sync: the last change holds the whole text.
                    match (path, p.content_changes.into_iter().last()) {
                        (Some(path), Some(change)) if self.docs.text(&path).is_some() => {
                            self.docs.set(&path, change.text);
                            self.pending.insert(path, now + DELAY);
                        }
                        _ => out.push(log(
                            MessageType::WARNING,
                            "didChange for a document that isn't open",
                        )),
                    }
                }
            }
            "textDocument/didSave" => {
                if let Ok(p) = serde_json::from_value::<DidSaveTextDocumentParams>(n.params)
                    && let Some(path) = uri_to_path(p.text_document.uri.as_str())
                {
                    self.pending.insert(path, now);
                }
            }
            "textDocument/didClose" => {
                if let Ok(p) = serde_json::from_value::<DidCloseTextDocumentParams>(n.params)
                    && let Some(path) = uri_to_path(p.text_document.uri.as_str())
                {
                    // Unsaved text is gone: what the disk says counts again.
                    self.docs.close(&path);
                    self.pending.insert(path, now);
                }
            }
            "workspace/didChangeConfiguration" => {
                if let Ok(p) = serde_json::from_value::<DidChangeConfigurationParams>(n.params) {
                    // VS Code sends `{ "lugha": { … } }` for its section; others the section itself.
                    let settings = p.settings.get("lugha").cloned().unwrap_or(p.settings);
                    self.set_entries(&settings, &mut out);
                    for path in self.docs.paths() {
                        self.pending.insert(path, now);
                    }
                }
            }
            // Anything else (`initialized`, `$/…`) needs nothing from us.
            _ => {}
        }
        out
    }

    /// When the next pending check may run.
    pub(super) fn deadline(&self) -> Option<Instant> {
        self.pending.values().min().copied()
    }

    /// Runs every check whose time has come; returns the messages to send.
    pub(super) fn run_due(&mut self, now: Instant) -> Vec<Message> {
        let due: Vec<PathBuf> = self
            .pending
            .iter()
            .filter(|(_, at)| **at <= now)
            .map(|(p, _)| p.clone())
            .collect();
        let mut out = Vec::new();
        let mut targets: Vec<Target> = Vec::new();
        for file in due {
            self.pending.remove(&file);
            for target in self.plan(&file, &mut out) {
                if !targets.contains(&target) {
                    targets.push(target);
                }
            }
        }
        for target in targets {
            let outcome = check::check(&target.main, &self.docs, target.alone);
            out.extend(outcome.log.iter().map(|l| log(MessageType::ERROR, l)));
            if !target.alone {
                self.seen.insert(target.main.clone(), outcome.loaded);
            }
            match outcome.snapshot {
                Some(snapshot) => {
                    let snapshot = Rc::new(snapshot);
                    // Only a check of text that parsed: its expressions are
                    // the whole program (decision 39).
                    if !snapshot.broken {
                        self.last_good.insert(target.main.clone(), snapshot.clone());
                    }
                    self.snapshots.insert(target.main.clone(), snapshot)
                }
                // A program that no longer loads or parses has nothing to hover.
                None => self.snapshots.remove(&target.main),
            };
            for file in self.store.update(&target.main, outcome.files) {
                out.extend(self.publish(&file));
            }
        }
        out
    }

    /// Answers a hover; also returns what the checks it had to run first
    /// publish, so a hover never answers from older text (decision 21).
    pub(super) fn hover(
        &mut self,
        params: &HoverParams,
        now: Instant,
    ) -> (Option<Hover>, Vec<Message>) {
        let at = &params.text_document_position_params;
        let (file, out) = self.fresh(at.text_document.uri.as_str(), now);
        let hover = file.and_then(|f| hover::answer(self.snapshot(&f)?, &f, at.position));
        (hover, out)
    }

    /// Answers Go to Definition, as fresh as a hover (decision 26).
    pub(super) fn definition(
        &mut self,
        params: &GotoDefinitionParams,
        now: Instant,
    ) -> (Option<Location>, Vec<Message>) {
        let at = &params.text_document_position_params;
        let (file, out) = self.fresh(at.text_document.uri.as_str(), now);
        let found = file.and_then(|f| definition::answer(self.snapshot(&f)?, &f, at.position));
        (found, out)
    }

    /// The path of `uri`, after running its pending check if it has one;
    /// with what that check publishes.
    fn fresh(&mut self, uri: &str, now: Instant) -> (Option<PathBuf>, Vec<Message>) {
        let Some(file) = uri_to_path(uri) else {
            return (None, Vec::new());
        };
        let mut out = Vec::new();
        if self.pending.contains_key(&file) {
            self.pending.insert(file.clone(), now);
            out = self.run_due(now);
        }
        (Some(file), out)
    }

    /// The last check that loaded `file`, among the checks planned for it.
    fn snapshot(&self, file: &Path) -> Option<&Snapshot> {
        Self::loaded(&self.snapshots, &self.plan(file, &mut Vec::new()), file)
    }

    /// The first of `checks` for `targets` that loaded `file`.
    fn loaded<'a>(
        checks: &'a HashMap<PathBuf, Rc<Snapshot>>,
        targets: &[Target],
        file: &Path,
    ) -> Option<&'a Snapshot> {
        // Planning again only reads: rejected entries were logged when checked.
        targets
            .iter()
            .filter_map(|t| checks.get(&t.main))
            .map(|s| &**s)
            .find(|s| s.has(file))
    }

    /// Answers signature help, from the last check that parsed as
    /// completion does (decision 35).
    pub(super) fn signature_help(
        &mut self,
        params: &SignatureHelpParams,
        now: Instant,
    ) -> (Option<SignatureHelp>, Vec<Message>) {
        let at = &params.text_document_position_params;
        let (file, out) = self.fresh(at.text_document.uri.as_str(), now);
        let help = file.and_then(|f| {
            let targets = self.plan(&f, &mut Vec::new());
            let text = match self.docs.text(&f) {
                Some(text) => text.to_string(),
                None => std::fs::read_to_string(&f).ok()?,
            };
            let current = Self::loaded(&self.snapshots, &targets, &f)
                .and_then(|s| signature::answer(s, &f, &text, at.position));
            // The call being typed is a broken statement the current check
            // dropped, so only the last good check may know its instance: its
            // filled-in signature wins over a written one (decision 34).
            let filled = |h: &SignatureHelp| h.signatures[0].documentation.is_some();
            if current.as_ref().is_some_and(filled) {
                return current;
            }
            let older = Self::loaded(&self.last_good, &targets, &f)
                .and_then(|s| signature::answer(s, &f, &text, at.position));
            match older {
                Some(help) if filled(&help) || current.is_none() => Some(help),
                _ => current,
            }
        });
        (help, out)
    }

    /// Answers completion from the last check that parsed (decision 29),
    /// as fresh as a hover.
    pub(super) fn completion(
        &mut self,
        params: &CompletionParams,
        now: Instant,
    ) -> (Vec<CompletionItem>, Vec<Message>) {
        let at = &params.text_document_position;
        let (file, out) = self.fresh(at.text_document.uri.as_str(), now);
        let items = file
            .and_then(|f| {
                let targets = self.plan(&f, &mut Vec::new());
                // The current text's check first (milestone 75); the last one with
                // a tree only when the current text didn't lex (decision 39).
                let snapshot = Self::loaded(&self.snapshots, &targets, &f)
                    .or_else(|| Self::loaded(&self.last_good, &targets, &f))?;
                let text = match self.docs.text(&f) {
                    Some(text) => text.to_string(),
                    None => std::fs::read_to_string(&f).ok()?,
                };
                Some(completion::answer(snapshot, &f, &text, at.position))
            })
            .unwrap_or_default();
        (items, out)
    }

    fn plan(&self, file: &Path, out: &mut Vec<Message>) -> Vec<Target> {
        let text = match self.docs.text(file) {
            Some(text) => text.to_string(),
            // A closed or saved-only file: its text is on disk. One that
            // can't be read is checked alone, which reports it.
            None => std::fs::read_to_string(file).unwrap_or_default(),
        };
        let seen: Vec<PathBuf> = self
            .seen
            .iter()
            .filter(|(main, loaded)| main.as_path() != file && loaded.iter().any(|l| l == file))
            .map(|(main, _)| main.clone())
            .collect();
        let plan = targets::plan(file, &text, &self.folders, &self.entries, &seen, |p| {
            p.is_file()
        });
        for (entry, reason) in &plan.rejected {
            out.push(log(
                MessageType::WARNING,
                &format!("lugha.entries: skipped `{entry}`: {reason}"),
            ));
        }
        plan.targets
    }

    fn publish(&self, file: &Path) -> Option<Message> {
        let params = PublishDiagnosticsParams {
            uri: publish::uri(file)?,
            diagnostics: self.store.for_file(file),
            version: None,
        };
        Some(Notification::new(PublishDiagnostics::METHOD.to_string(), params).into())
    }

    fn set_entries(&mut self, settings: &Value, out: &mut Vec<Message>) {
        match settings.get("entries") {
            None => {}
            Some(Value::Array(items)) if items.iter().all(Value::is_string) => {
                self.entries = items
                    .iter()
                    .filter_map(|i| i.as_str().map(String::from))
                    .collect();
            }
            Some(other) => {
                self.entries.clear();
                out.push(log(
                    MessageType::WARNING,
                    &format!("lugha.entries must be a list of strings, not {other}"),
                ));
            }
        }
    }
}

fn log(kind: MessageType, message: &str) -> Message {
    let params = LogMessageParams {
        typ: kind,
        message: message.to_string(),
    };
    Notification::new(LogMessage::METHOD.to_string(), params).into()
}

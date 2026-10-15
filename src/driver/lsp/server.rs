//! The server's state: open documents, settings, the checks waiting to run,
//! which files each main loaded, and what has been published.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use lsp_server::{Message, Notification};
use lsp_types::notification::{LogMessage, Notification as _, PublishDiagnostics};
use lsp_types::{
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams, LogMessageParams,
    MessageType, PublishDiagnosticsParams,
};
use serde_json::Value;

use super::check;
use super::documents::{Documents, uri_to_path};
use super::publish::{self, Store};
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
            for file in self.store.update(&target.main, outcome.files) {
                out.extend(self.publish(&file));
            }
        }
        out
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

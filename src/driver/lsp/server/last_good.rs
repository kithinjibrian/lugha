//! The requests answered from the last check that parsed when the current
//! text doesn't lex: signature help, completion (decisions 29, 35, 39) and
//! semantic tokens (milestone 154). A child of `server` for its state.

use std::time::Instant;

use lsp_server::Message;
use lsp_types::{
    CompletionItem, CompletionParams, SemanticTokens, SignatureHelp, SignatureHelpParams,
};

use super::Server;
use crate::driver::lsp::token_names::Names;
use crate::driver::lsp::{completion, signature, tokens};

impl Server {
    /// Answers signature help, from the last check that parsed as
    /// completion does (decision 35).
    pub(in crate::driver::lsp) fn signature_help(
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
    pub(in crate::driver::lsp) fn completion(
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
                Some(completion::answer(
                    snapshot,
                    &f,
                    &text,
                    at.position,
                    self.snippets,
                ))
            })
            .unwrap_or_default();
        (items, out)
    }

    /// Answers semantic tokens (milestone 154): names from the current
    /// check, or the last that parsed while the text doesn't lex.
    pub(in crate::driver::lsp) fn semantic_tokens(
        &mut self,
        uri: &str,
        now: Instant,
    ) -> (Option<SemanticTokens>, Vec<Message>) {
        let (file, out) = self.fresh(uri, now);
        let found = file.and_then(|f| {
            let text = self.docs.text(&f)?;
            let targets = self.plan(&f, &mut Vec::new());
            let snapshot = Self::loaded(&self.snapshots, &targets, &f)
                .or_else(|| Self::loaded(&self.last_good, &targets, &f));
            let names = snapshot.and_then(|s| {
                let (base, checked) = s.base(&f)?;
                Some((Names::new(s, base), checked))
            });
            Some(tokens::answer(text, names.as_ref().map(|(n, c)| (n, *c))))
        });
        (found, out)
    }
}

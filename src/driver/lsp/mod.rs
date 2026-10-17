//! `lughac lsp` (v16 draft decisions 13–17, milestone 70): a language
//! server over stdin and stdout that publishes `lughac check`'s diagnostics
//! for the editor's text, saved or not.
//!
//! One thread handles messages and runs checks between them; `lsp-server`
//! adds its own reader and writer threads. Never runs `git`, never writes a
//! file, opens no port.

mod check;
mod code_actions;
mod completion;
mod definition;
mod docs;
mod documents;
mod fixes;
mod formatting;
mod hover;
mod publish;
mod reexports;
mod references;
mod rename;
mod server;
mod signature;
mod snippets;
mod targets;

use std::io::Write;
use std::time::{Duration, Instant};

use lsp_server::{Connection, ErrorCode, Message, ProtocolError, Response};
use lsp_types::{
    CodeActionKind, CodeActionOptions, CodeActionParams, CodeActionProviderCapability,
    CompletionOptions, CompletionParams, CompletionResponse, DocumentFormattingParams,
    GotoDefinitionParams, GotoDefinitionResponse, HoverParams, HoverProviderCapability,
    InitializeParams, OneOf, PositionEncodingKind, SaveOptions, ServerCapabilities, ServerInfo,
    SignatureHelpOptions, SignatureHelpParams, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
};
use lsp_types::{ReferenceParams, RenameOptions, RenameParams, TextDocumentPositionParams};
use thiserror::Error;

use self::server::Server;

/// Why the server stopped other than by `shutdown` and `exit`.
#[derive(Debug, Error)]
enum LspError {
    #[error("{0}")]
    Protocol(#[from] ProtocolError),
    #[error("bad initialize parameters: {0}")]
    Params(#[from] serde_json::Error),
    #[error("the client went away")]
    Disconnected,
    #[error("{0}")]
    Io(#[from] std::io::Error),
}

/// Runs the server until the client ends it. Returns the exit code: 0 after
/// `shutdown` and `exit`, 1 after `exit` alone (as the protocol asks), 2
/// when the connection breaks (a malformed message, closed streams).
pub(super) fn serve(stderr: &mut dyn Write) -> u8 {
    let (connection, threads) = Connection::stdio();
    let result = run(&connection);
    // Closing our side lets the writer thread finish.
    drop(connection);
    // The reader thread's error (a malformed message) explains a lost
    // connection better than the loop's own "went away".
    let error = match (result, threads.join()) {
        (Ok(code), Ok(())) => return code,
        (_, Err(error)) => LspError::Io(error),
        (Err(error), Ok(())) => error,
    };
    // The client shows the server's stderr in its log; if that fails too,
    // the exit code still says it.
    let _ = writeln!(stderr, "error: lughac lsp: {error}");
    2
}

fn run(connection: &Connection) -> Result<u8, LspError> {
    let (id, params) = connection.initialize_start()?;
    let params: InitializeParams = serde_json::from_value(params)?;
    let (mut server, logs) = Server::new(&params);
    let result = serde_json::json!({
        "capabilities": capabilities(),
        "serverInfo": ServerInfo {
            name: "lughac".to_string(),
            version: Some(env!("CARGO_PKG_VERSION").to_string()),
        },
    });
    connection.initialize_finish(id, result)?;
    send(connection, logs)?;
    loop {
        // With nothing pending, wake up now and then anyway: cheaper than a
        // second receive path.
        let at = server
            .deadline()
            .unwrap_or_else(|| Instant::now() + Duration::from_secs(3600));
        match connection.receiver.recv_deadline(at) {
            Ok(Message::Request(request)) => {
                if connection.handle_shutdown(&request)? {
                    return Ok(0);
                }
                if request.method == "textDocument/formatting" {
                    let reply =
                        match serde_json::from_value::<DocumentFormattingParams>(request.params) {
                            Ok(params) => {
                                let found = server.formatting(params.text_document.uri.as_str());
                                let name = found.as_ref().map_or(String::new(), |(n, _)| n.clone());
                                formatting::reply(request.id, &name, found.map(|(_, f)| f))
                            }
                            Err(error) => vec![
                                Response::new_err(
                                    request.id,
                                    ErrorCode::InvalidParams as i32,
                                    error.to_string(),
                                )
                                .into(),
                            ],
                        };
                    send(connection, reply)?;
                    continue;
                }
                let reply = match request.method.as_str() {
                    "textDocument/hover" => {
                        match serde_json::from_value::<HoverParams>(request.params) {
                            Ok(params) => {
                                let (hover, published) = server.hover(&params, Instant::now());
                                send(connection, published)?;
                                Response::new_ok(request.id, hover)
                            }
                            Err(error) => Response::new_err(
                                request.id,
                                ErrorCode::InvalidParams as i32,
                                error.to_string(),
                            ),
                        }
                    }
                    "textDocument/codeAction" => {
                        match serde_json::from_value::<CodeActionParams>(request.params) {
                            Ok(params) => {
                                let (actions, published) =
                                    server.code_action(&params, Instant::now());
                                send(connection, published)?;
                                Response::new_ok(request.id, actions)
                            }
                            Err(error) => Response::new_err(
                                request.id,
                                ErrorCode::InvalidParams as i32,
                                error.to_string(),
                            ),
                        }
                    }
                    "textDocument/signatureHelp" => {
                        match serde_json::from_value::<SignatureHelpParams>(request.params) {
                            Ok(params) => {
                                let (help, published) =
                                    server.signature_help(&params, Instant::now());
                                send(connection, published)?;
                                Response::new_ok(request.id, help)
                            }
                            Err(error) => Response::new_err(
                                request.id,
                                ErrorCode::InvalidParams as i32,
                                error.to_string(),
                            ),
                        }
                    }
                    "textDocument/completion" => {
                        match serde_json::from_value::<CompletionParams>(request.params) {
                            Ok(params) => {
                                let (items, published) = server.completion(&params, Instant::now());
                                send(connection, published)?;
                                Response::new_ok(request.id, CompletionResponse::Array(items))
                            }
                            Err(error) => Response::new_err(
                                request.id,
                                ErrorCode::InvalidParams as i32,
                                error.to_string(),
                            ),
                        }
                    }
                    "textDocument/references" => {
                        match serde_json::from_value::<ReferenceParams>(request.params) {
                            Ok(params) => {
                                let (found, published) = server.references(&params, Instant::now());
                                send(connection, published)?;
                                Response::new_ok(request.id, found)
                            }
                            Err(error) => invalid(request.id, &error),
                        }
                    }
                    "textDocument/prepareRename" | "textDocument/rename" => {
                        let prepare = request.method == "textDocument/prepareRename";
                        let parsed = if prepare {
                            serde_json::from_value::<TextDocumentPositionParams>(request.params)
                                .map(|p| (p, None))
                        } else {
                            serde_json::from_value::<RenameParams>(request.params)
                                .map(|p| (p.text_document_position, Some(p.new_name)))
                        };
                        match parsed {
                            Ok((at, new)) => {
                                let (answer, published) =
                                    server.rename(&at, new.as_deref(), Instant::now());
                                send(connection, published)?;
                                match answer {
                                    Ok(value) => Response::new_ok(request.id, value),
                                    // The reason is what the editor shows.
                                    Err(reason) => Response::new_err(
                                        request.id,
                                        ErrorCode::RequestFailed as i32,
                                        reason,
                                    ),
                                }
                            }
                            Err(error) => invalid(request.id, &error),
                        }
                    }
                    "textDocument/definition" => {
                        match serde_json::from_value::<GotoDefinitionParams>(request.params) {
                            Ok(params) => {
                                let (found, published) = server.definition(&params, Instant::now());
                                send(connection, published)?;
                                let found = found.map(GotoDefinitionResponse::Scalar);
                                Response::new_ok(request.id, found)
                            }
                            Err(error) => Response::new_err(
                                request.id,
                                ErrorCode::InvalidParams as i32,
                                error.to_string(),
                            ),
                        }
                    }
                    _ => Response::new_err(
                        request.id,
                        ErrorCode::MethodNotFound as i32,
                        format!("lughac lsp doesn't handle `{}`", request.method),
                    ),
                };
                send(connection, [reply.into()])?;
            }
            Ok(Message::Notification(n)) if n.method == "exit" => return Ok(1),
            Ok(Message::Notification(n)) => send(connection, server.notify(n, Instant::now()))?,
            // We send no requests, so no response is awaited.
            Ok(Message::Response(_)) => {}
            Err(timeout) if timeout.is_timeout() => {}
            Err(_) => return Err(LspError::Disconnected),
        }
        send(connection, server.run_due(Instant::now()))?;
    }
}

fn send(
    connection: &Connection,
    messages: impl IntoIterator<Item = Message>,
) -> Result<(), LspError> {
    for message in messages {
        connection
            .sender
            .send(message)
            .map_err(|_| LspError::Disconnected)?;
    }
    Ok(())
}

/// The reply to a request whose parameters didn't parse.
fn invalid(id: lsp_server::RequestId, error: &serde_json::Error) -> Response {
    Response::new_err(id, ErrorCode::InvalidParams as i32, error.to_string())
}

fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(PositionEncodingKind::UTF16),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: Default::default(),
        })),
        document_formatting_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
            code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
            ..CodeActionOptions::default()
        })),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: Some(vec![",".to_string()]),
            ..SignatureHelpOptions::default()
        }),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".to_string()]),
            ..CompletionOptions::default()
        }),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(false),
                })),
                ..TextDocumentSyncOptions::default()
            },
        )),
        ..ServerCapabilities::default()
    }
}

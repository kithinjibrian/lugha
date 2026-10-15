//! `lughac lsp` (v16 draft decisions 13–17, milestone 70): a language
//! server over stdin and stdout that publishes `lughac check`'s diagnostics
//! for the editor's text, saved or not.
//!
//! One thread handles messages and runs checks between them; `lsp-server`
//! adds its own reader and writer threads. Never runs `git`, never writes a
//! file, opens no port.

mod check;
mod definition;
mod documents;
mod hover;
mod publish;
mod server;
mod targets;

use std::io::Write;
use std::time::{Duration, Instant};

use lsp_server::{Connection, ErrorCode, Message, ProtocolError, Response};
use lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, HoverParams, HoverProviderCapability,
    InitializeParams, OneOf, PositionEncodingKind, SaveOptions, ServerCapabilities, ServerInfo,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions,
};
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

fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(PositionEncodingKind::UTF16),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
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

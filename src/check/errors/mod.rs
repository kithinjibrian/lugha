//! One constructor per checker error code, so the wording lives in one place
//! (spec §9: codes are stable, messages may improve).
//! Split by code range: `names` (E03xx), `types` (E04xx), `places` (E05xx, W01xx).

mod names;
mod places;
mod types;

pub(super) use names::*;
pub(super) use places::*;
pub(super) use types::*;

use crate::diagnostic::{Diagnostic, Label};

fn with_reason(d: Diagnostic, reason: Option<&Label>) -> Diagnostic {
    match reason {
        Some(label) => d.with_label(label.span, label.message.clone()),
        None => d,
    }
}

//! One constructor per checker error code, so the wording lives in one place
//! (codes are stable; messages may improve).
//! Split by code range: `names` (E03xx), `types` (E04xx), `places` (E05xx, W01xx),
//! with `generics` holding the codes for generics from both ranges, and one module per
//! feature for the rest (`interfaces`, `control`, `externs`, …).

mod concurrency;
mod consts;
mod control;
mod derive;
mod externs;
mod generics;
mod interfaces;
mod names;
mod patterns;
mod places;
mod templates;
mod test_fns;
mod types;
mod values;
mod visibility;

pub(super) use concurrency::*;
pub(super) use consts::*;
pub(super) use control::*;
pub(super) use derive::*;
pub(super) use externs::*;
pub(super) use generics::*;
pub(super) use interfaces::*;
pub(super) use names::*;
pub(super) use patterns::*;
pub(super) use places::*;
pub(super) use templates::*;
pub(super) use test_fns::*;
pub(super) use types::*;
pub(super) use values::*;
pub(super) use visibility::*;

use crate::diagnostic::{Diagnostic, Label};

fn with_reason(d: Diagnostic, reason: Option<&Label>) -> Diagnostic {
    match reason {
        Some(label) => d.with_label(label.span, label.message.clone()),
        None => d,
    }
}

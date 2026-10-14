//! One constructor per checker error code, so the wording lives in one place
//! (spec §9: codes are stable, messages may improve).
//! Split by code range: `names` (E03xx), `types` (E04xx), `places` (E05xx, W01xx),
//! with `generics` holding milestone 11's codes from both ranges.

mod generics;
mod names;
mod places;
mod types;
mod v10;
mod v4;
mod v5;
mod v6;
mod v8;
mod v9;

pub(super) use generics::*;
pub(super) use names::*;
pub(super) use places::*;
pub(super) use types::*;
pub(super) use v4::*;
pub(super) use v5::*;
pub(super) use v6::*;
pub(super) use v8::*;
pub(super) use v9::*;
pub(super) use v10::*;

use crate::diagnostic::{Diagnostic, Label};

fn with_reason(d: Diagnostic, reason: Option<&Label>) -> Diagnostic {
    match reason {
        Some(label) => d.with_label(label.span, label.message.clone()),
        None => d,
    }
}

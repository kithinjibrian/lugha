//! The platforms lughac builds for (v17, milestone 133, PRP-165), and what
//! a program may not do on one: lughac knows C's rules for passing structs
//! by value only on x86-64, so on ARM each place an extern struct crosses
//! to C by value is E0449 rather than code C would misread.

use crate::diagnostic::Diagnostic;
use crate::span::Span;

/// The platform the executable is for (v17, milestone 133). Both are LP64
/// Linux with the same scalar sizes (`split_leaves::byte_size`); they differ
/// in instructions, in the runtime's stack switch and in how C passes
/// structs, which lughac implements only for x86-64 (E0449 on ARM).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Target {
    /// x86-64 Linux, `x86_64-unknown-linux-gnu`: the default.
    #[default]
    X86_64Linux,
    /// ARM64 Linux, `aarch64-unknown-linux-gnu`.
    Aarch64Linux,
}

impl Target {
    /// The target's LLVM triple.
    pub fn triple(self) -> &'static str {
        match self {
            Target::X86_64Linux => "x86_64-unknown-linux-gnu",
            Target::Aarch64Linux => "aarch64-unknown-linux-gnu",
        }
    }
}

/// E0449 at each of `spans` (an extern struct crossing to or from C by
/// value, `Checked::c_by_value`) when `target` lacks C's struct rules;
/// nothing on x86-64. Each place is reported once, in source order.
pub fn by_value_errors(target: Target, spans: &[Span]) -> Vec<Diagnostic> {
    if target == Target::X86_64Linux {
        return Vec::new();
    }
    let mut spans = spans.to_vec();
    spans.sort_by_key(|s| (s.start, s.end));
    spans.dedup();
    spans
        .into_iter()
        .map(|span| {
            Diagnostic::error(
                "E0449",
                "an extern struct can't cross to C by value on aarch64 yet",
                span,
            )
            .with_help(
                "pass it by pointer (`ptr p: P`, `mut p: P` or `out p: P`), or build for x86-64",
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{Target, by_value_errors};
    use crate::span::Span;

    #[test]
    fn only_arm_reports_each_place_once_in_order() {
        let spans = [Span::new(9, 12), Span::new(1, 4), Span::new(9, 12)];
        assert!(by_value_errors(Target::X86_64Linux, &spans).is_empty());
        let errors = by_value_errors(Target::Aarch64Linux, &spans);
        assert_eq!(errors.len(), 2);
        assert_eq!(errors[0].code, "E0449");
        assert_eq!(errors[0].span, Span::new(1, 4));
    }
}

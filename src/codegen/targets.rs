//! The platforms lughac builds for (v17, milestone 133, PRP-165). Extern
//! structs cross to C by each one's rules (`c_abi`, `c_abi_arm`, milestone
//! 134); E0449, which refused them on ARM before that, is retired.

/// The platform the executable is for (v17, milestone 133). Both are LP64
/// Linux with the same scalar sizes (`split_leaves::byte_size`); they differ
/// in instructions, in the runtime's stack switch and in how C passes structs.
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

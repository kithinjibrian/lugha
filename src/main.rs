//! lughac — command-line entry point. All logic lives in `lugha::driver`.

fn main() -> std::process::ExitCode {
    lugha::driver::main()
}

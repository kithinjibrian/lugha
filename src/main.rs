mod lexer;

use lexer::{tokenize, Token};

fn main() {
    let code = r#"
        let x = 3 + 4;
        return x;
    "#;

    match tokenize(code) {
        Ok(tokens) => {
            for token in tokens {
                println!("{:?}", token);
            }
        }
        Err(e) => {
            eprintln!("Lexer error: {}", e);
        }
    }
}

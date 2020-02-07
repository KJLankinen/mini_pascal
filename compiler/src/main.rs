use compiler::read_program_to_string;
use compiler::Parser;
use compiler::Scanner;
use std::env;
use std::process;

fn run() {
    let source_str = match read_program_to_string(env::args().collect()) {
        Ok(source_str) => source_str,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

    let mut parser = Parser {
        scanner: Scanner {
            column: 0,
            line: 1,
            chars: source_str.char_indices().peekable(),
            source_str: &source_str,
            current_token: None,
            next_token: None,
            keywords: [
                ("var", true),
                ("for", true),
                ("end", true),
                ("in", true),
                ("do", true),
                ("read", true),
                ("print", true),
                ("int", true),
                ("string", true),
                ("bool", true),
                ("assert", true),
            ]
            .iter()
            .cloned()
            .collect(),
        },
    };

    parser.parse();
}

fn main() {
    run();
}

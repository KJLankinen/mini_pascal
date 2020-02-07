use compiler::read_program_to_string;
use compiler::Scanner;
use compiler::TokenData;
use compiler::TokenType;
use std::env;
use std::process;

fn parse<'a>(scanner: &'a mut Scanner<'a>) {
    loop {
        scanner.step();
        println!("{:?}", scanner.current_token.expect("Panic 1."));
        println!("{:?}", scanner.next_token.expect("Panic 2."));

        if TokenType::EndOfProgram == scanner.current_token.expect("Panic 3.").token_type {
            break;
        }
    }
}

fn run() {
    let source_str = match read_program_to_string(env::args().collect()) {
        Ok(source_str) => source_str,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

    let mut scanner = Scanner {
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
    };

    parse(&mut scanner);
}

fn main() {
    run();
}

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
        scanner: Scanner::new(&source_str),
    };

    parser.parse();
}

fn main() {
    run();
}

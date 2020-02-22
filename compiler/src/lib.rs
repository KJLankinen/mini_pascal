pub mod parser;
mod scanner;

use parser::Parser;
use std::env;
use std::fs;
use std::process;

// Read the entire source code to a string and return that string
fn read_program_to_string(args: Vec<String>) -> Result<String, &'static str> {
    if 2 > args.len() {
        return Err("Provide the name of the file to compile.");
    }

    let filename: &str = &args[1];
    let contents = match fs::read_to_string(&filename) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Error with file {}: {}", filename, err);
            return Err("Problem with reading file");
        }
    };
    Ok(contents)
}

pub fn run() {
    let source_str = match read_program_to_string(env::args().collect()) {
        Ok(source_str) => source_str,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

    let mut parser = Parser::new(&source_str);
    parser.parse();
}

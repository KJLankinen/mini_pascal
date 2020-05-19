mod data_types;
mod lcrs_tree;
mod logger;
mod parser;
mod scanner;
mod semantic_analyzer;
mod stacker;
mod symbol_table;
mod wasmer;

use lcrs_tree::LcRsTree;
use logger::Logger;
use parser::Parser;
use semantic_analyzer::Analyzer;
use stacker::Stacker;
use std::{env, fs, process};
use symbol_table::SymbolTable;
use wasmer::Wasmer;

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let filetype = ".mpc";

    match args.len() {
        0..=1 => {
            eprintln!(
                "Usage:\n\"cargo run filename (--debug)\", where\t
            filename = the program to compile (it must end with {})\t
            --debug = optionally output the constructed AST to a .json file
            with the same name as the input file.
            E.g. \"cargo run hello{} --debug\" writes the AST to 'hello.json'.",
                filetype, filetype
            );
            process::exit(1);
        }
        2..=3 => {
            let filename = &args[1];
            if filename.ends_with(filetype) {
                match fs::read_to_string(&filename) {
                    Ok(source_str) => {
                        let filename_prefix = filename.trim_end_matches(filetype).to_owned();
                        let out_file = match args.len() {
                            3 => {
                                if "--debug" == args[2] {
                                    // Change "filename.mpc" to "filename.json"
                                    Some(filename_prefix.to_owned() + ".json")
                                } else {
                                    eprintln!(
                                    "Unidentified extra parameter. Only \"--debug\" is supported. Given parameter is \"{}\"",
                                    args[2]
                                    );
                                    process::exit(1);
                                }
                            }
                            _ => None,
                        };
                        let mut logger = Logger::new(&source_str, filename);
                        let mut tree = LcRsTree::new();
                        {
                            Parser::new(&source_str, &mut logger, &mut tree)
                                .parse(out_file.as_ref().map(|s| &**s));
                        }

                        let mut symbol_table = SymbolTable::new();
                        if logger.errors_encountered() {
                            logger.print_errors();
                            process::exit(1);
                        } else {
                            Analyzer::new(&mut tree, &mut logger, &mut symbol_table).analyze();
                        }

                        let mut instructions = vec![];
                        if logger.errors_encountered() {
                            logger.print_errors();
                            process::exit(1);
                        } else {
                            Stacker::new(&tree, &mut symbol_table, &mut instructions).stack_ir();
                        }

                        let mut wasm_string = String::new();
                        if logger.errors_encountered() {
                            logger.print_errors();
                            process::exit(1);
                        } else {
                            Wasmer::new(&instructions, &mut wasm_string).instructions_to_wasm();
                        }

                        fs::write(filename_prefix + ".wast", wasm_string)
                            .expect("Unable to write to a file.");

                        println!("");
                    }
                    Err(err) => {
                        eprintln!("Application error: {}, with filename \"{}\"", err, filename);
                        process::exit(1);
                    }
                }
            } else {
                eprintln!(
                    "The interpreted program should end with \"{}\". Given file is {}",
                    filetype, filename
                );
                process::exit(1);
            }
        }
        _ => {}
    }
}

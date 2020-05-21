mod data_types;
mod lcrs_tree;
mod linker;
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
        0..=2 => {
            eprintln!(
                "Usage:\n\"cargo run filename lib_filename (--debug)\", where\t
            filename = the program to compile (it must end with {})\t
            lib_filename = the library file (e.g. wasm/lib.wast)\t
            --debug = optionally output the constructed AST to a .json file
            with the same name as the input file.
            E.g. \"cargo run hello{} ../wasm/lib.wast --debug\" writes the AST to 'hello.json'.",
                filetype, filetype
            );
            process::exit(1);
        }
        3..=4 => {
            let filename = &args[1];
            let lib_filename = &args[2];
            if filename.ends_with(filetype) {
                match fs::read_to_string(&filename) {
                    Ok(source_str) => {
                        match fs::read_to_string(&lib_filename) {
                            Ok(lib_contents) => {
                                let filename_prefix =
                                    filename.trim_end_matches(filetype).to_owned();
                                let out_file = filename_prefix.to_owned() + ".wast";
                                let ast_file = match args.len() {
                                    4 => {
                                        if "--debug" == args[3] {
                                            // Change "filename.mpc" to "filename.json"
                                            Some(filename_prefix.to_owned() + ".json")
                                        } else {
                                            eprintln!(
                                    "Unidentified extra parameter. Only \"--debug\" is supported. Given parameter is \"{}\"",
                                    args[3]
                                    );
                                            process::exit(1);
                                        }
                                    }
                                    _ => None,
                                };

                                let lib_name = if let Some(idx) = lib_filename.rfind('/') {
                                    &lib_filename[idx + 1..]
                                } else {
                                    &lib_filename
                                };

                                let lib_name = if let Some(idx) = lib_name.rfind('.') {
                                    &lib_name[..idx]
                                } else {
                                    &lib_name
                                };

                                analyze(
                                    &source_str,
                                    &filename,
                                    &out_file,
                                    ast_file.as_ref().map(|s| &**s),
                                    &lib_contents,
                                    lib_name,
                                );
                            }
                            Err(err) => {
                                eprintln!(
                                    "Application error: {}, with lib file \"{}\"",
                                    err, lib_filename
                                );
                                process::exit(1);
                            }
                        }
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

fn analyze(
    source_str: &str,
    filename: &str,
    out_file: &str,
    ast_file: Option<&str>,
    lib_contents: &str,
    lib_name: &str,
) {
    let mut logger = Logger::new(&source_str, filename);
    let mut tree = LcRsTree::new();
    {
        Parser::new(&source_str, &mut logger, &mut tree).parse(ast_file);
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
        Wasmer::new(
            &instructions,
            &mut wasm_string,
            &symbol_table,
            lib_contents,
            lib_name,
        )
        .instructions_to_wasm();
    }

    match fs::write(out_file, wasm_string) {
        Ok(_) => {}
        Err(err) => {
            eprintln!("Error when writing compiled program to file: {}", err);
            process::exit(1);
        }
    }

    print!("\n");
}

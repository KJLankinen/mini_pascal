// Specify here the implemented and used crates
mod data_types;
mod lcrs_tree;
mod logger;
mod parser;
mod scanner;
mod semantic_analyzer;

use lcrs_tree::LcRsTree;
use logger::Logger;
use parser::Parser;
use semantic_analyzer::Analyzer;
use std::{env, fs, process};

// Called from main to kick off the interpretation
pub fn run() {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        0..=1 => {
            eprintln!(
                "Usage:\n\"cargo run filename (--debug)\", where\t
            filename = the program to interpret (it must end with .mpl)\t
            --debug = optionally output the constructed AST to a .json file
            with the same name as the input file.
            E.g. \"cargo run hello.mpl --debug\" writes the AST to 'hello.json'."
            );
            process::exit(1);
        }
        2..=3 => {
            let filename = &args[1];
            if filename.ends_with(".mpl") {
                match fs::read_to_string(&filename) {
                    Ok(source_str) => {
                        // Reading the source string from the given .mpl file was successfull
                        // Next see if cmd line arguments were passed and if the AST should be
                        // serialized to a json file
                        let out_file = match args.len() {
                            3 => {
                                if "--debug" == args[2] {
                                    // Change "filename.mpl" to "filename.json"
                                    Some(filename.trim_end_matches(".mpl").to_owned() + ".json")
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

                        let mut logger = Logger::new(&source_str);
                        let mut tree = LcRsTree::new();
                        {
                            Parser::new(&source_str, &mut logger, &mut tree)
                                .parse(out_file.as_ref().map(|s| &**s));
                        }

                        if logger.errors_encountered() {
                            logger.print_errors();
                            process::exit(1);
                        } else {
                            Analyzer::new(&tree, &mut logger).analyze();
                        }

                        if logger.errors_encountered() {
                            logger.print_errors();
                            process::exit(1);
                        } else {
                            // Interpret
                        }
                    }
                    Err(err) => {
                        eprintln!("Application error: {}, with filename \"{}\"", err, filename);
                        process::exit(1);
                    }
                }
            } else {
                eprintln!(
                    "The interpreted program should end with \".mpl\". Given file is {}",
                    filename
                );
                process::exit(1);
            }
        }
        _ => {}
    }
}

mod data_types;
//mod interpreter;
//mod lcrs_tree;
mod logger;
//mod parser;
mod scanner;
//mod semantic_analyzer;

//use interpreter::Interpreter;
//use lcrs_tree::LcRsTree;
use data_types::{ErrorType, TokenData, TokenType};
use logger::Logger;
use scanner::Scanner;
//use parser::Parser;
//use semantic_analyzer::Analyzer;
use std::{env, fs, process};

pub fn run() {
    let args: Vec<String> = env::args().collect();
    let filetype = ".mpc";

    match args.len() {
        0..=1 => {
            eprintln!(
                "Usage:\n\"cargo run filename (--debug)\", where\t
            filename = the program to interpret (it must end with {})\t
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
                        let out_file = match args.len() {
                            3 => {
                                if "--debug" == args[2] {
                                    // Change "filename.mpc" to "filename.json"
                                    Some(filename.trim_end_matches(filetype).to_owned() + ".json")
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
                        let mut scanner = Scanner::new(&source_str);

                        loop {
                            let token = scanner.next();
                            if TokenType::Undefined == token.token_type {
                                logger.add_error(ErrorType::SyntaxError(token, [].to_vec()));
                            } else if TokenType::EndOfProgram == token.token_type {
                                break;
                            } else {
                                println!("{:?}", token);
                            }
                        }

                        // If there were any unmatched multiline comment starting tokens, add those as errors.
                        for (line, col) in &scanner.unmatched_multiline_comment_prefixes {
                            logger.add_error(ErrorType::UnmatchedComment(*line, *col));
                        }
                        //let mut tree = LcRsTree::new();
                        //{
                        //    Parser::new(&source_str, &mut logger, &mut tree)
                        //        .parse(out_file.as_ref().map(|s| &**s));
                        //}

                        //if logger.errors_encountered() {
                        //    logger.print_errors();
                        //    process::exit(1);
                        //} else {
                        //    Analyzer::new(&mut tree, &mut logger).analyze();
                        //}

                        //if logger.errors_encountered() {
                        //    logger.print_errors();
                        //    process::exit(1);
                        //} else {
                        //    Interpreter::new(&tree, &mut logger).interpret();
                        //}

                        if logger.errors_encountered() {
                            logger.print_errors();
                        }

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

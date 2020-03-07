// Specify here the implemented and used crates
mod data_types;
mod lcrs_tree;
mod parser;
mod scanner;
mod semantic_analyzer;

use parser::Parser;
use std::{env, fs, io::BufWriter, process};

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
                        let mut parser = Parser::new(&source_str);
                        let success = match parser.parse() {
                            Ok(o) => o,
                            Err(_) => false,
                        };

                        // Parsing is completed, serialize the AST if so specified
                        if let Some(filename) = out_file {
                            if let Some(json) = parser.serialize() {
                                let file = fs::File::create(&filename).expect(
                                    format!(
                                        "Could not create a new file with the name {}",
                                        &filename
                                    )
                                    .as_str(),
                                );

                                println!("Writing the AST to the file \"{}\"", &filename);

                                let mut writer = BufWriter::new(&file);
                                match serde_json::to_writer_pretty(&mut writer, &json) {
                                    Ok(_) => {}
                                    Err(err) => {
                                        eprintln!("Error writing to file: {}", err);
                                        process::exit(1);
                                    }
                                }
                            }
                        }

                        // Place holder print
                        if success {
                            let mut analyzer = semantic_analyzer::Analyzer::new(parser.tree);
                            let success = analyzer.analyze();
                            if success {
                                println!("Semantic analysis done.");
                            }
                        } else {
                            println!("Syntax errors encountered during parse.");
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

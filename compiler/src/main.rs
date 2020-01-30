use std::collections::HashMap;
use std::env;
use std::process;

mod scanner;

fn run() {
    // The value is essentially useless, because the map will never have any false entries.
    let keywords: HashMap<&str, bool> = [
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
    .collect();

    let mut scanner_state: scanner::ScannerState = Default::default();
    scanner_state.contents = match scanner::read_program_to_string(env::args().collect()) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

    if let Err(err) = scanner::get_next_token(&mut scanner_state) {
        eprintln!("Error getting next token: {}", err);
        process::exit(1);
    }

    println!("Received token {:#?}", scanner_state.latest_token);

    //for (index, matched) in contents.match_indices("/*")

    //for word in contents.split_whitespace() {
    //    if let Some(_) = keywords.get(word) {
    //        println!("{}", word);
    //    } else {
    //        println!("{} is not a keyword", word);
    //    }
    //}

    //let mut result = Vec::new();
    //for word in contents.split_whitespace() {
    //    let mut last = 0;
    //    for (index, matched) in word.match_indices(|c: char| !c.is_alphanumeric()) {
    //        if last != index {
    //            result.push(&word[last..index]);
    //        }
    //        result.push(matched);
    //        last = index + matched.len();
    //    }
    //    if last < word.len() {
    //        result.push(&word[last..]);
    //    }
    //}
    //println!("{:?}", result);
}

fn main() {
    run();
}

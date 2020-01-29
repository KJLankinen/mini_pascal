use std::collections::HashMap;
use std::env;
use std::fs;
use std::process;
use std::string::String;

fn read_contents(args: Vec<String>) -> Result<String, &'static str> {
    if args.len() < 2 {
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

    let contents = match read_contents(env::args().collect()) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

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

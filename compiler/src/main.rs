use std::env;
use std::fs;
use std::process;

fn run(args: Vec<std::string::String>) -> Result<(), &'static str> {
    if args.len() < 2 {
        return Err("Provide the name of the file to compile.");
    }

    let filename: &str = &args[1];
    let contents = match fs::read_to_string(&filename) {
        Ok(string) => string,
        Err(err) => {
            eprintln!("Error with file {}: {}", filename, err);
            return Err("Problem with reading file");
        }
    };

    println!("Contents of file {}:", filename);
    for word in contents.split_whitespace() {
        println!("{}", word);
    }
    Ok(())
}

fn main() {
    if let Err(e) = run(env::args().collect()) {
        eprintln!("Application error: {}", e);
        process::exit(1);
    }
}

pub mod parser;
mod scanner;

use parser::Parser;
use serde::Serialize;
use serde_json::json;
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

#[derive(Serialize)]
struct JsonNode<'a> {
    token: scanner::TokenData<'a>,
    node_type: parser::NodeType,
    children: Option<serde_json::Value>,
}

// This function turns the vector representation of the tree into a json via a recursive call
// structure s.t. first all children of the current node are gathered to a vector, which is
// serialized by the first child. After that, all siblings are gathered to a vector, which is again
// serialized by the first child. The root of the tree then serializes itself and return a json
// value.
fn recursive_serialize<'a>(
    my_id: usize,
    tree: &'a Vec<parser::Node<'a>>,
    siblings: &mut Vec<JsonNode<'a>>,
) -> Option<serde_json::Value> {
    if tree.len() > my_id {
        // Pass this vector to the children. I will never use this, because my first child will
        // serialize the vector and return a json to me.
        let mut my_children = vec![];

        // First add myself and my children
        siblings.push(JsonNode {
            token: tree[my_id].token,
            node_type: tree[my_id].node_type,
            children: recursive_serialize(tree[my_id].left_child, tree, &mut my_children),
        });

        // Then call my first sibling who does what I did above.
        recursive_serialize(tree[my_id].right_sibling, tree, siblings);

        // If I am the first child of my parent, I will serialize the vector given to me by my
        // parent that now contains me and all my siblings with all our children
        if tree.len() > tree[my_id].parent && tree[tree[my_id].parent].left_child == my_id {
            return Some(json!(&siblings));
        } else if 0 == my_id {
            // I am (G)root
            return Some(json!(&siblings[0]));
        }
    }
    // I am nulbody
    None
}

// Serialize the tree to a json via a recursive function.
pub fn write_tree_to_json<'a>(tree: &'a Vec<parser::Node<'a>>) {
    let mut siblings = vec![];
    if let Some(children) = recursive_serialize(0, tree, &mut siblings) {
        println!("{}", children.to_string());
    }
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

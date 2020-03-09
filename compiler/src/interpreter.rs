use super::data_types::{NodeData, NodeType, SymbolType, TokenType};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::HashMap;
use std::io;
use std::process;

pub struct Interpreter<'a, 'b> {
    logger: &'b mut Logger<'a>,
    tree: &'b LcRsTree<NodeData<'a>>,
    symbols: HashMap<&'a str, SymbolType>,
    integers: HashMap<&'a str, i32>,
    strings: HashMap<&'a str, String>,
    booleans: HashMap<&'a str, bool>,
}

impl<'a, 'b> Interpreter<'a, 'b> {
    // ---------------------------------------------------------------------
    // Called to start the interpretation
    // ---------------------------------------------------------------------
    pub fn interpret(&mut self) {
        // Start walking down the tree
        let mut node = self.tree[0].left_child;

        while let Some(idx) = node {
            self.interpret_statement(idx);
            node = self.tree[idx].right_sibling;
        }
    }

    // ---------------------------------------------------------------------
    // Functions that interpret the program
    // ---------------------------------------------------------------------
    fn interpret_statement(&mut self, idx: usize) {
        match self.tree[idx]
            .data
            .node_type
            .expect("AST should not contain Nodes with type None.")
        {
            NodeType::Declaration => self.interpret_declaration(idx),
            NodeType::Assignment => self.interpret_assignment(idx),
            NodeType::For => self.interpret_for(idx),
            NodeType::Read => self.interpret_read(idx),
            NodeType::Print => self.interpret_print(idx),
            NodeType::Assert => self.interpret_assert(idx),
            _ => {
                assert!(false, "Unhandled node type.");
                ()
            }
        };
    }

    fn evaluate_boolean(&self, idx: usize) -> bool {
        if let NodeType::Operand(_) = self.tree[idx].data.node_type.unwrap() {
            assert!(
                false,
                "There are no bool literals, so this should never happen."
            );
            false
        } else {
            match self.tree[idx].data.token.unwrap().token_type {
                TokenType::OperatorNot => {
                    !self.evaluate_boolean(self.tree[idx].left_child.unwrap())
                }
                TokenType::OperatorAnd => {
                    self.evaluate_boolean(self.tree[idx].left_child.unwrap())
                        && self.evaluate_boolean(
                            self.tree[self.tree[idx].left_child.unwrap()]
                                .right_sibling
                                .unwrap(),
                        )
                }
                TokenType::OperatorLessThan => {
                    let et = self.tree[self.tree[idx].left_child.unwrap()]
                        .data
                        .node_type
                        .unwrap();
                    match et {
                        NodeType::Operand(t) | NodeType::Expression(t) => match t {
                            SymbolType::Int => {
                                self.evaluate_int(self.tree[idx].left_child.unwrap())
                                    < self.evaluate_int(
                                        self.tree[self.tree[idx].left_child.unwrap()]
                                            .right_sibling
                                            .unwrap(),
                                    )
                            }
                            SymbolType::String => {
                                self.evaluate_string(self.tree[idx].left_child.unwrap())
                                    .len()
                                    < self
                                        .evaluate_string(
                                            self.tree[self.tree[idx].left_child.unwrap()]
                                                .right_sibling
                                                .unwrap(),
                                        )
                                        .len()
                            }
                            _ => {
                                assert!(false, "Illegal expression.");
                                false
                            }
                        },
                        _ => {
                            assert!(false, "Illegal expression.");
                            false
                        }
                    }
                }
                TokenType::OperatorEqual => {
                    let et = self.tree[self.tree[idx].left_child.unwrap()]
                        .data
                        .node_type
                        .unwrap();
                    match et {
                        NodeType::Operand(t) | NodeType::Expression(t) => match t {
                            SymbolType::Int => {
                                self.evaluate_int(self.tree[idx].left_child.unwrap())
                                    == self.evaluate_int(
                                        self.tree[self.tree[idx].left_child.unwrap()]
                                            .right_sibling
                                            .unwrap(),
                                    )
                            }
                            SymbolType::String => {
                                self.evaluate_string(self.tree[idx].left_child.unwrap())
                                    .len()
                                    == self
                                        .evaluate_string(
                                            self.tree[self.tree[idx].left_child.unwrap()]
                                                .right_sibling
                                                .unwrap(),
                                        )
                                        .len()
                            }
                            SymbolType::Bool => {
                                self.evaluate_boolean(self.tree[idx].left_child.unwrap())
                                    == self.evaluate_boolean(
                                        self.tree[self.tree[idx].left_child.unwrap()]
                                            .right_sibling
                                            .unwrap(),
                                    )
                            }
                            _ => {
                                assert!(false, "Illegal expression.");
                                false
                            }
                        },
                        _ => {
                            assert!(false, "Illegal expression.");
                            false
                        }
                    }
                }
                _ => {
                    assert!(false, "Illegal token type at boolean expression.");
                    false
                }
            }
        }
    }

    fn evaluate_int(&self, idx: usize) -> i32 {
        if let NodeType::Operand(_) = self.tree[idx].data.node_type.unwrap() {
            match self.tree[idx].data.token.unwrap().token_type {
                TokenType::LiteralInt => {
                    match self.tree[idx].data.token.unwrap().value.parse::<i32>() {
                        Ok(v) => v,
                        Err(e) => {
                            eprintln!(
                                "\"{}\" is not a valid integer. Error in parse: {}",
                                self.tree[idx].data.token.unwrap().value,
                                e
                            );
                            process::exit(1);
                        }
                    }
                }
                TokenType::Identifier => *self
                    .integers
                    .get(self.tree[idx].data.token.unwrap().value)
                    .unwrap(),
                _ => {
                    assert!(false, "Illegal token type for int operand.");
                    0
                }
            }
        } else {
            let ev1 = self.evaluate_int(self.tree[idx].left_child.unwrap());
            let ev2 = self.evaluate_int(
                self.tree[self.tree[idx].left_child.unwrap()]
                    .right_sibling
                    .unwrap(),
            );

            match self.tree[idx].data.token.unwrap().token_type {
                TokenType::OperatorPlus => ev1 + ev2,
                TokenType::OperatorMinus => ev1 - ev2,
                TokenType::OperatorMultiply => ev1 * ev2,
                TokenType::OperatorDivide => ev1 / ev2,
                _ => {
                    assert!(false, "Illegal token type at int  expression.");
                    0
                }
            }
        }
    }

    fn evaluate_string(&self, idx: usize) -> String {
        if let NodeType::Operand(_) = self.tree[idx].data.node_type.unwrap() {
            match self.tree[idx].data.token.unwrap().token_type {
                TokenType::LiteralString => self.tree[idx].data.token.unwrap().value.to_owned(),
                TokenType::Identifier => self
                    .strings
                    .get(self.tree[idx].data.token.unwrap().value)
                    .unwrap()
                    .to_owned(),
                _ => {
                    assert!(false, "Illegal token type for string operand.");
                    "".to_owned()
                }
            }
        } else {
            if TokenType::OperatorPlus == self.tree[idx].data.token.unwrap().token_type {
                self.evaluate_string(self.tree[idx].left_child.unwrap())
                    + &self.evaluate_string(
                        self.tree[self.tree[idx].left_child.unwrap()]
                            .right_sibling
                            .unwrap(),
                    )
            } else {
                assert!(false, "Illegal operation for strings.");
                "".to_owned()
            }
        }
    }

    fn interpret_declaration(&mut self, idx: usize) {
        let identifier_type;
        if let TokenType::Type(t) = self.tree[idx].data.token.unwrap().token_type {
            identifier_type = t;
        } else {
            identifier_type = SymbolType::Undefined;
        }

        let lc = self.tree[idx].left_child.unwrap();
        let rs = self.tree[lc].right_sibling.unwrap();
        let identifier_name = self.tree[lc].data.token.unwrap().value;

        match identifier_type {
            SymbolType::Int => {
                let assigned_value = match self.tree[rs].data.token {
                    Some(_) => self.evaluate_int(rs),
                    None => 0,
                };
                self.integers.insert(identifier_name, assigned_value);
            }
            SymbolType::String => {
                let assigned_value = match self.tree[rs].data.token {
                    Some(_) => self.evaluate_string(rs),
                    None => "".to_owned(),
                };
                self.strings.insert(identifier_name, assigned_value);
            }
            SymbolType::Bool => {
                let assigned_value = match self.tree[rs].data.token {
                    Some(_) => self.evaluate_boolean(rs),
                    None => true,
                };
                self.booleans.insert(identifier_name, assigned_value);
            }
            _ => {
                assert!(false, "Illegal type in declaration.");
            }
        }

        self.symbols.insert(identifier_name, identifier_type);
    }

    fn interpret_assignment(&mut self, idx: usize) {
        let value = self.tree[self.tree[idx].left_child.unwrap()]
            .data
            .token
            .unwrap()
            .value;
        let idx = self.tree[self.tree[idx].left_child.unwrap()]
            .right_sibling
            .unwrap();
        match self.symbols.get(value).unwrap() {
            SymbolType::Int => {
                self.integers.insert(value, self.evaluate_int(idx));
            }
            SymbolType::String => {
                self.strings.insert(value, self.evaluate_string(idx));
            }
            _ => {
                assert!(false, "Literal booleans are not supported.");
            }
        }
    }

    fn interpret_for(&mut self, idx: usize) {}

    fn interpret_read(&mut self, idx: usize) {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let input = match input.split(char::is_whitespace).next() {
                    Some(v) => v,
                    None => {
                        eprintln!("Empty input is not accepted");
                        process::exit(1);
                    }
                };
                let value = self.tree[idx].data.token.unwrap().value;

                match self.symbols.get(value).unwrap() {
                    SymbolType::Int => match input.parse::<i32>() {
                        Ok(v) => {
                            self.integers.insert(value, v);
                        }
                        Err(e) => {
                            eprintln!(
                                "\"{}\" is not a valid integer. Error in parse: {}",
                                input, e
                            );
                            process::exit(1);
                        }
                    },
                    SymbolType::String => {
                        self.strings.insert(value, input.to_owned());
                    }
                    _ => {
                        assert!(false, "Can't read values to booleans.");
                    }
                }
            }
            Err(e) => {
                eprintln!("Bad input: {}", e);
                process::exit(1);
            }
        }
    }

    fn interpret_print(&mut self, idx: usize) {
        match self.tree[self.tree[idx].left_child.unwrap()]
            .data
            .node_type
            .unwrap()
        {
            NodeType::Expression(t) | NodeType::Operand(t) => {
                let idx = self.tree[idx].left_child.unwrap();
                match t {
                    SymbolType::Int => println!("{}", self.evaluate_int(idx)),
                    SymbolType::String => println!("{}", self.evaluate_string(idx)),
                    _ => assert!(false, "Illegal symbol type for print."),
                }
            }
            _ => {
                println!("{:#?}", self.tree[idx].data.node_type);
                assert!(false, "Illegal expression type at print.");
            }
        }
    }

    fn interpret_assert(&mut self, idx: usize) {
        if false == self.evaluate_boolean(self.tree[idx].left_child.unwrap()) {
            eprintln!(
                "Assertion failed: {}",
                self.logger
                    .get_line(self.tree[idx].data.token.unwrap().line as usize)
            );
            process::exit(1);
        }
    }

    pub fn new(tree: &'b LcRsTree<NodeData<'a>>, logger: &'b mut Logger<'a>) -> Self {
        Interpreter {
            logger: logger,
            tree: tree,
            integers: HashMap::new(),
            strings: HashMap::new(),
            booleans: HashMap::new(),
            symbols: HashMap::new(),
        }
    }
}

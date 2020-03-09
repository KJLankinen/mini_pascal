use super::data_types::{ErrorType, NodeData, NodeType, SymbolType, TokenType};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::HashMap;
use std::process;

pub struct Interpreter<'a, 'b> {
    logger: &'b mut Logger<'a>,
    tree: &'b LcRsTree<NodeData<'a>>,
    integers: HashMap<&'a str, i32>,
    strings: HashMap<&'a str, &'a str>,
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

    fn evaluate_boolean(&mut self, idx: usize) -> bool {
        if let NodeType::Operand(ot) = self.tree[idx].data.node_type.unwrap() {
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

    fn evaluate_int(&mut self, idx: usize) -> i32 {
        if let NodeType::Operand(_) = self.tree[idx].data.node_type.unwrap() {
            match self.tree[idx].data.token.unwrap().token_type {
                TokenType::LiteralInt => {
                    match self.tree[idx].data.token.unwrap().value.parse::<i32>() {
                        Ok(v) => v,
                        Err(e) => {
                            self.logger.add_error(ErrorType::IntParseError(
                                e,
                                self.tree[idx].data.token.unwrap(),
                            ));
                            process::exit(1);
                            0
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

    fn evaluate_string(&mut self, idx: usize) -> String {
        if let NodeType::Operand(_) = self.tree[idx].data.node_type.unwrap() {
            match self.tree[idx].data.token.unwrap().token_type {
                TokenType::LiteralString => self.tree[idx].data.token.unwrap().value.to_owned(),
                TokenType::Identifier => self.strings.get(self.tree[idx].data.token.unwrap().value).unwrap().to_owned()
                _ => {
                    assert!(false, "Illegal token type for string operand.");
                    "".to_owned()
                }
            }
        } else {
            if TokenType::OperatorPlus == self.tree[idx].data.token.unwrap().token_type {
                self.evaluate_string(self.tree[idx].left_child.unwrap())
                    + self.evaluate_string(
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

    fn interpret_declaration(&mut self, idx: usize) {}

    fn interpret_assignment(&mut self, idx: usize) {}

    fn interpret_for(&mut self, idx: usize) {}

    fn interpret_read(&mut self, idx: usize) {}

    fn interpret_print(&mut self, idx: usize) {}

    fn interpret_assert(&mut self, idx: usize) {}

    pub fn new(tree: &'b LcRsTree<NodeData<'a>>, logger: &'b mut Logger<'a>) -> Self {
        Interpreter {
            logger: logger,
            tree: tree,
            integers: HashMap::new(),
            strings: HashMap::new(),
            booleans: HashMap::new(),
        }
    }
}

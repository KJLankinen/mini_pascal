use super::data_types::{NodeType, SymbolType, TokenData, TokenType};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io;
use std::process;

pub struct Interpreter<'a, 'b> {
    logger: &'b mut Logger<'a>,
    tree: &'b LcRsTree<NodeType<'a>>,
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
            self.handle_statement(idx);
            node = self.tree[idx].right_sibling;
        }
    }

    // ---------------------------------------------------------------------
    // Functions that interpret the program
    // ---------------------------------------------------------------------
    fn handle_statement(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::Declaration {
                identifier,
                symbol_type,
                expression,
            } => {
                let ref identifier =
                    identifier.expect("Declaration is missing an identifier token.");
                self.handle_declaration(identifier, symbol_type, expression);
            }
            NodeType::Assignment {
                identifier,
                expression,
            } => {
                let ref identifier =
                    identifier.expect("Assignment is missing an identifier token.");
                self.handle_assignment(identifier, expression);
            }
            NodeType::For {
                identifier,
                start_expression,
                end_expression,
                first_statement,
            } => {
                let ref identifier = identifier.expect("For loop is missing an identifier token.");
                self.handle_for(
                    identifier,
                    start_expression,
                    end_expression,
                    first_statement,
                );
            }
            NodeType::Read { identifier } => {
                let ref identifier = identifier.expect("Read is missing an identifier token.");
                self.handle_read(identifier);
            }
            NodeType::Print { token, expression } => {
                let ref token = token.expect("Print is missing a token.");
                self.handle_print(token, expression);
            }
            NodeType::Assert { token, expression } => {
                let ref token = token.expect("Assert is missing a token.");
                self.handle_assert(token, expression);
            }
            _ => {
                assert!(false, "Unhandled node type.");
                ()
            }
        };
    }

    fn evaluate_boolean(&self, idx: usize) -> bool {
        match self.tree[idx].data {
            NodeType::Operand {
                token,
                symbol_type: _,
            } => {
                let token = token.unwrap();
                match token.token_type {
                    TokenType::Identifier => *self.booleans.get(token.value).unwrap(),
                    _ => {
                        assert!(false, "Should never happen. {:#?}", token);
                        false
                    }
                }
            }
            NodeType::Expression {
                operator,
                symbol_type: _,
            } => {
                let operator = operator.unwrap();
                let lc = self.tree[idx].left_child.unwrap();
                let rs = self.tree[lc].right_sibling;

                match operator.token_type {
                    TokenType::OperatorAnd => {
                        self.evaluate_boolean(lc) && self.evaluate_boolean(rs.unwrap())
                    }
                    TokenType::OperatorNot => !self.evaluate_boolean(lc),
                    tt @ TokenType::OperatorLessThan | tt @ TokenType::OperatorEqual => {
                        let rs = rs.unwrap();
                        let value1;
                        let value2;

                        match self.tree[lc].data {
                            NodeType::Operand {
                                token: _,
                                symbol_type,
                            }
                            | NodeType::Expression {
                                operator: _,
                                symbol_type,
                            } => match symbol_type {
                                SymbolType::Int => {
                                    value1 = self.evaluate_int(lc);
                                    value2 = self.evaluate_int(rs);
                                }
                                SymbolType::String => {
                                    value1 = self.evaluate_string(lc).len().try_into().unwrap();
                                    value2 = self.evaluate_string(rs).len().try_into().unwrap();
                                }
                                SymbolType::Bool => {
                                    value1 = if self.evaluate_boolean(lc) { 1 } else { 0 };
                                    value2 = if self.evaluate_boolean(rs) { 1 } else { 0 };
                                }
                                SymbolType::Undefined => {
                                    assert!(
                                        false,
                                        "Illegal symbol type for operator at interpretation."
                                    );

                                    value1 = 0;
                                    value2 = 0;
                                }
                            },
                            _ => {
                                assert!(
                                    false,
                                    "Illegal node type for expression at interpretation."
                                );

                                value1 = 0;
                                value2 = 0;
                            }
                        }

                        if TokenType::OperatorLessThan == tt {
                            value1 < value2
                        } else {
                            value1 == value2
                        }
                    }
                    _ => {
                        assert!(false, "Illegal token type for operator at interpretation.");
                        false
                    }
                }
            }
            _ => {
                assert!(false, "Illegal node type for expression at interpretation.");
                false
            }
        }
    }

    fn evaluate_int(&self, idx: usize) -> i32 {
        match self.tree[idx].data {
            NodeType::Operand {
                token,
                symbol_type: _,
            } => {
                let token = token.unwrap();
                match token.token_type {
                    TokenType::LiteralInt => {
                        match token.value.parse::<i32>() {
                            Ok(v) => v,
                            Err(e) => {
                                eprintln!("Could not parse integer literal to integer value: {}. Exiting.", e);
                                process::exit(1);
                            }
                        }
                    }
                    TokenType::Identifier => *self.integers.get(token.value).unwrap(),
                    _ => {
                        assert!(false, "Should never happen. {:#?}", token);
                        0
                    }
                }
            }
            NodeType::Expression {
                operator,
                symbol_type: _,
            } => {
                let operator = operator.unwrap();
                let lc = self.tree[idx].left_child.unwrap();
                let rs = self.tree[lc].right_sibling.unwrap();

                match operator.token_type {
                    TokenType::OperatorPlus => self.evaluate_int(lc) + self.evaluate_int(rs),
                    TokenType::OperatorMinus => self.evaluate_int(lc) - self.evaluate_int(rs),
                    TokenType::OperatorMultiply => self.evaluate_int(lc) * self.evaluate_int(rs),
                    TokenType::OperatorDivide => self.evaluate_int(lc) / self.evaluate_int(rs),
                    _ => {
                        assert!(false, "Illegal token type for operator at interpretation.");
                        0
                    }
                }
            }
            _ => {
                assert!(false, "Illegal node type for expression at interpretation.");
                0
            }
        }
    }

    fn evaluate_string(&self, idx: usize) -> String {
        match self.tree[idx].data {
            NodeType::Operand {
                token,
                symbol_type: _,
            } => {
                let token = token.unwrap();
                match token.token_type {
                    TokenType::LiteralString => token.value.to_owned(),
                    TokenType::Identifier => self.strings.get(token.value).unwrap().to_owned(),
                    _ => {
                        assert!(false, "Should never happen. {:#?}", token);
                        "".to_owned()
                    }
                }
            }
            NodeType::Expression {
                operator,
                symbol_type: _,
            } => {
                let operator = operator.unwrap();
                let lc = self.tree[idx].left_child.unwrap();
                let rs = self.tree[lc].right_sibling.unwrap();

                match operator.token_type {
                    TokenType::OperatorPlus => self.evaluate_string(lc) + &self.evaluate_string(rs),
                    _ => {
                        assert!(false, "Illegal token type for operator at interpretation.");
                        "".to_owned()
                    }
                }
            }
            _ => {
                assert!(false, "Illegal node type for expression at interpretation.");
                "".to_owned()
            }
        }
    }

    fn handle_declaration(
        &mut self,
        identifier: &TokenData<'a>,
        symbol_type: SymbolType,
        expr: Option<usize>,
    ) {
        self.symbols.insert(identifier.value, symbol_type);
        match symbol_type {
            SymbolType::Int => {
                let value = match expr {
                    Some(idx) => self.evaluate_int(idx),
                    None => 0,
                };
                self.integers.insert(identifier.value, value);
            }
            SymbolType::String => {
                let value = match expr {
                    Some(idx) => self.evaluate_string(idx),
                    None => "".to_owned(),
                };
                self.strings.insert(identifier.value, value);
            }
            SymbolType::Bool => {
                let value = match expr {
                    Some(idx) => self.evaluate_boolean(idx),
                    None => false,
                };
                self.booleans.insert(identifier.value, value);
            }
            SymbolType::Undefined => {
                assert!(
                    false,
                    "Declaration should never have an undefined type at interpretation."
                );
            }
        }
    }

    fn handle_assignment(&mut self, identifier: &TokenData<'a>, expression: usize) {
        match self.symbols.get(identifier.value) {
            Some(st) => match st {
                SymbolType::Int => {
                    let value = self.evaluate_int(expression);
                    self.integers.insert(identifier.value, value);
                }
                SymbolType::String => {
                    let value = self.evaluate_string(expression);
                    self.strings.insert(identifier.value, value);
                }
                SymbolType::Bool => {
                    let value = self.evaluate_boolean(expression);
                    self.booleans.insert(identifier.value, value);
                }
                SymbolType::Undefined => {
                    assert!(
                        false,
                        "Symbol type should never be undefined at interpretation."
                    );
                }
            },
            None => {
                assert!(false, "Should never happen.");
            }
        }
    }

    fn handle_for(
        &mut self,
        identifier: &TokenData<'a>,
        start_expr: usize,
        end_expr: usize,
        first_statement: Option<usize>,
    ) {
        let range_start = self.evaluate_int(start_expr);
        let range_end = self.evaluate_int(end_expr);
        if range_start < range_end {
            if let Some(idx) = first_statement {
                let mut next;
                for i in range_start..=range_end {
                    self.integers.insert(identifier.value, i);
                    next = idx;
                    self.handle_statement(next);
                    while let Some(id) = self.tree[next].right_sibling {
                        self.handle_statement(id);
                        next = id;
                    }
                }
            }
        } else {
            eprint!("Start of range is not smaller than end of range in for loop. ");
            eprint!(
                "Range starts from {} and ends at {}.",
                range_start, range_end
            );
            eprintln!(
                "{}. Exiting.",
                self.logger.get_line(identifier.line as usize)
            );
            process::exit(1);
        }
    }

    fn handle_read(&mut self, identifier: &TokenData<'a>) {
        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => {
                let input = match buffer.split(char::is_whitespace).next() {
                    Some(v) => v,
                    None => {
                        eprintln!("Empty input is not accepted. Exiting.");
                        process::exit(1);
                    }
                };
                match self.symbols.get(identifier.value) {
                    Some(st) => match st {
                        SymbolType::Int => {
                            match input.parse::<i32>() {
                                Ok(v) => self.integers.insert(identifier.value, v),
                                Err(e) => {
                                    eprintln!(
                                        "Bad input, couldn't parse to integer. Error: {}. Exiting.",
                                        e
                                    );
                                    process::exit(1);
                                }
                            };
                        }
                        SymbolType::String => {
                            self.strings.insert(identifier.value, input.to_owned());
                        }
                        SymbolType::Bool => {
                            assert!(false, "This should be caught during semantic analysis.");
                        }
                        SymbolType::Undefined => {
                            assert!(
                                false,
                                "Symbol type should never be undefined in interpretation."
                            );
                        }
                    },
                    None => {
                        assert!(false, "Should never happen.");
                    }
                }
            }
            Err(e) => {
                eprintln!("Bad input: {}. Exiting.", e);
                process::exit(1);
            }
        }
    }

    fn handle_print(&mut self, _: &TokenData<'a>, expression: usize) {
        match self.tree[expression].data {
            NodeType::Operand {
                token: _,
                symbol_type,
            }
            | NodeType::Expression {
                operator: _,
                symbol_type,
            } => match symbol_type {
                SymbolType::Int => {
                    let value = self.evaluate_int(expression);
                    println!("{}", value);
                }
                SymbolType::String => {
                    let value = self.evaluate_string(expression);
                    println!("{}", value);
                }
                SymbolType::Bool => {
                    let value = self.evaluate_boolean(expression);
                    println!("{}", value);
                }
                SymbolType::Undefined => {
                    assert!(
                        false,
                        "Undefined reached at print handling during interpretation."
                    );
                }
            },
            _ => {
                assert!(false, "Should never happen.");
            }
        }
    }

    fn handle_assert(&mut self, token: &TokenData<'a>, expression: usize) {
        if self.evaluate_boolean(expression) {
        } else {
            eprintln!(
                "Assertion failed on line {}! {}",
                token.line,
                self.logger.get_line(token.line as usize)
            );
            process::exit(1);
        }
    }

    pub fn new(tree: &'b LcRsTree<NodeType<'a>>, logger: &'b mut Logger<'a>) -> Self {
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

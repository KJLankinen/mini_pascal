use super::data_types::{ErrorType, NodeData, NodeType, SymbolType, TokenType};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::{HashMap, HashSet};

pub struct Analyzer<'a, 'b> {
    tree: &'b mut LcRsTree<NodeData<'a>>,
    symbols: HashMap<&'a str, SymbolType>,
    logger: &'b mut Logger<'a>,
    blocked_identifiers: HashSet<&'a str>,
}

impl<'a, 'b> Analyzer<'a, 'b> {
    // ---------------------------------------------------------------------
    // Called to start the semantic analysis
    // ---------------------------------------------------------------------
    pub fn analyze(&mut self) {
        // Start walking down the tree
        assert!(self.tree.len() > 0);
        let mut node = self.tree[0].left_child;

        while let Some(idx) = node {
            self.handle_statement(idx);
            node = self.tree[idx].right_sibling;
        }
    }

    // ---------------------------------------------------------------------
    // Functions that handle the semantic constraints
    // ---------------------------------------------------------------------
    fn handle_statement(&mut self, idx: usize) {
        match self.tree[idx].data.node_type {
            NodeType::Declaration => self.handle_declaration(idx),
            NodeType::Assignment => self.handle_assignment(idx),
            NodeType::For => self.handle_for(idx),
            NodeType::Read => self.handle_read(idx),
            NodeType::Print => self.handle_print(idx),
            NodeType::Assert => self.handle_assert(idx),
            _ => {
                assert!(false, "Unhandled node type.");
                ()
            }
        };
    }

    fn get_operand_type(&mut self, idx: usize) -> SymbolType {
        let token = &self.tree[idx]
            .data
            .token
            .expect("Operand is missing a token.");

        match token.token_type {
            TokenType::LiteralInt => {
                self.tree[idx].data.node_type = NodeType::Operand(SymbolType::Int);
                SymbolType::Int
            }
            TokenType::LiteralString => {
                self.tree[idx].data.node_type = NodeType::Operand(SymbolType::String);
                SymbolType::String
            }
            TokenType::Identifier => {
                if let Some(symbol) = self.symbols.get(token.value) {
                    self.tree[idx].data.node_type = NodeType::Operand(*symbol);
                    *symbol
                } else {
                    self.logger
                        .add_error(ErrorType::UndeclaredIdentifier(*token));
                    SymbolType::Undefined
                }
            }
            _ => {
                assert!(false, "Operand has an illegal token type.");
                SymbolType::Undefined
            }
        }
    }

    fn get_expression_type(&mut self, idx: usize) -> SymbolType {
        if let NodeType::Operand(_) = self.tree[idx].data.node_type {
            self.get_operand_type(idx)
        } else {
            let lc = self.tree[idx]
                .left_child
                .expect("Expression is missing a child node.");
            let et = self.get_expression_type(lc);
            let expr_token = &self.tree[idx]
                .data
                .token
                .expect("Expression is missing a token.");

            match expr_token.token_type {
                TokenType::OperatorNot => {
                    if SymbolType::Bool == et {
                        self.tree[idx].data.node_type = NodeType::Expression(et);
                        SymbolType::Bool
                    } else {
                        self.logger
                            .add_error(ErrorType::IllegalOperation(*expr_token, et));
                        SymbolType::Undefined
                    }
                }
                tt @ TokenType::OperatorAnd
                | tt @ TokenType::OperatorPlus
                | tt @ TokenType::OperatorMinus
                | tt @ TokenType::OperatorMultiply
                | tt @ TokenType::OperatorDivide
                | tt @ TokenType::OperatorLessThan
                | tt @ TokenType::OperatorEqual => {
                    let et2 = self.get_expression_type(
                        self.tree[lc]
                            .right_sibling
                            .expect("Expression is missing its second child."),
                    );

                    if et == et2 {
                        match tt {
                            TokenType::OperatorAnd => {
                                if SymbolType::Bool == et {
                                    self.tree[idx].data.node_type = NodeType::Expression(et);
                                    et
                                } else {
                                    self.logger
                                        .add_error(ErrorType::IllegalOperation(*expr_token, et));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorPlus => {
                                if SymbolType::String == et || SymbolType::Int == et {
                                    self.tree[idx].data.node_type = NodeType::Expression(et);
                                    et
                                } else {
                                    self.logger
                                        .add_error(ErrorType::IllegalOperation(*expr_token, et));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorMinus => {
                                if SymbolType::Int == et {
                                    self.tree[idx].data.node_type = NodeType::Expression(et);
                                    et
                                } else {
                                    self.logger
                                        .add_error(ErrorType::IllegalOperation(*expr_token, et));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorMultiply => {
                                if SymbolType::Int == et {
                                    self.tree[idx].data.node_type = NodeType::Expression(et);
                                    et
                                } else {
                                    self.logger
                                        .add_error(ErrorType::IllegalOperation(*expr_token, et));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorDivide => {
                                if SymbolType::Int == et {
                                    self.tree[idx].data.node_type = NodeType::Expression(et);
                                    et
                                } else {
                                    self.logger
                                        .add_error(ErrorType::IllegalOperation(*expr_token, et));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorLessThan => {
                                self.tree[idx].data.node_type =
                                    NodeType::Expression(SymbolType::Bool);
                                SymbolType::Bool
                            }
                            TokenType::OperatorEqual => {
                                self.tree[idx].data.node_type =
                                    NodeType::Expression(SymbolType::Bool);
                                SymbolType::Bool
                            }
                            _ => {
                                assert!(false, "Expression token has an illegal type.");
                                SymbolType::Undefined
                            }
                        }
                    } else {
                        self.logger.add_error(ErrorType::MismatchedTypes(
                            *expr_token,
                            et,
                            Some(et2),
                        ));
                        SymbolType::Undefined
                    }
                }
                _ => {
                    assert!(false, "Expression token has an illegal type.");
                    SymbolType::Undefined
                }
            }
        }
    }

    fn handle_declaration(&mut self, idx: usize) {
        let lc = self.tree[idx]
            .left_child
            .expect("Declaration is missing an identifier.");
        let id_token = &self.tree[lc]
            .data
            .token
            .expect("Identifier is missing a token.");
        let symbol = self.symbols.get(id_token.value);

        if symbol.is_none() {
            if let TokenType::Type(t) = &self.tree[idx]
                .data
                .token
                .expect("Declaration is missing a token.")
                .token_type
            {
                self.symbols.insert(id_token.value, *t);
                let rs = self.tree[lc]
                    .right_sibling
                    .expect("Declaration is missing a child expression.");

                if self.tree[rs].data.token.is_some() {
                    let et = self.get_expression_type(rs);
                    if et != *t {
                        self.logger.add_error(ErrorType::MismatchedTypes(
                            self.tree[idx]
                                .data
                                .token
                                .expect("Declaration is missing a token."),
                            *t,
                            Some(et),
                        ));
                    }
                    match self.tree[rs].data.node_type {
                        NodeType::Expression(_) => {
                            self.tree[rs].data.node_type = NodeType::Expression(*t)
                        }
                        NodeType::Operand(_) => {
                            self.tree[rs].data.node_type = NodeType::Operand(*t)
                        }
                        _ => assert!(
                            false,
                            "Declaration's child expression has a wrong node type."
                        ),
                    }
                }
            } else {
                assert!(false, "Identifier's TokenType is something else than Type.");
            }
        } else {
            self.logger.add_error(ErrorType::Redeclaration(*id_token));
        }
    }

    fn handle_assignment(&mut self, idx: usize) {
        let identifier = &self.tree[self.tree[idx]
            .left_child
            .expect("Assignment is missing an identifier in AST.")];
        let id_token = &identifier
            .data
            .token
            .expect("Identifier is missing a token.");

        if self.blocked_identifiers.contains(id_token.value) {
            self.logger
                .add_error(ErrorType::AssignmentToBlockedVariable(*id_token));
        } else {
            let rs = identifier
                .right_sibling
                .expect("Assignment should contain an expression.");
            let et = self.get_expression_type(rs);
            let symbol = self.symbols.get(id_token.value);

            if symbol.is_none() {
                self.logger
                    .add_error(ErrorType::UndeclaredIdentifier(*id_token));
            } else if et != *symbol.unwrap() {
                self.logger.add_error(ErrorType::MismatchedTypes(
                    self.tree[idx]
                        .data
                        .token
                        .expect("Assignment is missing a token."),
                    *symbol.unwrap(),
                    Some(et),
                ));
            } else {
                match self.tree[rs].data.node_type {
                    NodeType::Expression(_) => {
                        self.tree[rs].data.node_type = NodeType::Expression(et)
                    }
                    NodeType::Operand(_) => self.tree[rs].data.node_type = NodeType::Operand(et),
                    _ => assert!(
                        false,
                        "Declaration's child expression has a wrong node type."
                    ),
                }
            }
        }
    }

    fn handle_for(&mut self, idx: usize) {
        let node = &self.tree[idx];
        let identifier = &self.tree[node
            .left_child
            .expect("Assignment is missing an identifier in AST.")];
        let id_token = identifier
            .data
            .token
            .expect("Identifier is missing a token.");

        if self.symbols.contains_key(id_token.value) {
            let rs = identifier
                .right_sibling
                .expect("For loop should contain an expression.");
            let et = self.get_expression_type(rs);

            if SymbolType::Int == et {
                let rs = self.tree[rs]
                    .right_sibling
                    .expect("For loop should contain an expression.");
                let et = self.get_expression_type(rs);

                if SymbolType::Int == et {
                    self.blocked_identifiers.insert(id_token.value);
                    let mut next_statement = self.tree[rs].right_sibling;
                    while let Some(rs) = next_statement {
                        self.handle_statement(rs);
                        next_statement = self.tree[rs].right_sibling;
                    }
                    self.blocked_identifiers.remove(id_token.value);
                } else {
                    self.logger.add_error(ErrorType::MismatchedTypes(
                        self.tree[idx]
                            .data
                            .token
                            .expect("For loop is missing a token."),
                        et,
                        None,
                    ));
                }
            } else {
                self.logger.add_error(ErrorType::MismatchedTypes(
                    self.tree[idx]
                        .data
                        .token
                        .expect("For loop is missing a token."),
                    et,
                    None,
                ));
            }
        } else {
            self.logger
                .add_error(ErrorType::UndeclaredIdentifier(id_token));
        }
    }

    fn handle_read(&mut self, idx: usize) {
        let token = &self.tree[idx]
            .data
            .token
            .expect("Read statement has no token.");

        if let Some(symbol) = self.symbols.get(token.value) {
            let symbol = *symbol;
            if SymbolType::Int == symbol || SymbolType::String == symbol {
                self.tree[idx].data.node_type = NodeType::Operand(symbol);
            } else {
                self.logger
                    .add_error(ErrorType::MismatchedTypes(*token, symbol, None));
            }
        } else {
            self.logger
                .add_error(ErrorType::UndeclaredIdentifier(*token));
        }
    }

    fn handle_print(&mut self, idx: usize) {
        let et = self.get_expression_type(
            self.tree[idx]
                .left_child
                .expect("Print is missing a child expression."),
        );
        if SymbolType::Int == et || SymbolType::String == et {
            let lc = self.tree[idx]
                .left_child
                .expect("Print is missing a child expression.");

            match self.tree[lc].data.node_type {
                NodeType::Expression(_) => self.tree[lc].data.node_type = NodeType::Expression(et),
                NodeType::Operand(_) => self.tree[lc].data.node_type = NodeType::Operand(et),
                _ => assert!(false, "Print's child expression has a wrong node type."),
            }
        } else {
            self.logger.add_error(ErrorType::MismatchedTypes(
                self.tree[idx]
                    .data
                    .token
                    .expect("Print is missing a token."),
                et,
                None,
            ));
        }
    }

    fn handle_assert(&mut self, idx: usize) {
        let et = self.get_expression_type(
            self.tree[idx]
                .left_child
                .expect("Assert is missing a child expression."),
        );

        if SymbolType::Bool == et {
            let lc = self.tree[idx]
                .left_child
                .expect("Assert is missing a child expression.");

            match self.tree[lc].data.node_type {
                NodeType::Expression(_) => self.tree[lc].data.node_type = NodeType::Expression(et),
                NodeType::Operand(_) => self.tree[lc].data.node_type = NodeType::Operand(et),
                _ => assert!(false, "Assert's child expression has a wrong node type."),
            }
        } else {
            self.logger.add_error(ErrorType::MismatchedTypes(
                self.tree[idx]
                    .data
                    .token
                    .expect("Assertion is missing a token."),
                et,
                None,
            ));
        }
    }

    // ---------------------------------------------------------------------
    // Auxiliary functions
    // ---------------------------------------------------------------------
    pub fn new(tree: &'b mut LcRsTree<NodeData<'a>>, logger: &'b mut Logger<'a>) -> Self {
        Analyzer {
            tree: tree,
            symbols: HashMap::new(),
            logger: logger,
            blocked_identifiers: HashSet::new(),
        }
    }
}

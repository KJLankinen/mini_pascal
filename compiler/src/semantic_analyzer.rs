use super::data_types::{ErrorType, NodeData, NodeType, SymbolType, TokenType};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::{HashMap, HashSet};

pub struct Analyzer<'a, 'b> {
    tree: &'b LcRsTree<NodeData<'a>>,
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
        match self.tree[idx]
            .data
            .node_type
            .expect("AST should not contain Nodes with type None.")
        {
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
            TokenType::LiteralInt => SymbolType::Int,
            TokenType::LiteralString => SymbolType::String,
            TokenType::Identifier => {
                if let Some(symbol) = self.symbols.get(token.value) {
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
        if let NodeType::Operand(_) = self.tree[idx]
            .data
            .node_type
            .expect("No node type on expression.")
        {
            self.get_operand_type(idx)
        } else {
            let lc = self.tree[idx]
                .left_child
                .expect("Expression is missing a child node.");
            let expr_type = self.get_expression_type(lc);
            let expr_token = &self.tree[idx]
                .data
                .token
                .expect("Expression is missing a token.");

            match expr_token.token_type {
                TokenType::OperatorNot => {
                    if SymbolType::Bool == expr_type {
                        SymbolType::Bool
                    } else {
                        self.logger
                            .add_error(ErrorType::IllegalOperation(*expr_token, expr_type));
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
                    let second_expr_type = self.get_expression_type(
                        self.tree[lc]
                            .right_sibling
                            .expect("Expression is missing its second child."),
                    );

                    if expr_type == second_expr_type {
                        match tt {
                            TokenType::OperatorAnd => {
                                if SymbolType::Bool == expr_type {
                                    expr_type
                                } else {
                                    self.logger.add_error(ErrorType::IllegalOperation(
                                        *expr_token,
                                        expr_type,
                                    ));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorPlus => {
                                if SymbolType::String == expr_type || SymbolType::Int == expr_type {
                                    expr_type
                                } else {
                                    self.logger.add_error(ErrorType::IllegalOperation(
                                        *expr_token,
                                        expr_type,
                                    ));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorMinus => {
                                if SymbolType::Int == expr_type {
                                    expr_type
                                } else {
                                    self.logger.add_error(ErrorType::IllegalOperation(
                                        *expr_token,
                                        expr_type,
                                    ));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorMultiply => {
                                if SymbolType::Int == expr_type {
                                    expr_type
                                } else {
                                    self.logger.add_error(ErrorType::IllegalOperation(
                                        *expr_token,
                                        expr_type,
                                    ));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorDivide => {
                                if SymbolType::Int == expr_type {
                                    expr_type
                                } else {
                                    self.logger.add_error(ErrorType::IllegalOperation(
                                        *expr_token,
                                        expr_type,
                                    ));
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorLessThan => SymbolType::Bool,
                            TokenType::OperatorEqual => SymbolType::Bool,
                            _ => {
                                assert!(false, "Expression token has an illegal type.");
                                SymbolType::Undefined
                            }
                        }
                    } else {
                        self.logger.add_error(ErrorType::MismatchedTypes(
                            *expr_token,
                            expr_type,
                            Some(second_expr_type),
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
        let id_token = &self.tree[self.tree[idx]
            .left_child
            .expect("Declaration is missing an identifier.")]
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
            } else {
                assert!(false, "Identifier's TokenType is something else than Type.");
            }
        } else {
            self.logger.add_error(ErrorType::Redeclaration(*id_token));
        }
    }

    fn handle_assignment(&mut self, idx: usize) {
        let node = &self.tree[idx];
        let identifier = &self.tree[node
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
            let expr_type = self.get_expression_type(rs);
            let symbol = self.symbols.get(id_token.value);

            if symbol.is_none() {
                self.logger
                    .add_error(ErrorType::UndeclaredIdentifier(*id_token));
            } else if expr_type != *symbol.unwrap() {
                self.logger.add_error(ErrorType::MismatchedTypes(
                    node.data.token.expect("Assignment is missing a token."),
                    *symbol.unwrap(),
                    Some(expr_type),
                ));
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
            let expr_type = self.get_expression_type(rs);

            if SymbolType::Int == expr_type {
                let rs = self.tree[rs]
                    .right_sibling
                    .expect("For loop should contain an expression.");
                let expr_type = self.get_expression_type(rs);

                if SymbolType::Int == expr_type {
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
                        expr_type,
                        None,
                    ));
                }
            } else {
                self.logger.add_error(ErrorType::MismatchedTypes(
                    self.tree[idx]
                        .data
                        .token
                        .expect("For loop is missing a token."),
                    expr_type,
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
            if SymbolType::Int != symbol && SymbolType::String != symbol {
                self.logger
                    .add_error(ErrorType::MismatchedTypes(*token, symbol, None));
            }
        } else {
            self.logger
                .add_error(ErrorType::UndeclaredIdentifier(*token));
        }
    }

    fn handle_print(&mut self, idx: usize) {
        let expr_type = self.get_expression_type(
            self.tree[idx]
                .left_child
                .expect("Print is missing a child expression."),
        );
        if SymbolType::Int != expr_type && SymbolType::String != expr_type {
            self.logger.add_error(ErrorType::MismatchedTypes(
                self.tree[idx]
                    .data
                    .token
                    .expect("Print is missing a token."),
                expr_type,
                None,
            ));
        }
    }

    fn handle_assert(&mut self, idx: usize) {
        let expr_type = self.get_expression_type(
            self.tree[idx]
                .left_child
                .expect("Assert is missing a child expression."),
        );

        if SymbolType::Bool != expr_type {
            self.logger.add_error(ErrorType::MismatchedTypes(
                self.tree[idx]
                    .data
                    .token
                    .expect("Assertion is missing a token."),
                expr_type,
                None,
            ));
        }
    }

    // ---------------------------------------------------------------------
    // Auxiliary functions
    // ---------------------------------------------------------------------
    pub fn new(tree: &'b LcRsTree<NodeData<'a>>, logger: &'b mut Logger<'a>) -> Self {
        Analyzer {
            tree: tree,
            symbols: HashMap::new(),
            logger: logger,
            blocked_identifiers: HashSet::new(),
        }
    }
}

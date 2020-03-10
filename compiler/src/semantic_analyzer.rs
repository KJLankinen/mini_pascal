use super::data_types::{ErrorType, NodeType, SymbolType, TokenData, TokenType};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::{HashMap, HashSet};

pub struct Analyzer<'a, 'b> {
    tree: &'b mut LcRsTree<NodeType<'a>>,
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

    fn check_expression(&mut self, idx: usize) -> SymbolType {
        match self.tree[idx].data {
            NodeType::Operand { token, symbol_type } => match symbol_type {
                SymbolType::Int | SymbolType::String | SymbolType::Bool => symbol_type,
                SymbolType::Undefined => {
                    let ref token = token.expect("Operand is missing a token.");
                    match self.symbols.get(token.value) {
                        Some(st) => {
                            self.tree[idx].data = NodeType::Operand {
                                token: Some(*token),
                                symbol_type: *st,
                            };
                            *st
                        }
                        None => {
                            self.logger
                                .add_error(ErrorType::UndeclaredIdentifier(*token));
                            SymbolType::Undefined
                        }
                    }
                }
            },
            NodeType::Expression {
                operator,
                symbol_type: _,
            } => {
                let operator = operator.expect("Expression is missing an operator token.");
                let lc = self.tree[idx]
                    .left_child
                    .expect("Expression is missing a child.");
                let expr_type = self.check_expression(lc);

                let et = match operator.token_type {
                    TokenType::OperatorNot => {
                        if SymbolType::Bool == expr_type {
                            SymbolType::Bool
                        } else {
                            self.logger
                                .add_error(ErrorType::IllegalOperation(operator, expr_type));
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
                        let expr_type2 = self.check_expression(
                            self.tree[lc]
                                .right_sibling
                                .expect("Expression is missing its second child."),
                        );

                        if expr_type == expr_type2 {
                            match tt {
                                TokenType::OperatorAnd => {
                                    if SymbolType::Bool == expr_type {
                                        expr_type
                                    } else {
                                        SymbolType::Undefined
                                    }
                                }
                                TokenType::OperatorPlus => {
                                    if SymbolType::String == expr_type
                                        || SymbolType::Int == expr_type
                                    {
                                        expr_type
                                    } else {
                                        SymbolType::Undefined
                                    }
                                }
                                TokenType::OperatorMinus => {
                                    if SymbolType::Int == expr_type {
                                        expr_type
                                    } else {
                                        SymbolType::Undefined
                                    }
                                }
                                TokenType::OperatorMultiply => {
                                    if SymbolType::Int == expr_type {
                                        expr_type
                                    } else {
                                        SymbolType::Undefined
                                    }
                                }
                                TokenType::OperatorDivide => {
                                    if SymbolType::Int == expr_type {
                                        expr_type
                                    } else {
                                        SymbolType::Undefined
                                    }
                                }
                                TokenType::OperatorLessThan => SymbolType::Bool,
                                TokenType::OperatorEqual => SymbolType::Bool,
                                _ => {
                                    assert!(false, "Illegal token type for an operator.");
                                    SymbolType::Undefined
                                }
                            }
                        } else {
                            self.logger.add_error(ErrorType::MismatchedTypes(
                                operator, expr_type, expr_type2,
                            ));

                            return SymbolType::Undefined;
                        }
                    }
                    _ => {
                        assert!(false, "Expression token has an illegal type.");
                        SymbolType::Undefined
                    }
                };

                if SymbolType::Undefined == et {
                    self.logger
                        .add_error(ErrorType::IllegalOperation(operator, expr_type));
                } else {
                    self.tree[idx].data = NodeType::Expression {
                        operator: Some(operator),
                        symbol_type: et,
                    };
                }

                et
            }
            _ => {
                assert!(
                    false,
                    "Tree does not contain neither an expression nor an operand at given index."
                );
                SymbolType::Undefined
            }
        }
    }

    fn handle_declaration(
        &mut self,
        identifier: &TokenData<'a>,
        symbol_type: SymbolType,
        expr: Option<usize>,
    ) {
        match self.symbols.get(identifier.value) {
            Some(_) => {
                self.logger.add_error(ErrorType::Redeclaration(*identifier));
            }
            None => {
                self.symbols.insert(identifier.value, symbol_type);
                if let Some(idx) = expr {
                    let expr_type = self.check_expression(idx);
                    if expr_type != symbol_type {
                        self.logger.add_error(ErrorType::AssignMismatchedType(
                            *identifier,
                            symbol_type,
                            expr_type,
                        ));
                    }
                }
            }
        }
    }

    fn handle_assignment(&mut self, identifier: &TokenData<'a>, expression: usize) {
        if self.blocked_identifiers.contains(identifier.value) {
            self.logger
                .add_error(ErrorType::AssignmentToBlockedVariable(*identifier));
        } else {
            match self.symbols.get(identifier.value) {
                Some(st) => {
                    let st = *st;
                    let expr_type = self.check_expression(expression);
                    if expr_type != st {
                        self.logger.add_error(ErrorType::AssignMismatchedType(
                            *identifier,
                            st,
                            expr_type,
                        ));
                    }
                }
                None => {
                    self.logger
                        .add_error(ErrorType::UndeclaredIdentifier(*identifier));
                }
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
        match self.symbols.get(identifier.value) {
            Some(st) => {
                if SymbolType::Int == *st {
                    let expr_type = self.check_expression(start_expr);
                    if SymbolType::Int == expr_type {
                        let expr_type = self.check_expression(end_expr);
                        if SymbolType::Int == expr_type {
                            self.blocked_identifiers.insert(identifier.value);
                            let mut next_statement = first_statement;
                            while let Some(rs) = next_statement {
                                self.handle_statement(rs);
                                next_statement = self.tree[rs].right_sibling;
                            }
                            self.blocked_identifiers.remove(identifier.value);
                        } else {
                            self.logger.add_error(ErrorType::ForMismatchedType(
                                *identifier,
                                None,
                                None,
                                Some(expr_type),
                            ));
                        }
                    } else {
                        self.logger.add_error(ErrorType::ForMismatchedType(
                            *identifier,
                            None,
                            Some(expr_type),
                            None,
                        ));
                    }
                } else {
                    self.logger.add_error(ErrorType::ForMismatchedType(
                        *identifier,
                        Some(*st),
                        None,
                        None,
                    ));
                }
            }
            None => {
                self.logger
                    .add_error(ErrorType::UndeclaredIdentifier(*identifier));
            }
        }
    }

    fn handle_read(&mut self, identifier: &TokenData<'a>) {
        match self.symbols.get(identifier.value) {
            Some(st) => match st {
                SymbolType::Int | SymbolType::String => (),
                _ => {
                    self.logger
                        .add_error(ErrorType::IOMismatchedType(*identifier, *st));
                }
            },
            None => {
                self.logger
                    .add_error(ErrorType::UndeclaredIdentifier(*identifier));
            }
        }
    }

    fn handle_print(&mut self, token: &TokenData<'a>, expression: usize) {
        let expr_type = self.check_expression(expression);
        if SymbolType::Int != expr_type && SymbolType::String != expr_type {
            self.logger
                .add_error(ErrorType::IOMismatchedType(*token, expr_type));
        }
    }

    fn handle_assert(&mut self, token: &TokenData<'a>, expression: usize) {
        let expr_type = self.check_expression(expression);
        if SymbolType::Bool != expr_type {
            self.logger
                .add_error(ErrorType::AssertMismatchedType(*token, expr_type));
        }
    }

    // ---------------------------------------------------------------------
    // Auxiliary functions
    // ---------------------------------------------------------------------
    pub fn new(tree: &'b mut LcRsTree<NodeType<'a>>, logger: &'b mut Logger<'a>) -> Self {
        Analyzer {
            tree: tree,
            symbols: HashMap::new(),
            logger: logger,
            blocked_identifiers: HashSet::new(),
        }
    }
}

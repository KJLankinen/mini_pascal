use super::data_types::{ErrorType, NodeType, SymbolType, TokenData, TokenType};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::{HashMap, HashSet};

pub struct Analyzer<'a, 'b> {
    tree: &'b mut LcRsTree<NodeType<'a>>,
    // scoped symbol table with a vector can be something like the following:
    // symbols: HashMap<(&'a str, u32), SymbolType>,
    // scope_stack: Vec<u32>,
    // then loop over scope_stack and get the first variable with the correct name
    symbols: HashMap<&'a str, SymbolType>,
    logger: &'b mut Logger<'a>,
}

impl<'a, 'b> Analyzer<'a, 'b> {
    // ---------------------------------------------------------------------
    // Called to start the semantic analysis
    // ---------------------------------------------------------------------
    pub fn analyze(&mut self) {
        assert!(self.tree.len() > 0);
        self.handle_statement(0);
    }

    // ---------------------------------------------------------------------
    // Functions that handle the semantic constraints
    // ---------------------------------------------------------------------
    fn handle_statement(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::Program(data) => {
                // Subroutines are under optional idx, main block under idx
                if let Some(idx) = data.opt_idx {
                    self.handle_statement(idx);
                }
                self.handle_statement(data.idx);
            }
            NodeType::Block(idx) => {
                // Idx points to first statement
                self.handle_statement(idx);
                let mut next = idx;
                while let Some(idx) = self.tree[next].right_sibling {
                    self.handle_statement(idx);
                    next = idx;
                }
            }
            NodeType::Subroutines(idx) => {
                // Idx points to first subroutine
                self.handle_statement(idx);
                let mut next = idx;
                while let Some(idx) = self.tree[next].right_sibling {
                    self.handle_statement(idx);
                    next = idx;
                }
            }
            NodeType::Function(data) => {
                // TokenIdxOptIdxOptIdx
                // Token is identifier
                // Idx is block
                // opt_idx1 is first parameter
                // opt_idx2 is return type
            }
            NodeType::ParamList(idx) => {
                // Idx points to first parameter
                self.handle_statement(idx);
                let mut next = idx;
                while let Some(idx) = self.tree[next].right_sibling {
                    self.handle_statement(idx);
                    next = idx;
                }
            }
            NodeType::Parameter(data) => {
                // token is name (alias) of identifier
                // idx points to variable type
                // bool says if reference
            }
            NodeType::VariableType(symbol_type) => {
                // type is type
            }
            NodeType::Identifier(token) => {
                // token is identifier
            }
            NodeType::Assert(idx) => {
                // idx points to boolean expression
            }
            NodeType::Assignment(data) => {
                // token is identifier
                // idx is expression
                // opt_idx is array indexing expression
            }
            NodeType::Call(data) => {
                // token is identifier
                // idx is first expression
            }
            NodeType::Declaration(data) => {
                // idx1 is first identifier
                // idx2 is type
            }
            NodeType::Return(idx) => {
                // idx is expression
            }
            NodeType::Read(idx) => {
                // idx is first variable node
            }
            NodeType::Write(idx) => {
                // idx is first expression
            }
            NodeType::If(data) => {
                // idx1 is boolean expr
                // idx2 is if statement
                // opt_idx is else statement
            }
            NodeType::While(data) => {
                // idx1 is boolean expression
                // idx2 is statement
            }
            NodeType::Expression(data) => {
                // token is rel op
                // idx1 is first simple expr
                // idx2 is second simple expr
            }
            NodeType::SimpleExpression(idx) => {
                // idx is first term
            }
            NodeType::Term(data) => {
                // token is sign/operator
                // idx is factor
            }
            NodeType::Factor(data) => {
                // token is operator or None for first factor
                // idx is the concrete factor node
            }
            NodeType::Variable(data) => {
                // token is identifier
                // opt_idx is possible array indexing expression
            }
            NodeType::Literal(token) => {
                // token is the literal data
            }
            NodeType::Not(data) => {
                // token is the not operator
                // idx is the factor
            }
            NodeType::ArraySize(data) => {
                // token is the .size operator
                // idx is the factor
            }
            _ => {
                assert!(false, "Unhandled node type.");
                ()
            }
        };
    }

    /*fn check_expression(&mut self, idx: usize) -> SymbolType {
        match self.tree[idx].data {
            NodeType::Operand { token, symbol_type } => {
                match symbol_type {
                    SymbolType::Int | SymbolType::String | SymbolType::Bool => symbol_type,
                    SymbolType::Undefined => {
                        let ref token = token.expect("Operand is missing a token.");
                        match self.symbols.get(token.value) {
                            Some(st) => {
                                assert!(SymbolType::Undefined != *st, "Symbol type of a declared identifier should never be undefined.");
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
                }
            }
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
                    TokenType::OperatorNot => match expr_type {
                        // If expression has type Undefined, an error has already been flagged so
                        // stop the error propagation here.
                        SymbolType::Bool | SymbolType::Undefined => SymbolType::Bool,
                        _ => {
                            self.logger
                                .add_error(ErrorType::IllegalOperation(operator, expr_type));
                            SymbolType::Undefined
                        }
                    },
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

                        let same_type = expr_type == expr_type2;
                        let has_undefined = SymbolType::Undefined == expr_type
                            || SymbolType::Undefined == expr_type2;

                        if same_type && has_undefined {
                            // Both are Undefined, don't check anything, because we have no
                            // information, making any analysis just guessing.
                            return SymbolType::Undefined;
                        } else if same_type || has_undefined {
                            let correct_type = if SymbolType::Undefined == expr_type {
                                expr_type2
                            } else {
                                expr_type
                            };

                            match correct_type {
                                SymbolType::Int => match tt {
                                    TokenType::OperatorPlus
                                    | TokenType::OperatorMinus
                                    | TokenType::OperatorMultiply
                                    | TokenType::OperatorDivide => correct_type,
                                    TokenType::OperatorAnd => SymbolType::Undefined,
                                    TokenType::OperatorLessThan | TokenType::OperatorEqual => {
                                        SymbolType::Bool
                                    }
                                    _ => {
                                        assert!(false, "Illegal token type for an operator.");
                                        SymbolType::Undefined
                                    }
                                },
                                SymbolType::String => match tt {
                                    TokenType::OperatorPlus => correct_type,
                                    TokenType::OperatorAnd
                                    | TokenType::OperatorMinus
                                    | TokenType::OperatorMultiply
                                    | TokenType::OperatorDivide => SymbolType::Undefined,
                                    TokenType::OperatorLessThan | TokenType::OperatorEqual => {
                                        SymbolType::Bool
                                    }
                                    _ => {
                                        assert!(false, "Illegal token type for an operator.");
                                        SymbolType::Undefined
                                    }
                                },
                                SymbolType::Bool => match tt {
                                    TokenType::OperatorPlus
                                    | TokenType::OperatorMinus
                                    | TokenType::OperatorMultiply
                                    | TokenType::OperatorDivide => SymbolType::Undefined,
                                    TokenType::OperatorAnd
                                    | TokenType::OperatorLessThan
                                    | TokenType::OperatorEqual => SymbolType::Bool,
                                    _ => {
                                        assert!(false, "Illegal token type for an operator.");
                                        SymbolType::Undefined
                                    }
                                },
                                SymbolType::Undefined => {
                                    assert!(false, "This should never happen.");
                                    SymbolType::Undefined
                                }
                            }
                        } else {
                            // Neither is undefined, but types are different. This is a new error.
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
    }*/

    fn check_expression(&mut self, idx: usize) -> SymbolType {
        SymbolType::Undefined
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
                    // Don't flag error, if the expression type is undefined, because something is
                    // wrong with the expression and errors about that have already been flagged.
                    // This prevents unnecessary error propagation.
                    if expr_type != symbol_type && SymbolType::Undefined != expr_type {
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
        match self.symbols.get(identifier.value) {
            Some(st) => {
                let st = *st;
                let expr_type = self.check_expression(expression);
                // Don't flag error, if the expression type is undefined, because something is
                // wrong with the expression and errors about that have already been flagged.
                // This prevents unnecessary error propagation.
                if expr_type != st && SymbolType::Undefined != expr_type {
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
        if SymbolType::Int != expr_type
            && SymbolType::String != expr_type
            && SymbolType::Undefined != expr_type
        {
            self.logger
                .add_error(ErrorType::IOMismatchedType(*token, expr_type));
        }
    }

    fn handle_assert(&mut self, token: &TokenData<'a>, expression: usize) {
        let expr_type = self.check_expression(expression);
        if SymbolType::Bool != expr_type && SymbolType::Undefined != expr_type {
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
        }
    }
}

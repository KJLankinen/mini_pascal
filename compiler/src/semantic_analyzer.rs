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
    function_signatures: HashMap<&'a str, FunctionSignature<'a>>,
    logger: &'b mut Logger<'a>,
}

struct Parameter<'a> {
    is_ref: bool,
    symbol_type: SymbolType,
    id: &'a str,
}

struct FunctionSignature<'a> {
    parameters: Option<Vec<Parameter<'a>>>,
    return_type: Option<SymbolType>,
}

impl<'a, 'b> Analyzer<'a, 'b> {
    // ---------------------------------------------------------------------
    // Called to start the semantic analysis
    // ---------------------------------------------------------------------
    pub fn analyze(&mut self) {
        assert!(self.tree.len() > 0);
        self.program(0);
    }

    // ---------------------------------------------------------------------
    // Functions that handle the semantic constraints
    // ---------------------------------------------------------------------

    fn program(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::Program(data) => {
                // Subroutines are under optional idx, main block under idx
                if let Some(idx) = data.opt_idx {
                    self.subroutines(idx);
                }
                self.block(data.idx);
            }
            _ => {
                assert!(false, "Program must start with program node.");
            }
        }
    }

    fn subroutines(&mut self, idx: usize) {
        // Process the functions in two passes:
        // - first: process all function signatures and add them to map
        // - second: process the blocks of the functions
        // This way (mutually) recursive functions calls can be matched to known signatures.
        match self.tree[idx].data {
            NodeType::Subroutines(idx) => {
                // Process signatures
                self.function_signature(idx);
                let mut next = idx;
                while let Some(idx) = self.tree[next].right_sibling {
                    self.function_signature(idx);
                    next = idx;
                }

                // Process blocks
                self.function_block(idx);
                let mut next = idx;
                while let Some(idx) = self.tree[next].right_sibling {
                    self.function_block(idx);
                    next = idx;
                }
            }
            _ => {
                assert!(false, "Unexpected node.");
            }
        }
    }

    fn block(&mut self, idx: usize) {
        // Block must check that:
        // - if return type = Some, there is return statements everywhere and they have the correct
        // type
        // - else, there is no return statements, since the cfg does not allow "empty" returns.
        match self.tree[idx].data {
            NodeType::Block(idx) => {
                // Idx points to first statement
                self.statement(idx);
                let mut next = idx;
                while let Some(idx) = self.tree[next].right_sibling {
                    self.statement(idx);
                    next = idx;
                }
            }
            _ => {
                assert!(false, "Unexpected node.");
            }
        }
    }

    fn function_signature(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::Function(data) => {
                let token = data.token.expect("Function has no identifier token.");
                if let Some(_) = self.function_signatures.get(token.value) {
                    self.logger.add_error(ErrorType::Redeclaration(token));
                } else {
                    let mut fs = FunctionSignature {
                        parameters: None,
                        return_type: None,
                    };

                    // Handle parameter list
                    if let Some(idx) = data.opt_idx {
                        fn parameter<'a, 'b>(
                            analyzer: &mut Analyzer<'a, 'b>,
                            idx: usize,
                            param_vec: &mut Vec<Parameter<'a>>,
                        ) {
                            if let NodeType::Parameter(data) = analyzer.tree[idx].data {
                                // token is name (alias) of identifier
                                // idx points to variable type
                                // bool says if reference
                                param_vec.push(Parameter {
                                    is_ref: data.b,
                                    symbol_type: if let NodeType::VariableType(st) =
                                        analyzer.tree[data.idx].data
                                    {
                                        st
                                    } else {
                                        SymbolType::Undefined
                                    },
                                    id: data
                                        .token
                                        .expect("Parameter has no identifier token.")
                                        .value,
                                });
                            } else {
                                assert!(false, "Unexpected node.");
                            }
                        }

                        let mut param_vec = Vec::new();
                        if let NodeType::ParamList(idx) = self.tree[idx].data {
                            parameter(self, idx, &mut param_vec);
                            let mut next = idx;
                            while let Some(idx) = self.tree[next].right_sibling {
                                parameter(self, idx, &mut param_vec);
                                next = idx;
                            }
                        }
                        fs.parameters = Some(param_vec);
                    }

                    // Handle return type
                    if let Some(idx) = data.opt_idx2 {
                        fs.return_type = if let NodeType::VariableType(st) = self.tree[idx].data {
                            Some(st)
                        } else {
                            None
                        };
                    }

                    self.function_signatures.insert(token.value, fs);
                }
            }
            _ => {
                assert!(false, "Unexpected node.");
            }
        }
    }

    fn function_block(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::Function(data) => {
                self.block(data.idx);
            }
            _ => {
                assert!(false, "Unexpected node.");
            }
        }
    }

    fn statement(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::Assert(data) => {
                let et = self.check_expression(data.idx);
                if SymbolType::Bool != et && SymbolType::Undefined != et {
                    self.logger.add_error(ErrorType::AssertMismatchedType(
                        data.token.expect("Assert is missing a token."),
                        et,
                    ));
                }
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
                //NodeType::Identifier(token) => {
                // token is identifier
                //}
            }
            NodeType::Return(data) => {
                // token is "return" token
                // idx is expression
            }
            NodeType::Read(data) => {
                // token is "read" token
                // idx is first variable node
            }
            NodeType::Write(data) => {
                // token is "write" token
                // idx is first expression
            }
            NodeType::If(data) => {
                // token is "if" token
                // idx1 is boolean expr
                // idx2 is if statement
                // opt_idx is else statement
            }
            NodeType::While(data) => {
                // token is "while" token
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

    fn check_expression(&mut self, idx: usize) -> SymbolType {
        SymbolType::Undefined
    }

    // ---------------------------------------------------------------------
    // Auxiliary functions
    // ---------------------------------------------------------------------
    pub fn new(tree: &'b mut LcRsTree<NodeType<'a>>, logger: &'b mut Logger<'a>) -> Self {
        Analyzer {
            tree: tree,
            symbols: HashMap::new(),
            function_signatures: HashMap::new(),
            logger: logger,
        }
    }
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

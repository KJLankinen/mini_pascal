use super::data_types::{
    ErrorType, IdxIdx, NodeType, SymbolType, TokenData, TokenIdx, TokenOptIdx,
};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::HashMap;

pub struct Analyzer<'a, 'b> {
    tree: &'b mut LcRsTree<NodeType<'a>>,
    symbols: HashMap<(&'a str, u32), SymbolType>,
    function_signatures: HashMap<&'a str, FunctionSignature<'a>>,
    logger: &'b mut Logger<'a>,
    scope_depth: u32,
    current_return_type: Option<SymbolType>,
}

struct Parameter<'a> {
    is_ref: bool,
    symbol_type: SymbolType,
    id: &'a str,
}

struct FunctionSignature<'a> {
    parameters: Option<Vec<Parameter<'a>>>,
    return_type: Option<SymbolType>,
    token: TokenData<'a>,
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
                // Clear symbols after last subroutine and before main block
                self.symbols.clear();
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

    fn block(&mut self, idx: usize) -> Option<SymbolType> {
        assert!(false, "Block not yet done");
        // Correct type is checked by return. Need to check that those that must return, always
        // will return.
        match self.tree[idx].data {
            NodeType::Block(idx) => {
                // Increment scope so that names can be reused
                self.scope_depth += 1;
                // Idx points to first statement
                self.statement(idx);
                let mut next = idx;
                while let Some(idx) = self.tree[next].right_sibling {
                    self.statement(idx);
                    next = idx;
                }
                assert!(
                    self.scope_depth > 0,
                    "Scope depth should never be zero inside a block."
                );
                self.scope_depth -= 1;
            }
            _ => {
                assert!(false, "Unexpected node.");
            }
        }
        None
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
                        token: token,
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
                                    symbol_type: analyzer.check_type(data.idx),
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
                        fs.return_type = Some(self.check_type(idx));
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
                // Reset the symbol table so it'll contain only values in this scope,
                // including the parameters of the function
                self.symbols.clear();
                let fs = self
                    .function_signatures
                    .get(data.token.expect("Function is missing a token.").value)
                    .expect("Function signature should already be stored in the map.");

                // Set the current return type to the return type of the function
                self.current_return_type = fs.return_type;

                if let Some(params) = &fs.parameters {
                    for param in params {
                        self.symbols
                            .insert((param.id, self.scope_depth), param.symbol_type);
                    }
                }
                self.block(data.idx);
            }
            _ => {
                assert!(false, "Unexpected node.");
            }
        }

        // Reset to None, which is the return type of main block
        self.current_return_type = None;
    }

    // ---------------------------------------------------------------------
    // Statements
    // ---------------------------------------------------------------------
    fn statement(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::Block(idx) => {
                self.block(idx);
            }
            NodeType::Assert(data) => self.assert_statement(&data),
            NodeType::Assignment(data) => self.assign_statement(&data),
            NodeType::Call(data) => {
                self.call_statement(&data);
            }
            NodeType::Declaration(data) => self.declaration_statement(&data),
            NodeType::Return(data) => {
                self.return_statement(&data);
            }
            NodeType::Read(idx) => self.read_statement(idx),
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
            NodeType::RelOp(data) => {
                // token is rel op
                // idx is left add op
                // opt_idx is right add op
            }
            NodeType::AddOp(data) => {
                // token is op
                // idx is mul op
                // opt_idx is add op
            }
            NodeType::MulOp(data) => {
                // token is mulop
                // idx is factor
                // opt_idx is mul op
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

    fn assign_statement(&mut self, data: &IdxIdx) {
        // idx is variable
        // idx2 is expression
        if let NodeType::Variable(variable_data) = self.tree[data.idx].data {
            let token = variable_data.token.expect("Variable is missing a token.");
            let et = self.get_expression_type(data.idx2);
            if let Some(vt) = self.get_variable_type(data.idx) {
                if et != vt {
                    self.logger
                        .add_error(ErrorType::AssignMismatchedType(token, vt, et));
                }
            } else {
                self.logger
                    .add_error(ErrorType::UndeclaredIdentifier(token));
            }
        } else {
            assert!(false, "Unexpected node type.");
        }
    }

    fn assert_statement(&mut self, data: &TokenIdx<'a>) {
        let et = self.get_expression_type(data.idx);
        if SymbolType::Bool != et && SymbolType::Undefined != et {
            self.logger.add_error(ErrorType::AssertMismatchedType(
                data.token.expect("Assert is missing a token."),
                et,
            ));
        }
    }

    fn call_statement(&mut self, data: &TokenOptIdx<'a>) -> Option<SymbolType> {
        // Gather argument types to vector
        let mut argument_types = vec![];
        if let Some(idx) = data.opt_idx {
            argument_types.push(self.get_expression_type(idx));
            let mut next = idx;
            while let Some(idx) = self.tree[next].right_sibling {
                argument_types.push(self.get_expression_type(idx));
                next = idx;
            }
        }

        let token = data.token.expect("Call is missing a token.");
        if let Some(fs) = self.function_signatures.get(token.value) {
            if let Some(params) = &fs.parameters {
                if params.len() == argument_types.len() {
                    for i in 0..params.len() {
                        if params[i].symbol_type != argument_types[i] {
                            self.logger.add_error(ErrorType::MismatchedArgumentTypes(
                                fs.token,
                                params.iter().map(|p| p.symbol_type).collect(),
                                token,
                                argument_types,
                            ));
                            break;
                        }
                    }
                } else if params.len() > argument_types.len() {
                    self.logger.add_error(ErrorType::TooFewArguments(
                        fs.token,
                        params.iter().map(|p| p.symbol_type).collect(),
                        token,
                        argument_types,
                    ));
                } else {
                    self.logger.add_error(ErrorType::TooManyArguments(
                        fs.token,
                        params.iter().map(|p| p.symbol_type).collect(),
                        token,
                        argument_types,
                    ));
                }
            } else {
                if false == argument_types.is_empty() {
                    self.logger.add_error(ErrorType::TooManyArguments(
                        fs.token,
                        vec![],
                        token,
                        argument_types,
                    ));
                }
            }

            fs.return_type
        } else {
            self.logger
                .add_error(ErrorType::UndeclaredIdentifier(token));
            None
        }
    }

    fn declaration_statement(&mut self, data: &IdxIdx) {
        let st = self.check_type(data.idx2);
        let mut next = Some(data.idx);
        while let Some(idx) = next {
            if let NodeType::Identifier(token) = self.tree[idx].data {
                let token = token.expect("Identifier is missing a token.");
                if let Some(_) = self.symbols.get(&(token.value, self.scope_depth)) {
                    self.logger.add_error(ErrorType::Redeclaration(token));
                } else {
                    self.symbols.insert((token.value, self.scope_depth), st);
                }
            } else {
                break;
            }

            next = self.tree[idx].right_sibling;
        }
    }

    fn return_statement(&mut self, data: &TokenOptIdx<'a>) -> Option<SymbolType> {
        let rt = if let Some(idx) = data.opt_idx {
            Some(self.get_expression_type(idx))
        } else {
            None
        };

        if rt != self.current_return_type {
            self.logger.add_error(ErrorType::MismatchedReturnType(
                data.token.expect("Return is missing a token."),
                rt,
                self.current_return_type,
            ));
        }

        rt
    }

    fn read_statement(&mut self, idx: usize) {
        let mut next = Some(idx);
        while let Some(idx) = next {
            if let NodeType::Variable(variable_data) = self.tree[idx].data {
                let token = variable_data.token.expect("Variable is missing a token.");
                if let Some(st) = self.get_variable_type(idx) {
                    // Should we save the variable types somewhere for emitter?
                    // Maybe do some map with (line, col) of first variable as key, and vector of
                    // types as value, which can then be fetched easily
                    match st {
                        SymbolType::ArrayBool(_)
                        | SymbolType::ArrayInt(_)
                        | SymbolType::ArrayReal(_)
                        | SymbolType::ArrayString(_) => {
                            self.logger
                                .add_error(ErrorType::ReadMismatchedType(token, st));
                        }
                        SymbolType::Undefined => {
                            assert!(false, "Variable type should never be undefined.");
                        }
                        _ => {}
                    }
                } else {
                    self.logger
                        .add_error(ErrorType::UndeclaredIdentifier(token));
                }
            } else {
                break;
            }

            next = self.tree[idx].right_sibling;
        }
    }

    // ---------------------------------------------------------------------
    // Type checking
    // ---------------------------------------------------------------------
    fn check_type(&mut self, idx: usize) -> SymbolType {
        match self.tree[idx].data {
            NodeType::VariableType(data) => {
                match data.st {
                    SymbolType::ArrayInt(expr_idx)
                    | SymbolType::ArrayString(expr_idx)
                    | SymbolType::ArrayBool(expr_idx)
                    | SymbolType::ArrayReal(expr_idx) => {
                        let et = self.get_expression_type(expr_idx);
                        if SymbolType::Int != et {
                            self.logger.add_error(ErrorType::IndexTypeMismatch(
                                data.token.expect("Type is missing a token."),
                                et,
                            ));
                        }
                    }
                    SymbolType::Undefined
                    | SymbolType::Int
                    | SymbolType::String
                    | SymbolType::Bool
                    | SymbolType::Real => {}
                };
                data.st
            }
            _ => {
                assert!(false, "Unexpected node type.");
                SymbolType::Undefined
            }
        }
    }

    fn get_expression_type(&mut self, idx: usize) -> SymbolType {
        SymbolType::Undefined
    }

    fn get_variable_type(&mut self, idx: usize) -> Option<SymbolType> {
        match self.tree[idx].data {
            NodeType::Variable(data) => {
                let mut symbol_type = None;
                let token = data.token.expect("Variable is missing a token.");

                // Go upwards in scope and match to the first identifier that is found
                for i in (0..=self.scope_depth).rev() {
                    if let Some(&st) = self.symbols.get(&(token.value, i)) {
                        // If variable contains an array indexing expression
                        // i.e. opt_idx = Some(idx), check that
                        // 1. The expression results in an integer
                        // 2. The identifier is of type array
                        let st = if let Some(expr_idx) = data.opt_idx {
                            let et = self.get_expression_type(expr_idx);
                            if SymbolType::Int != et {
                                self.logger
                                    .add_error(ErrorType::IndexTypeMismatch(token, et));
                            }
                            match st {
                                st @ SymbolType::Int
                                | st @ SymbolType::String
                                | st @ SymbolType::Bool
                                | st @ SymbolType::Real => {
                                    self.logger.add_error(ErrorType::IllegalIndexing(token, st));
                                    st
                                }
                                SymbolType::ArrayInt(_) => SymbolType::Int,
                                SymbolType::ArrayString(_) => SymbolType::String,
                                SymbolType::ArrayBool(_) => SymbolType::Bool,
                                SymbolType::ArrayReal(_) => SymbolType::Real,
                                SymbolType::Undefined => SymbolType::Undefined,
                            }
                        } else {
                            st
                        };

                        assert!(
                            SymbolType::Undefined != st,
                            "Symbol type from symbol table should never be undefined."
                        );
                        symbol_type = Some(st);
                        break;
                    }
                }
                symbol_type
            }
            _ => {
                assert!(false, "Unexpected node type.");
                None
            }
        }
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
            scope_depth: 0,
            current_return_type: None,
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

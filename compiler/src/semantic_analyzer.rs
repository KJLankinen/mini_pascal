use super::data_types::{
    ErrorType, IdxIdx, NodeType, SymbolType, TokenData, TokenIdx, TokenIdxIdx, TokenIdxIdxOptIdx,
    TokenOptIdx,
};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::HashMap;

pub struct Analyzer<'a, 'b> {
    tree: &'b mut LcRsTree<NodeType<'a>>,
    function_signatures: HashMap<&'a str, FunctionSignature<'a>>,
    logger: &'b mut Logger<'a>,
    current_return_type: Option<SymbolType>,
    scope: Scope<'a>,
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
        if let NodeType::Program(data) = self.tree[idx].data {
            if let Some(idx) = data.opt_idx {
                self.subroutines(idx);
            }
            self.block(data.idx);
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn subroutines(&mut self, idx: usize) {
        if let NodeType::Subroutines(idx) = self.tree[idx].data {
            // Process the functions in two passes:
            // First: process all function signatures and add them to map
            let mut next = Some(idx);
            while let Some(idx) = next {
                self.function_signature(idx);
                next = self.tree[idx].right_sibling;
            }

            // Second: process the blocks of the functions
            // This way (mutually) recursive functions calls can be matched to known signatures.
            let mut next = Some(idx);
            while let Some(idx) = next {
                self.function_block(idx);
                next = self.tree[idx].right_sibling;
            }
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn block(&mut self, idx: usize) {
        if let NodeType::Block(idx) = self.tree[idx].data {
            self.scope.step_in();
            let mut next = Some(idx);
            while let Some(idx) = next {
                self.statement(idx);
                next = self.tree[idx].right_sibling;
            }
            self.scope.step_out();
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn function_signature(&mut self, idx: usize) {
        if let NodeType::Function(data) = self.tree[idx].data {
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
                    let mut param_vec = Vec::new();
                    if let NodeType::ParamList(idx) = self.tree[idx].data {
                        let mut next = Some(idx);
                        while let Some(idx) = next {
                            if let NodeType::Parameter(data) = self.tree[idx].data {
                                param_vec.push(Parameter {
                                    is_ref: data.b,
                                    symbol_type: self.check_type(data.idx),
                                    id: data
                                        .token
                                        .expect("Parameter has no identifier token.")
                                        .value,
                                });
                            } else {
                                assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
                            }
                            next = self.tree[idx].right_sibling;
                        }
                    } else {
                        assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
                    }
                    fs.parameters = Some(param_vec);
                }

                // Handle return type
                if let Some(idx) = data.opt_idx2 {
                    fs.return_type = Some(self.check_type(idx));
                }

                self.function_signatures.insert(token.value, fs);
            }
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn function_block(&mut self, idx: usize) {
        if let NodeType::Function(data) = self.tree[idx].data {
            let token = data.token.expect("Function is missing a token.");

            // Set return type
            self.current_return_type = self
                .function_signatures
                .get(token.value)
                .expect("Function signature should already be stored in the map.")
                .return_type;

            if self.current_return_type.is_some() {
                // Make a preliminary pass that makes sure the function returns
                // at least something from every possible branch.
                // The correctness of the return type is checked during actual analysis
                if false == self.block_returns(data.idx) {
                    self.logger
                        .add_error(ErrorType::MissingReturnStatements(token));
                }
            }

            // Step into new scope and declare function parameters inside the scope
            self.scope.step_in();
            if let Some(params) = &self
                .function_signatures
                .get(token.value)
                .expect("Function signature should already be stored in the map.")
                .parameters
            {
                for param in params {
                    self.scope.insert(param.id, param.symbol_type);
                }
            }

            // Analyze the function block
            self.block(data.idx);

            // Step out and reset return type
            self.scope.step_out();
            self.current_return_type = None;
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    // ---------------------------------------------------------------------
    // Statements
    // ---------------------------------------------------------------------
    fn statement(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::Block(_) => self.block(idx),
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
            NodeType::Write(idx) => self.write_statement(idx),
            NodeType::If(data) => self.if_statement(&data),
            NodeType::While(data) => self.while_statement(&data),
            NodeType::RelOp(_) => {
                // token is rel op
                // idx is left add op
                // opt_idx is right add op
            }
            NodeType::AddOp(_) => {
                // token is op
                // idx is mul op
                // opt_idx is add op
            }
            NodeType::MulOp(_) => {
                // token is mulop
                // idx is factor
                // opt_idx is mul op
            }
            NodeType::Factor(_) => {
                // token is operator or None for first factor
                // idx is the concrete factor node
            }
            NodeType::Variable(_) => {
                // token is identifier
                // opt_idx is possible array indexing expression
            }
            NodeType::Literal(_) => {
                // token is the literal data
            }
            NodeType::Not(_) => {
                // token is the not operator
                // idx is the factor
            }
            NodeType::ArraySize(_) => {
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
        if let NodeType::Variable(variable_data) = self.tree[data.idx].data {
            let token = variable_data.token.expect("Variable is missing a token.");
            let et = self.get_expression_type(data.idx2);
            if let Some(vt) = self.get_variable_type(data.idx) {
                if et != vt && SymbolType::Undefined != et {
                    self.logger
                        .add_error(ErrorType::AssignMismatchedType(token, vt, et));
                }
            } else {
                self.logger
                    .add_error(ErrorType::UndeclaredIdentifier(token));
            }
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[data.idx]);
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
        let mut argument_types = vec![];
        let mut next = data.opt_idx;
        while let Some(idx) = next {
            argument_types.push(self.get_expression_type(idx));
            next = self.tree[idx].right_sibling;
        }

        let token = data.token.expect("Call is missing a token.");
        if let Some(fs) = self.function_signatures.get(token.value) {
            if let Some(params) = &fs.parameters {
                if params.len() == argument_types.len() {
                    for i in 0..params.len() {
                        if params[i].symbol_type != argument_types[i]
                            && SymbolType::Undefined != argument_types[i]
                        {
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
                if let Some(_) = self.scope.get(token.value) {
                    self.logger.add_error(ErrorType::Redeclaration(token));
                } else {
                    self.scope.insert(token.value, st);
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

        if self.current_return_type != rt && Some(SymbolType::Undefined) != rt {
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

    fn write_statement(&mut self, idx: usize) {
        let mut next = Some(idx);
        while let Some(idx) = next {
            // Should these be stored somewhere or just check here?
            self.get_expression_type(idx);
            next = self.tree[idx].right_sibling;
        }
    }

    fn if_statement(&mut self, data: &TokenIdxIdxOptIdx<'a>) {
        let et = self.get_expression_type(data.idx);
        if SymbolType::Bool != et && SymbolType::Undefined != et {
            self.logger.add_error(ErrorType::ExprTypeMismatch(
                data.token.expect("If is missing a token."),
                SymbolType::Bool,
                et,
            ));
        }

        self.scope.step_in();
        self.statement(data.idx2);
        self.scope.step_out();
        if let Some(idx) = data.opt_idx {
            self.scope.step_in();
            self.statement(idx);
            self.scope.step_out();
        }
    }

    fn while_statement(&mut self, data: &TokenIdxIdx<'a>) {
        let et = self.get_expression_type(data.idx);
        if SymbolType::Bool != et && SymbolType::Undefined != et {
            self.logger.add_error(ErrorType::ExprTypeMismatch(
                data.token.expect("While is missing a token."),
                SymbolType::Bool,
                et,
            ));
        }

        self.scope.step_in();
        self.statement(data.idx2);
        self.scope.step_out();
    }

    // ---------------------------------------------------------------------
    // Type checking
    // ---------------------------------------------------------------------
    fn check_type(&mut self, idx: usize) -> SymbolType {
        if let NodeType::VariableType(data) = self.tree[idx].data {
            match data.st {
                SymbolType::ArrayInt(expr_idx)
                | SymbolType::ArrayString(expr_idx)
                | SymbolType::ArrayBool(expr_idx)
                | SymbolType::ArrayReal(expr_idx) => {
                    let et = self.get_expression_type(expr_idx);
                    if SymbolType::Int != et && SymbolType::Undefined != et {
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
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
            SymbolType::Undefined
        }
    }

    fn get_expression_type(&mut self, _idx: usize) -> SymbolType {
        SymbolType::Undefined
    }

    fn get_variable_type(&mut self, idx: usize) -> Option<SymbolType> {
        if let NodeType::Variable(data) = self.tree[idx].data {
            let mut symbol_type = None;
            let token = data.token.expect("Variable is missing a token.");

            if let Some(&st) = self.scope.find(token.value) {
                // If variable contains an array indexing expression
                // i.e. opt_idx = Some(idx), check that
                // 1. The expression results in an integer
                // 2. The identifier is of type array
                let st = if let Some(expr_idx) = data.opt_idx {
                    let et = self.get_expression_type(expr_idx);
                    if SymbolType::Int != et && SymbolType::Undefined != et {
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
            }
            symbol_type
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
            None
        }
    }

    // ---------------------------------------------------------------------
    // Auxiliary functions
    // ---------------------------------------------------------------------
    pub fn new(tree: &'b mut LcRsTree<NodeType<'a>>, logger: &'b mut Logger<'a>) -> Self {
        Analyzer {
            tree: tree,
            function_signatures: HashMap::new(),
            logger: logger,
            current_return_type: None,
            scope: Scope::new(),
        }
    }

    fn block_returns(&mut self, idx: usize) -> bool {
        // This function checks if a block returns something.
        // Block returns, if at least one of the following holds:
        // - one of the statements of the block is a return statement
        // - the block contains an unconditional block that returns
        // - there is an if-then-else clause, where both if and else returns
        fn if_returns<'a, 'b>(analyzer: &mut Analyzer<'a, 'b>, idx: usize) -> bool {
            // This function checks whether or not both the if and the else clauses return
            if let NodeType::If(data) = analyzer.tree[idx].data {
                // There must exist an else clause for both to return
                if let Some(else_idx) = data.opt_idx {
                    // Check whether the if returns
                    if match analyzer.tree[data.idx2].data {
                        NodeType::Return(_) => true,
                        NodeType::Block(_) => analyzer.block_returns(data.idx2),
                        NodeType::If(_) => if_returns(analyzer, data.idx2),
                        _ => false,
                    } {
                        // Check whether the else returns
                        match analyzer.tree[else_idx].data {
                            NodeType::Return(_) => true,
                            NodeType::Block(_) => analyzer.block_returns(else_idx),
                            NodeType::If(_) => if_returns(analyzer, else_idx),
                            _ => false,
                        }
                    } else {
                        // If didn't return
                        false
                    }
                } else {
                    // There is no else
                    false
                }
            } else {
                assert!(false, "Unexpected node {:#?}.", analyzer.tree[idx]);
                false
            }
        }

        let mut every_branch_returns = false;
        if let NodeType::Block(idx) = self.tree[idx].data {
            let mut next = Some(idx);
            while let Some(idx) = next {
                every_branch_returns = match self.tree[idx].data {
                    NodeType::Return(_) => true,
                    NodeType::If(_) => if_returns(self, idx),
                    NodeType::Block(_) => self.block_returns(idx),
                    _ => false,
                };

                if every_branch_returns {
                    break;
                }
                next = self.tree[idx].right_sibling;
            }
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
        every_branch_returns
    }
}

// ---------------------------------------------------------------------
// Auxiliary structs used during the semantic analysis
// ---------------------------------------------------------------------
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

pub struct Scope<'a> {
    depth: usize,
    pub symbols: HashMap<usize, HashMap<&'a str, SymbolType>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Scope {
            depth: 0,
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: &'a str, st: SymbolType) {
        self.symbols
            .get_mut(&self.depth)
            .expect("Scope should have a map at current depth.")
            .insert(key, st);
    }

    pub fn get(&self, key: &'a str) -> Option<&SymbolType> {
        self.symbols
            .get(&self.depth)
            .expect("Scope should have a map at current depth.")
            .get(key)
    }

    pub fn find(&self, key: &'a str) -> Option<&SymbolType> {
        for i in (0..=self.depth).rev() {
            if let Some(hm) = self.symbols.get(&i) {
                if let Some(st) = hm.get(key) {
                    return Some(st);
                }
            }
        }
        None
    }

    pub fn step_in(&mut self) {
        self.depth += 1;
        if self.symbols.get(&self.depth).is_none() {
            self.symbols.insert(self.depth, HashMap::new());
        }
    }

    pub fn step_out(&mut self) {
        if let Some(hm) = self.symbols.get_mut(&self.depth) {
            hm.clear();
        }
        assert!(
            self.depth > 0,
            "Scope depth should never be 0 before stepping out."
        );
        self.depth -= 1;
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

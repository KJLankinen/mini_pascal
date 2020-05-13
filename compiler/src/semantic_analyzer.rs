use super::data_types::{
    ErrorType, IdxIdx, NodeType, SymbolType, TokenData, TokenIdx, TokenIdxIdx, TokenIdxIdxOptIdx,
    TokenOptIdx, TokenType,
};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use super::symbol_table::SymbolTable;
use std::collections::HashMap;

pub struct Analyzer<'a, 'b> {
    tree: &'b mut LcRsTree<NodeType<'a>>,
    function_signatures: HashMap<&'a str, FunctionSignature<'a>>,
    logger: &'b mut Logger<'a>,
    current_return_type: Option<SymbolType>,
    symbol_table: SymbolTable<'a>,
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
            self.symbol_table.step_in();
            let mut next = Some(idx);
            while let Some(idx) = next {
                self.statement(idx);
                next = self.tree[idx].right_sibling;
            }
            self.symbol_table.step_out();
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
                                    _is_ref: data.b,
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

            // Step into new symbol_table and declare function parameters inside the scope
            self.symbol_table.step_in();
            if let Some(params) = &self
                .function_signatures
                .get(token.value)
                .expect("Function signature should already be stored in the map.")
                .parameters
            {
                for param in params {
                    self.symbol_table.insert(param.id, param.symbol_type);
                }
            }

            // Analyze the function block
            self.block(data.idx);

            // Step out and reset return type
            self.symbol_table.step_out();
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
            NodeType::Return(data) => self.return_statement(&data),
            NodeType::Read(idx) => self.read_statement(idx),
            NodeType::Write(idx) => self.write_statement(idx),
            NodeType::If(data) => self.if_statement(&data),
            NodeType::While(data) => self.while_statement(&data),
            _ => assert!(false, "Unexpected node {:#?}.", self.tree[idx]),
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
                if let Some(_) = self.symbol_table.get(token.value) {
                    self.logger.add_error(ErrorType::Redeclaration(token));
                } else {
                    self.symbol_table.insert(token.value, st);
                }
            } else {
                break;
            }

            next = self.tree[idx].right_sibling;
        }
    }

    fn return_statement(&mut self, data: &TokenOptIdx<'a>) {
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

        self.symbol_table.step_in();
        self.statement(data.idx2);
        self.symbol_table.step_out();
        if let Some(idx) = data.opt_idx {
            self.symbol_table.step_in();
            self.statement(idx);
            self.symbol_table.step_out();
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

        self.symbol_table.step_in();
        self.statement(data.idx2);
        self.symbol_table.step_out();
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

    fn get_variable_type(&mut self, idx: usize) -> Option<SymbolType> {
        if let NodeType::Variable(data) = self.tree[idx].data {
            let mut symbol_type = None;
            let token = data.token.expect("Variable is missing a token.");

            if let Some(&st) = self.symbol_table.find(token.value) {
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

    fn match_operands(
        &mut self,
        type1: SymbolType,
        type2: SymbolType,
        accepted_types: &Vec<SymbolType>,
        token: &TokenData<'a>,
    ) -> SymbolType {
        // This is a helper function that figures out what is the return type, given two types as
        // input and a vector of allowed types for the operator, e.g. [ST::Int, ST::Bool, ...]
        let type1_accepted = accepted_types.iter().find(|&&t| t == type1).is_some();
        let type2_accepted = accepted_types.iter().find(|&&t| t == type2).is_some();
        let mut log_error = false;

        let rt = if type1 == type2 {
            if SymbolType::Undefined == type1 || type1_accepted {
                type1
            } else {
                log_error = true;
                SymbolType::Undefined
            }
        } else if SymbolType::Undefined == type1 || SymbolType::Undefined == type2 {
            let (defined_type, defined_type_accepted) = if SymbolType::Undefined == type1 {
                (type2, type2_accepted)
            } else {
                (type1, type1_accepted)
            };

            if defined_type_accepted {
                defined_type
            } else {
                log_error = true;
                SymbolType::Undefined
            }
        } else {
            if type1_accepted && type2_accepted {
                log_error = true;
                SymbolType::Undefined
            } else if type1_accepted {
                log_error = true;
                type1
            } else if type2_accepted {
                log_error = true;
                type2
            } else {
                log_error = true;
                SymbolType::Undefined
            }
        };

        if log_error {
            self.logger
                .add_error(ErrorType::IllegalOperation(*token, vec![type1, type2]));
        }

        rt
    }

    fn get_expression_type(&mut self, idx: usize) -> SymbolType {
        match self.tree[idx].data {
            NodeType::RelOp(data) => {
                let token = data.token.expect("Relation operator is missing a token.");
                let type1 = self.get_expression_type(data.idx);
                let type2 = self.get_expression_type(data.idx2);

                match token.token_type {
                    TokenType::OperatorEqual
                    | TokenType::OperatorNotEqual
                    | TokenType::OperatorGreater
                    | TokenType::OperatorGreaterEqual
                    | TokenType::OperatorLess
                    | TokenType::OperatorLessEqual => {
                        if SymbolType::Undefined
                            == self.match_operands(
                                type1,
                                type2,
                                &vec![
                                    SymbolType::Bool,
                                    SymbolType::Int,
                                    SymbolType::Real,
                                    SymbolType::String,
                                ],
                                &token,
                            )
                        {
                            SymbolType::Undefined
                        } else {
                            SymbolType::Bool
                        }
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        SymbolType::Undefined
                    }
                }
            }
            NodeType::AddOp(data) => {
                let token = data.token.expect("Add operator is missing a token.");
                if let Some(idx) = data.opt_idx {
                    let type1 = self.get_expression_type(data.idx);
                    let type2 = self.get_expression_type(idx);

                    match token.token_type {
                        TokenType::OperatorPlus => self.match_operands(
                            type1,
                            type2,
                            &vec![SymbolType::Int, SymbolType::Real, SymbolType::String],
                            &token,
                        ),
                        TokenType::OperatorMinus => self.match_operands(
                            type1,
                            type2,
                            &vec![SymbolType::Int, SymbolType::Real],
                            &token,
                        ),
                        TokenType::OperatorOr => {
                            self.match_operands(type1, type2, &vec![SymbolType::Bool], &token)
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                            SymbolType::Undefined
                        }
                    }
                } else {
                    // Operator is sign, and idx is add_op
                    let type1 = self.get_expression_type(data.idx);
                    match type1 {
                        SymbolType::Int | SymbolType::Real => type1,
                        _ => {
                            self.logger
                                .add_error(ErrorType::IllegalOperation(token, vec![type1]));
                            SymbolType::Undefined
                        }
                    }
                }
            }
            NodeType::MulOp(data) => {
                let token = data.token.expect("Multiply operator is missing a token.");
                let type1 = self.get_expression_type(data.idx);
                let type2 = self.get_expression_type(data.idx2);

                match token.token_type {
                    TokenType::OperatorMultiply | TokenType::OperatorDivide => self.match_operands(
                        type1,
                        type2,
                        &vec![SymbolType::Int, SymbolType::Real],
                        &token,
                    ),
                    TokenType::OperatorModulo => {
                        self.match_operands(type1, type2, &vec![SymbolType::Int], &token)
                    }
                    TokenType::OperatorAnd => {
                        self.match_operands(type1, type2, &vec![SymbolType::Bool], &token)
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        SymbolType::Undefined
                    }
                }
            }
            NodeType::Variable(data) => {
                if let Some(vt) = self.get_variable_type(idx) {
                    vt
                } else {
                    self.logger.add_error(ErrorType::UndeclaredIdentifier(
                        data.token.expect("Variable is missing a token."),
                    ));
                    SymbolType::Undefined
                }
            }
            NodeType::Literal(token) => {
                match token.expect("Literal is missing a token.").token_type {
                    TokenType::LiteralBoolean => SymbolType::Bool,
                    TokenType::LiteralInt => SymbolType::Int,
                    TokenType::LiteralReal => SymbolType::Real,
                    TokenType::LiteralString => SymbolType::String,
                    _ => SymbolType::Undefined,
                }
            }
            NodeType::Not(data) => {
                let ft = self.get_expression_type(data.idx);
                match ft {
                    SymbolType::Bool | SymbolType::Undefined => {}
                    _ => {
                        self.logger.add_error(ErrorType::ExprTypeMismatch(
                            data.token.expect("Array size operator is missing a token."),
                            SymbolType::Bool,
                            ft,
                        ));
                    }
                }
                SymbolType::Bool
            }
            NodeType::ArraySize(data) => {
                let ft = self.get_expression_type(data.idx);
                match ft {
                    SymbolType::Bool | SymbolType::Int | SymbolType::Real | SymbolType::String => {
                        self.logger.add_error(ErrorType::ArraySizeTypeMismatch(
                            data.token.expect("Array size operator is missing a token."),
                            ft,
                        ));
                    }
                    _ => {}
                }
                SymbolType::Int
            }
            NodeType::Call(data) => self
                .call_statement(&data)
                .unwrap_or_else(|| SymbolType::Undefined),
            _ => {
                assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
                SymbolType::Undefined
            }
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
            symbol_table: SymbolTable::new(),
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
    _is_ref: bool,
    symbol_type: SymbolType,
    id: &'a str,
}

struct FunctionSignature<'a> {
    parameters: Option<Vec<Parameter<'a>>>,
    return_type: Option<SymbolType>,
    token: TokenData<'a>,
}

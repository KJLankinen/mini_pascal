use super::data_types::{
    IdxIdx, NodeType, SymbolType, TokenIdx, TokenIdxIdx, TokenIdxIdxOptIdx, TokenOptIdx,
    TokenSymbolIdxIdxOptIdx, TokenType,
};
use super::lcrs_tree::LcRsTree;
use super::symbol_table::SymbolTable;

pub struct Stacker<'a, 'b> {
    tree: &'b LcRsTree<NodeType<'a>>,
    symbol_table: &'b mut SymbolTable<'a>,
    fname: Option<&'a str>,
}

impl<'a, 'b> Stacker<'a, 'b> {
    pub fn stack_ir(&mut self) {
        self.program(0);
    }

    // ---------------------------------------------------------------------
    // Functions that handle emitting instructions
    // ---------------------------------------------------------------------
    fn program(&mut self, idx: usize) {
        if let NodeType::Program(data) = self.tree[idx].data {
            self.fname = Some(data.token.expect("Program is missing a token.").value);
            if let Some(idx) = data.opt_idx {
                self.subroutines(idx);
            }
            self.block(data.idx);
            self.fname = None;
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn subroutines(&mut self, idx: usize) {
        if let NodeType::Subroutines(idx) = self.tree[idx].data {
            let mut next = Some(idx);
            while let Some(idx) = next {
                self.function(idx);
                next = self.tree[idx].right_sibling;
            }
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn block(&mut self, idx: usize) {
        if let NodeType::Block(idx) = self.tree[idx].data {
            let mut next = Some(idx);
            while let Some(idx) = next {
                self.statement(idx);
                next = self.tree[idx].right_sibling;
            }
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn function(&mut self, idx: usize) {
        if let NodeType::Function(data) = self.tree[idx].data {
            self.fname = Some(data.token.expect("Function is missing a token.").value);
            self.fname = None;
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
            NodeType::Call(data) => self.call_statement(&data),
            NodeType::Declaration(_) => {}
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
            self.expression(data.idx2);
            let _local_idx = self.get_variable_local_idx(&variable_data);
            if let Some(arr_idx) = variable_data.opt_idx {
                // emit local_idx
                self.expression(arr_idx);
            // emit call array_assign(value, addr, idx)
            } else {
                // emit local set to local_idx
            }
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[data.idx]);
        }
    }

    fn assert_statement(&mut self, _data: &TokenIdx<'a>) {}

    fn call_statement(&mut self, _data: &TokenOptIdx<'a>) {}

    fn return_statement(&mut self, _data: &TokenOptIdx<'a>) {}

    fn read_statement(&mut self, _idx: usize) {}

    fn write_statement(&mut self, _idx: usize) {}

    fn if_statement(&mut self, _data: &TokenIdxIdxOptIdx<'a>) {}

    fn while_statement(&mut self, _data: &TokenIdxIdx<'a>) {}

    fn expression(&mut self, idx: usize) {
        match self.tree[idx].data {
            NodeType::RelOp(data) => {
                let token = data.token.expect("Relation operator is missing a token.");
                let st = data.st;
                self.expression(data.idx);
                self.expression(data.idx2);

                match st {
                    SymbolType::Bool => match token.token_type {
                        TokenType::OperatorEqual => {
                            // emit i32 eq
                        }
                        TokenType::OperatorNotEqual => {
                            // emit i32 neq
                        }
                        TokenType::OperatorGreater => {
                            // emit i32 gre
                        }
                        TokenType::OperatorGreaterEqual => {
                            // emit i32 geq
                        }
                        TokenType::OperatorLess => {
                            // emit i32 less
                        }
                        TokenType::OperatorLessEqual => {
                            // emit i32 leq
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    SymbolType::Int => match token.token_type {
                        TokenType::OperatorEqual => {
                            // emit i32 eq
                        }
                        TokenType::OperatorNotEqual => {
                            // emit i32 neq
                        }
                        TokenType::OperatorGreater => {
                            // emit i32 gre
                        }
                        TokenType::OperatorGreaterEqual => {
                            // emit i32 geq
                        }
                        TokenType::OperatorLess => {
                            // emit i32 less
                        }
                        TokenType::OperatorLessEqual => {
                            // emit i32 leq
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    SymbolType::Real => match token.token_type {
                        TokenType::OperatorEqual => {
                            // emit f32 eq
                        }
                        TokenType::OperatorNotEqual => {
                            // emit f32 neq
                        }
                        TokenType::OperatorGreater => {
                            // emit f32 grea
                        }
                        TokenType::OperatorGreaterEqual => {
                            // emit f32 geq
                        }
                        TokenType::OperatorLess => {
                            // emit f32 less
                        }
                        TokenType::OperatorLessEqual => {
                            // emit f32 leq
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    SymbolType::String => match token.token_type {
                        TokenType::OperatorEqual => {
                            // emit call string_eq
                        }
                        TokenType::OperatorNotEqual => {
                            // emit call string_neq
                        }
                        TokenType::OperatorGreater => {
                            // emit call string_grea
                        }
                        TokenType::OperatorGreaterEqual => {
                            // emit call string_geq
                        }
                        TokenType::OperatorLess => {
                            // emit call string_less
                        }
                        TokenType::OperatorLessEqual => {
                            // emit call string_leq
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    _ => {
                        assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                    }
                }
            }
            NodeType::AddOp(data) => {
                let token = data.token.expect("Add operator is missing a token.");
                let st = data.st;

                if let Some(idx) = data.opt_idx {
                    self.expression(data.idx);
                    self.expression(idx);

                    match token.token_type {
                        TokenType::OperatorPlus => {
                            match st {
                                SymbolType::Int => {
                                    // emit i32 add
                                }
                                SymbolType::Real => {
                                    // emit f32 add
                                }
                                SymbolType::String => {
                                    // emit call $string_concatenate
                                }
                                _ => {
                                    assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                                }
                            }
                        }
                        TokenType::OperatorMinus => {
                            match st {
                                SymbolType::Int => {
                                    // emit i32 sub
                                }
                                SymbolType::Real => {
                                    // emit f32 sub
                                }
                                _ => {
                                    assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                                }
                            }
                        }
                        TokenType::OperatorOr => {
                            // emit i32 or
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    }
                } else {
                    // Operator is sign, and idx is add_op
                    self.expression(data.idx);

                    match token.token_type {
                        TokenType::OperatorPlus => {}
                        TokenType::OperatorMinus => {
                            match st {
                                SymbolType::Int => {
                                    // emit i32 -1
                                    // emit i32 mul
                                }
                                SymbolType::Real => {
                                    // emit f32 -1
                                    // emut f32 mul
                                }
                                _ => {
                                    assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                                }
                            }
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    }
                }
            }
            NodeType::MulOp(data) => {
                let token = data.token.expect("Multiply operator is missing a token.");
                let st = data.st;
                self.expression(data.idx);
                self.expression(data.idx2);

                match token.token_type {
                    TokenType::OperatorMultiply => {
                        match st {
                            SymbolType::Int => {
                                // emit i32 mul
                            }
                            SymbolType::Real => {
                                // emit f32 mul
                            }
                            _ => {
                                assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                            }
                        }
                    }
                    TokenType::OperatorDivide => {
                        match st {
                            SymbolType::Int => {
                                // emit i32 div
                            }
                            SymbolType::Real => {
                                // emit f32 div
                            }
                            _ => {
                                assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                            }
                        }
                    }
                    TokenType::OperatorModulo => {
                        // emit i32 mod
                    }
                    TokenType::OperatorAnd => {
                        // emit i32 and
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                    }
                }
            }
            NodeType::Variable(data) => {
                let _local_idx = self.get_variable_local_idx(&data);
                if let Some(arr_idx) = data.opt_idx {
                    // emit local_idx
                    self.expression(arr_idx);
                // emit call array_access(addr, idx)
                } else {
                    // emit local get with local_idx
                }
            }
            NodeType::Literal(data) => {
                let token = data.token.unwrap();
                match token.token_type {
                    TokenType::LiteralBoolean => {
                        if "true" == token.value {
                            // emit i32 1
                        } else {
                            // emit i32 0
                        }
                    }
                    TokenType::LiteralInt => {
                        let _literal = token
                            .value
                            .parse::<i32>()
                            .expect("Literal int str should be possible to parse to an integer.");
                        // emit i32 const
                    }
                    TokenType::LiteralReal => {
                        let _literal = token
                            .value
                            .parse::<f32>()
                            .expect("Literal real str should be possible to parse to a float.");
                        // emit f32 const
                    }
                    TokenType::LiteralString => {
                        // The idx is the "ranking" of the string literal. In other words, if this
                        // is the fifth literal string that is found in the program, idx is 5.
                        // Literal strings are stored consecutively in memory and the accessing
                        // functions uses this idx to find the address of the string literal.
                        let _idx = data
                            .opt_idx
                            .expect("Literal string should have some opt_idx.");
                        // emit _idx as i32 const
                        // emit call $get_string_literal(idx)
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", data);
                    }
                }
            }
            NodeType::Not(data) => {
                self.expression(data.idx);
                // emit eqz
            }
            NodeType::ArraySize(data) => {
                self.expression(data.idx);
                // emit call $array_size(addr)
            }
            NodeType::Call(data) => self.call_statement(&data),
            _ => {
                assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
            }
        }
    }

    fn get_variable_local_idx(&mut self, data: &TokenSymbolIdxIdxOptIdx<'a>) -> usize {
        data.idx
            + self
                .symbol_table
                .get_variable_index(
                    self.fname
                        .expect("Function must have a name at this point."),
                    data.st,
                    data.idx2,
                )
                .expect("Symbol table should contain a variable index.")
    }

    pub fn new(tree: &'b LcRsTree<NodeType<'a>>, symbol_table: &'b mut SymbolTable<'a>) -> Self {
        Stacker {
            tree: tree,
            symbol_table: symbol_table,
            fname: None,
        }
    }
}

use super::data_types::{
    IdxIdx, NodeType, SymbolType, TokenIdx, TokenIdxIdx, TokenIdxIdxOptIdx, TokenOptIdx, TokenType,
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
        if let NodeType::Function(_data) = self.tree[idx].data {
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
        if let NodeType::Variable(_variable_data) = self.tree[data.idx].data {
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
                let _count = data.idx
                    + self
                        .symbol_table
                        .get_variable_index(
                            self.fname
                                .expect("Function must have a name at this point."),
                            data.st,
                            data.idx2,
                        )
                        .expect("Symbol table should contain a variable index.");

                match data.st {
                    SymbolType::Bool | SymbolType::Int | SymbolType::Real | SymbolType::String => {
                        // emit local get with count
                    }
                    SymbolType::ArrayBool(_)
                    | SymbolType::ArrayInt(_)
                    | SymbolType::ArrayReal(_)
                    | SymbolType::ArrayString(_) => {
                        // emit call $check_array or smthn with count
                    }
                    _ => {
                        assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                    }
                }
            }
            NodeType::Literal(_token) => {
                // convert literal to the value
            }
            NodeType::Not(data) => {
                self.expression(data.idx);
                // emit eqz
            }
            NodeType::ArraySize(data) => {
                self.expression(data.idx);
                // emit call $array_size
            }
            NodeType::Call(data) => self.call_statement(&data),
            _ => {
                assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
            }
        }
    }

    pub fn new(tree: &'b LcRsTree<NodeType<'a>>, symbol_table: &'b mut SymbolTable<'a>) -> Self {
        Stacker {
            tree: tree,
            symbol_table: symbol_table,
            fname: None,
        }
    }
}

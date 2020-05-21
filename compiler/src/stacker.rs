use super::data_types::Instruction as Instr;
use super::data_types::NodeType as NT;
use super::data_types::SymbolType as ST;
use super::data_types::TokenType as TT;
use super::data_types::WasmType as WT;
use super::data_types::{
    IdxIdx, TokenIdx, TokenIdxIdx, TokenIdxIdxOptIdx, TokenIdxOptIdx, TokenOptIdx, VariableData,
};
use super::lcrs_tree::LcRsTree;
use super::symbol_table::SymbolTable;
use std::collections::{HashMap, HashSet};
use std::mem;

pub struct Stacker<'a, 'b> {
    tree: &'b LcRsTree<NT<'a>>,
    symbol_table: &'b mut SymbolTable<'a>,
    fname: Option<&'a str>,
    instructions: &'b mut Vec<Instr<'a>>,
    library_functions: HashSet<&'a str>,
}

macro_rules! emit {
    ($stacker:expr, $item:expr) => {
        if let Instr::LibFunc(name) = $item {
            $stacker.library_functions.insert(name);
        }
        $stacker.instructions.push($item)
    };
}

impl<'a, 'b> Stacker<'a, 'b> {
    pub fn stack_ir(&mut self) {
        self.program(0);
        mem::swap(
            &mut self.symbol_table.library_functions,
            &mut self.library_functions,
        );
    }

    // ---------------------------------------------------------------------
    // Functions that handle emitting instructions
    // ---------------------------------------------------------------------
    fn program(&mut self, idx: usize) {
        if let NT::Program(data) = self.tree[idx].data {
            let token = data.token.expect("Program is missing a token.");
            let (it, ft) = self
                .symbol_table
                .get_local_variable_totals(token.value)
                .expect("Function should have totals.");

            emit!(self, Instr::ProgramBegin(token.value));
            emit!(self, Instr::Imports);
            if 0 < self.symbol_table.borrow_string_literals().len() {
                emit!(self, Instr::DataSegment);
            }

            if let Some(idx) = data.opt_idx {
                self.subroutines(idx);
            }
            emit!(self, Instr::FunctionBegin("_start"));
            self.fname = Some(token.value);

            for _ in 0..it {
                emit!(self, Instr::Local(None, WT::I32(None)));
            }

            for _ in 0..ft {
                emit!(self, Instr::Local(None, WT::F32(None)));
            }

            let ref_count = self.symbol_table.get_function_ref_count(token.value);
            if let Some(nr) = ref_count {
                // nr tells the maximum number of arguments that are references across all the
                // different function calls that are issued from this function.
                // The local variables that are passed as references are stored in linear
                // memory, the base address of which is stored in the local special variable "refs".
                if 0 < nr {
                    emit!(self, Instr::Local(Some("refs"), WT::I32(None)));

                    // Must be done as the first possible instruction in "_start"
                    emit!(self, Instr::GlobalPointers);

                    emit!(self, Instr::Const(WT::I32(Some((nr * 4) as i32)))); // 4 bytes per variable
                    emit!(self, Instr::LibFunc("allocate"));
                    emit!(self, Instr::SetLocal(WT::Str("refs")));
                }
            } else {
                // Must be done as the first possible instruction in "_start"
                emit!(self, Instr::GlobalPointers);
            }

            emit!(self, Instr::BlockBegin(Some("FB")));
            self.block(data.idx);
            emit!(self, Instr::End);

            self.fname = None;
            emit!(self, Instr::End);
            emit!(self, Instr::End);
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn subroutines(&mut self, idx: usize) {
        if let NT::Subroutines(idx) = self.tree[idx].data {
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
        if let NT::Block(idx) = self.tree[idx].data {
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
        if let NT::Function(data) = self.tree[idx].data {
            let token = data.token.expect("Function is missing a token.");
            self.fname = Some(token.value);
            let (it, ft) = self
                .symbol_table
                .get_local_variable_totals(token.value)
                .expect("Function should have totals.");

            emit!(self, Instr::FunctionBegin(token.value));
            for param in &self
                .symbol_table
                .get_function_signature(token.value)
                .expect("Function should have a signature.")
                .parameters
            {
                let wt = if ST::Real == param.symbol_type && false == param.is_ref {
                    WT::F32(None)
                } else {
                    WT::I32(None)
                };
                emit!(self, Instr::Param(wt));
            }

            let rwt = self
                .symbol_table
                .get_function_signature(token.value)
                .expect("Function should have a signature.")
                .return_type
                .map(|rt| {
                    if ST::Real == rt {
                        WT::F32(None)
                    } else {
                        WT::I32(None)
                    }
                });

            if let Some(wt) = rwt {
                emit!(self, Instr::Result(wt));
            }

            for _ in 0..it {
                emit!(self, Instr::Local(None, WT::I32(None)));
            }

            for _ in 0..ft {
                emit!(self, Instr::Local(None, WT::F32(None)));
            }

            if let Some(wt) = rwt {
                emit!(self, Instr::Local(Some("rv"), wt));
            }

            let ref_count = self.symbol_table.get_function_ref_count(token.value);
            if let Some(nr) = ref_count {
                // nr tells the maximum number of arguments that are references across all the
                // different function calls that are issued from this function.
                // The local variables that are passed as references are stored in linear
                // memory, the base address of which is stored in the local special variable "refs".
                if 0 < nr {
                    emit!(self, Instr::Local(Some("refs"), WT::I32(None)));
                    emit!(self, Instr::Const(WT::I32(Some((nr * 4) as i32)))); // 4 bytes per variable
                    emit!(self, Instr::LibFunc("allocate"));
                    emit!(self, Instr::SetLocal(WT::Str("refs")));
                }
            }

            emit!(self, Instr::BlockBegin(Some("FB")));
            self.block(data.idx);
            emit!(self, Instr::End);

            if rwt.is_some() {
                emit!(self, Instr::GetLocal(WT::Str("rv")));
            }

            emit!(self, Instr::End);

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
            NT::Block(_) => self.block(idx),
            NT::Assert(data) => self.assert_statement(&data),
            NT::Assignment(data) => self.assign_statement(&data),
            NT::Call(_) => self.call_statement(idx),
            NT::Declaration(data) => self.declaration(&data),
            NT::Return(data) => self.return_statement(&data),
            NT::Read(idx) => self.read_statement(idx),
            NT::Write(data) => self.write_statement(&data),
            NT::If(data) => self.if_statement(&data),
            NT::While(data) => self.while_statement(&data),
            _ => assert!(false, "Unexpected node {:#?}.", self.tree[idx]),
        };
    }

    fn assign_statement(&mut self, data: &IdxIdx) {
        if let NT::Variable(variable_data) = self.tree[data.idx].data {
            self.emit_set_local_pre_expr(&variable_data);
            self.expression(data.idx2);
            self.emit_set_local_post_expr(&variable_data);
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[data.idx]);
        }
    }

    fn assert_statement(&mut self, data: &TokenIdxOptIdx<'a>) {
        let idx = data.opt_idx.expect("Assert should have some opt_idx.");
        emit!(self, Instr::BlockBegin(None));
        emit!(self, Instr::BlockBegin(None));
        self.expression(data.idx);
        emit!(self, Instr::Eqz);
        emit!(self, Instr::BrIf(WT::I32(Some(0))));
        emit!(self, Instr::Br(WT::I32(Some(1))));
        emit!(self, Instr::End);
        emit!(self, Instr::Const(WT::I32(Some(idx as i32))));
        emit!(self, Instr::LibFunc("get_string_literal"));
        emit!(self, Instr::LibFunc("write_string"));
        emit!(self, Instr::LibFunc("write_newline"));
        emit!(self, Instr::Unreachable);
        emit!(self, Instr::End);
    }

    fn call_statement(&mut self, idx: usize) {
        if let NT::Call(data) = self.tree[idx].data {
            // Helper function
            let add_ref_idx_to_stack = |stacker: &mut Stacker<'a, 'b>, value: i32| {
                emit!(stacker, Instr::Const(WT::I32(Some(value))));
                emit!(stacker, Instr::GetLocal(WT::Str("refs")));
                emit!(stacker, Instr::Add(WT::I32(None)));
            };

            let token = data.token.expect("Call statement is missing a token.");
            let param_info = self
                .symbol_table
                .get_function_signature(token.value)
                .and_then(|fs| {
                    Some(
                        fs.parameters
                            .iter()
                            .map(|p| (p.is_ref, p.symbol_type))
                            .collect(),
                    )
                })
                .unwrap_or_else(|| vec![]);

            let mut next = data.opt_idx;
            let mut i = 0;
            let mut ref_idx = 0;
            let mut reference_map: HashMap<&str, i32> = HashMap::new();

            // Push all arguments on stack
            while let Some(idx) = next {
                let (is_ref, symbol_type) = param_info[i];
                if let NT::Variable(var_data) = self.tree[idx].data {
                    // We're passing a locally defined variable to the function (an lvalue)
                    let local_idx = self.get_variable_local_idx(&var_data);
                    let var_token = var_data.token.expect("Variable is missing a token.");

                    match symbol_type {
                        ST::Bool | ST::Int | ST::Real => {
                            if is_ref {
                                if let Some(arr_idx) = var_data.array_idx {
                                    emit!(self, Instr::GetLocal(WT::I32(Some(local_idx as i32),)));
                                    self.expression(arr_idx);
                                    let string_idx = var_data.string_idx.expect(
                                        "Variable should have some string index at call statement.",
                                    );
                                    emit!(self, Instr::Const(WT::I32(Some(string_idx as i32))));
                                    emit!(self, Instr::LibFunc("check_bounds"));
                                    // Get back to stack
                                    emit!(self, Instr::GetLocal(WT::I32(Some(local_idx as i32),)));
                                    emit!(self, Instr::Add(WT::I32(None)));
                                } else if var_data.is_ref {
                                    // We're passing a reference to a variable that we got
                                    // ourselves as a reference. Just pass the address on.
                                    // The variable at "local_idx" is a parameter we received and
                                    // it contains the address of the variable our caller gave us.
                                    emit!(self, Instr::GetLocal(WT::I32(Some(local_idx as i32),)));
                                } else {
                                    // We're passing a local variable (non-reference parameter or
                                    // an ordinary local) as a reference.
                                    if reference_map.get(var_token.value).is_none() {
                                        // A new variable is passed as ref
                                        add_ref_idx_to_stack(self, ref_idx);
                                        let instr =
                                            Instr::GetLocal(WT::I32(Some(local_idx as i32)));
                                        emit!(self, instr);
                                        let wt = if ST::Real == symbol_type {
                                            WT::F32(None)
                                        } else {
                                            WT::I32(None)
                                        };
                                        emit!(self, Instr::MemStore(wt));
                                        reference_map.insert(var_token.value, ref_idx);
                                    }

                                    let ref_idx = reference_map.get(var_token.value).expect(
                                        "The variable must be in the reference map by now.",
                                    );
                                    add_ref_idx_to_stack(self, *ref_idx);
                                }
                            } else {
                                // We're not passing a reference, but just an ordinary bool, int or
                                // real by value. The variable may still be something we got as
                                // reference, but that is handled at self.expression.
                                self.expression(idx);
                            }
                        }
                        ST::String
                        | ST::ArrayBool(_)
                        | ST::ArrayInt(_)
                        | ST::ArrayReal(_)
                        | ST::ArrayString(_) => {
                            // Arrays and strings are always passed as pointers,
                            // not as pointers to pointers.
                            if is_ref {
                                // If we're passing a reference,
                                // we can just pass the pointer, i.e. the actual address.
                                emit!(self, Instr::GetLocal(WT::I32(Some(local_idx as i32,))));
                            } else {
                                // Allocate a new array/string and pass its address: we don't want
                                // the callee to modify our data, since the array/string is passed
                                // by value.
                                let stride = if ST::String == symbol_type { 1 } else { 4 };
                                emit!(self, Instr::Const(WT::I32(Some(stride))));
                                emit!(self, Instr::Const(WT::I32(Some(0))));
                                emit!(self, Instr::LibFunc("new_array"));
                                emit!(self, Instr::GetLocal(WT::I32(Some(local_idx as i32,))));
                                emit!(self, Instr::LibFunc("copy_array"));
                            }
                        }
                        _ => {
                            assert!(false, "Unexpected symbol type {:#?}", var_data);
                        }
                    }
                } else {
                    // We're passing an rvalue, i.e. the result of an expression to a function.
                    // If the result of an expression is a value (not a memory address, i.e. not a
                    // string or any of the array types), store it in our local reference memory.
                    // This is done, because the callee will handle all reference parameters in the
                    // same way, i.e. load the value from a memory address, regardless of it's
                    // l or r-valueness.

                    if is_ref && self.is_bir(symbol_type) {
                        // Expression returns a value
                        add_ref_idx_to_stack(self, ref_idx);
                        self.expression(idx);
                        let wt = if ST::Real == symbol_type {
                            WT::F32(None)
                        } else {
                            WT::I32(None)
                        };
                        emit!(self, Instr::MemStore(wt));
                        add_ref_idx_to_stack(self, ref_idx);
                    } else {
                        // Expression returns a memory address
                        self.expression(idx);
                    }
                }

                ref_idx += is_ref as i32;
                i += 1;
                next = self.tree[idx].right_sibling;
            }

            // Call the function with the arguments on stack
            emit!(self, Instr::Call(WT::Str(token.value)));

            // If any locals were passed as references, store the value they've become back to
            // themselves.
            next = data.opt_idx;
            while let Some(idx) = next {
                if let NT::Variable(var_data) = self.tree[idx].data {
                    if let Some(&idx) = reference_map
                        .get(var_data.token.expect("Variable is missing a token").value)
                    {
                        assert!(
                            self.is_bir(var_data.st),
                            "Only variables of type bool, int, or real should lead here."
                        );
                        add_ref_idx_to_stack(self, idx);
                        let wt = if ST::Real == var_data.st {
                            WT::F32(None)
                        } else {
                            WT::I32(None)
                        };
                        emit!(self, Instr::MemLoad(wt));
                        let local_idx = self.get_variable_local_idx(&var_data);
                        emit!(self, Instr::SetLocal(WT::I32(Some(local_idx as i32))));
                    }
                }
                next = self.tree[idx].right_sibling;
            }

            // If current function has no return type and this call is a statement (= not part of an
            // expression), drop the value from stack.
            match self.tree[self.tree[idx].parent.unwrap()].data {
                NT::Block(_) | NT::If(_) | NT::While(_) => {
                    let return_type = self
                        .symbol_table
                        .get_function_signature(token.value)
                        .expect("Function should have a signature.")
                        .return_type;

                    if return_type.is_some() {
                        emit!(self, Instr::Drop);
                    }
                }
                _ => {}
            }
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
        }
    }

    fn declaration(&mut self, data: &IdxIdx) {
        let mut next = Some(data.idx);
        while let Some(idx) = next {
            if let NT::Variable(data) = self.tree[idx].data {
                let local_idx = self.get_variable_local_idx(&data);
                match data.st {
                    ST::Bool | ST::Int | ST::Real => {
                        let wt = if ST::Real == data.st {
                            WT::F32(Some(0.0))
                        } else {
                            WT::I32(Some(0))
                        };
                        emit!(self, Instr::Const(wt));
                    }
                    ST::String => {
                        emit!(self, Instr::Const(WT::I32(Some(1)))); // stride of the array, 1 byte
                        emit!(self, Instr::Const(WT::I32(Some(0)))); // lenght of the array, 0 bytes
                        emit!(self, Instr::LibFunc("new_array"));
                    }
                    ST::ArrayBool(expr_idx)
                    | ST::ArrayInt(expr_idx)
                    | ST::ArrayReal(expr_idx)
                    | ST::ArrayString(expr_idx) => {
                        let str_idx = data
                            .string_idx
                            .expect("Variable (at declaration) should have some string_idx.");
                        emit!(self, Instr::BlockBegin(None));
                        self.expression(expr_idx);
                        emit!(self, Instr::Const(WT::I32(Some(252)))); // 4 x i32 take space at the start
                        emit!(self, Instr::Less(WT::I32(None)));
                        emit!(self, Instr::BrIf(WT::I32(Some(0))));
                        emit!(self, Instr::Const(WT::I32(Some(str_idx as i32))));
                        emit!(self, Instr::LibFunc("get_string_literal"));
                        emit!(self, Instr::LibFunc("write_string"));
                        emit!(self, Instr::LibFunc("write_newline"));
                        emit!(self, Instr::Unreachable);
                        emit!(self, Instr::End);
                        emit!(self, Instr::Const(WT::I32(Some(4)))); // stride of the array, 4 bytes
                        self.expression(expr_idx);
                        emit!(self, Instr::LibFunc("new_array"));
                    }
                    _ => {
                        assert!(false, "Unexpected symbol type {:#?}.", data);
                    }
                }
                emit!(self, Instr::SetLocal(WT::I32(Some(local_idx as i32))));
            } else {
                break;
            }

            next = self.tree[idx].right_sibling;
        }
    }

    fn return_statement(&mut self, data: &TokenOptIdx<'a>) {
        if let Some(idx) = data.opt_idx {
            self.expression(idx);
            emit!(self, Instr::SetLocal(WT::Str("rv")));
        }
        emit!(self, Instr::Br(WT::Str("FB")));
    }

    fn read_statement(&mut self, idx: usize) {
        emit!(self, Instr::LibFunc("read_input"));
        let mut next = Some(idx);
        while let Some(idx) = next {
            if let NT::Variable(variable_data) = self.tree[idx].data {
                self.emit_set_local_pre_expr(&variable_data);
                let func_name = match variable_data.st {
                    ST::Bool => "bool_from_input",
                    ST::Int => "i32_from_input",
                    ST::Real => "f32_from_input",
                    ST::String => "string_from_input",
                    ST::ArrayBool(_)
                    | ST::ArrayInt(_)
                    | ST::ArrayReal(_)
                    | ST::ArrayString(_)
                    | ST::Undefined => {
                        assert!(false, "Unexpected symbol type {:#?}.", variable_data);
                        ""
                    }
                };
                emit!(self, Instr::LibFunc(func_name));
                self.emit_set_local_post_expr(&variable_data);
            } else {
                break;
            }

            next = self.tree[idx].right_sibling;
        }
    }

    fn write_statement(&mut self, data: &TokenIdx<'a>) {
        let token = data.token.expect("Write is missing a token.");
        let arguments = self
            .symbol_table
            .get_write_arguments((token.line, token.column));
        let mut i = 0;
        let mut next = Some(data.idx);
        while let Some(idx) = next {
            self.expression(idx);
            let func_name = match arguments[i] {
                ST::Bool => "write_bool",
                ST::Int => "write_i32",
                ST::Real => "write_f32",
                ST::String => "write_string",
                ST::ArrayBool(_) | ST::ArrayInt(_) | ST::ArrayReal(_) | ST::ArrayString(_) => {
                    "write_i32"
                }
                ST::Undefined => {
                    assert!(false, "Unexpected symbol type {:#?}.", data);
                    ""
                }
            };
            emit!(self, Instr::LibFunc(func_name));

            next = self.tree[idx].right_sibling;
            i += 1;
        }
        emit!(self, Instr::LibFunc("write_newline"));
        assert_eq!(i, arguments.len(), "Not all arguments used.");
    }

    fn if_statement(&mut self, data: &TokenIdxIdxOptIdx<'a>) {
        let has_else = data.opt_idx.is_some();
        let br_if_label = if has_else { 1 } else { 0 };

        if has_else {
            emit!(self, Instr::BlockBegin(None));
            emit!(self, Instr::BlockBegin(None));
        }
        emit!(self, Instr::BlockBegin(None));
        self.expression(data.idx);
        emit!(self, Instr::Eqz);
        emit!(self, Instr::BrIf(WT::I32(Some(br_if_label))));
        self.statement(data.idx2);
        emit!(self, Instr::End);
        if has_else {
            emit!(self, Instr::Br(WT::I32(Some(1))));
            emit!(self, Instr::End);
            self.statement(data.opt_idx.unwrap());
            emit!(self, Instr::End);
        }
    }

    fn while_statement(&mut self, data: &TokenIdxIdx<'a>) {
        emit!(self, Instr::BlockBegin(None));
        emit!(self, Instr::LoopBegin(None));
        self.expression(data.idx);
        emit!(self, Instr::Eqz);
        emit!(self, Instr::BrIf(WT::I32(Some(1))));
        self.statement(data.idx2);
        emit!(self, Instr::Br(WT::I32(Some(0))));
        emit!(self, Instr::End);
        emit!(self, Instr::End);
    }

    fn expression(&mut self, idx: usize) {
        match self.tree[idx].data {
            NT::RelOp(data) => {
                let token = data.token.expect("Relation operator is missing a token.");
                let st = data.st;
                self.expression(data.idx);
                self.expression(data.idx2);

                match st {
                    ST::Bool | ST::Int | ST::Real => {
                        let wt = if ST::Real == st {
                            WT::F32(None)
                        } else {
                            WT::I32(None)
                        };
                        let instr = match token.token_type {
                            TT::OperatorEqual => Instr::Eq(wt),
                            TT::OperatorNotEqual => Instr::NEq(wt),
                            TT::OperatorGreater => Instr::Great(wt),
                            TT::OperatorGreaterEqual => Instr::GreatEq(wt),
                            TT::OperatorLess => Instr::Less(wt),
                            TT::OperatorLessEqual => Instr::LessEq(wt),
                            _ => {
                                assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                                Instr::Eq(wt)
                            }
                        };
                        emit!(self, instr);
                    }
                    ST::String => {
                        let func_name = match token.token_type {
                            TT::OperatorEqual => "string_eq",
                            TT::OperatorNotEqual => "string_neq",
                            TT::OperatorGreater => "string_great",
                            TT::OperatorGreaterEqual => "string_great_eq",
                            TT::OperatorLess => "string_less",
                            TT::OperatorLessEqual => "string_less_eq",
                            _ => {
                                assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                                ""
                            }
                        };
                        emit!(self, Instr::LibFunc(func_name));
                    }
                    _ => {
                        assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                    }
                }
            }
            NT::AddOp(data) => {
                let token = data.token.expect("Add operator is missing a token.");
                let st = data.st;

                if let Some(idx) = data.opt_idx {
                    self.expression(data.idx);
                    self.expression(idx);

                    let instr = if ST::String == st {
                        Instr::LibFunc("string_concatenate")
                    } else {
                        let wt = if ST::Real == st {
                            WT::F32(None)
                        } else {
                            WT::I32(None)
                        };
                        match token.token_type {
                            TT::OperatorPlus => Instr::Add(wt),
                            TT::OperatorMinus => Instr::Sub(wt),
                            TT::OperatorOr => Instr::Or(wt),
                            _ => {
                                assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                                Instr::Add(wt)
                            }
                        }
                    };
                    emit!(self, instr);
                } else {
                    // Operator is sign, and idx is add_op
                    self.expression(data.idx);
                    let (const_instr, mul_instr) = if ST::Real == st {
                        (Instr::Const(WT::F32(Some(-1.0))), Instr::Mul(WT::F32(None)))
                    } else {
                        (Instr::Const(WT::I32(Some(-1))), Instr::Mul(WT::I32(None)))
                    };
                    emit!(self, const_instr);
                    emit!(self, mul_instr);
                }
            }
            NT::MulOp(data) => {
                let token = data.token.expect("Multiply operator is missing a token.");
                let st = data.st;
                self.expression(data.idx);
                self.expression(data.idx2);

                let wt = if ST::Real == st {
                    WT::F32(None)
                } else {
                    WT::I32(None)
                };
                let instr = match token.token_type {
                    TT::OperatorMultiply => Instr::Mul(wt),
                    TT::OperatorDivide => Instr::Div(wt),
                    TT::OperatorModulo => Instr::Mod(wt),
                    TT::OperatorAnd => Instr::And(wt),
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        Instr::Mul(wt)
                    }
                };
                emit!(self, instr);
            }
            NT::Variable(data) => {
                let local_idx = self.get_variable_local_idx(&data);
                emit!(self, Instr::GetLocal(WT::I32(Some(local_idx as i32))));
                match data.st {
                    ST::Bool | ST::Int | ST::Real => {
                        if let Some(arr_idx) = data.array_idx {
                            self.expression(arr_idx);
                            let string_idx = data
                                .string_idx
                                .expect("Variable should have some string index at array access.");
                            emit!(self, Instr::Const(WT::I32(Some(string_idx as i32))));
                            let func_name = if let ST::ArrayReal(_) = data.st {
                                "array_access_f"
                            } else {
                                "array_access_i"
                            };
                            emit!(self, Instr::LibFunc(func_name));
                        } else if data.is_ref {
                            let wt = if ST::Real == data.st {
                                WT::F32(None)
                            } else {
                                WT::I32(None)
                            };
                            emit!(self, Instr::MemLoad(wt));
                        }
                    }
                    ST::String => {
                        // String references pass their actual address, non-references pass the
                        // address of a copy. In either case, the value already points to the data.
                    }
                    ST::ArrayBool(_) | ST::ArrayInt(_) | ST::ArrayReal(_) | ST::ArrayString(_) => {
                        // Array references pass their actual address, non-references pass the
                        // address of a copy. In either case, the value already points to the data.
                    }
                    _ => {
                        assert!(false, "Unexpected symbol type {:#?}", data);
                    }
                }
            }
            NT::Literal(data) => {
                let token = data.token.unwrap();
                match token.token_type {
                    TT::LiteralBool => {
                        let wt = WT::I32(Some(if "true" == token.value { 1 } else { 0 }));
                        emit!(self, Instr::Const(wt));
                    }
                    TT::LiteralInt => {
                        let literal = token
                            .value
                            .parse::<i32>()
                            .expect("Literal int str should be possible to parse to an integer.");
                        emit!(self, Instr::Const(WT::I32(Some(literal))));
                    }
                    TT::LiteralReal => {
                        let literal = token
                            .value
                            .parse::<f32>()
                            .expect("Literal real str should be possible to parse to a float.");
                        emit!(self, Instr::Const(WT::F32(Some(literal))));
                    }
                    TT::LiteralString => {
                        // The idx is the "ranking" of the string literal. In other words, if this
                        // is the fifth literal string that is found in the program, idx is 5.
                        // Literal strings are stored consecutively in memory and the accessing
                        // functions uses this idx to find the address of the string literal.
                        let str_idx = data
                            .opt_idx
                            .expect("Literal string should have some opt_idx.");
                        emit!(self, Instr::Const(WT::I32(Some(str_idx as i32))));
                        emit!(self, Instr::LibFunc("get_string_literal"));
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", data);
                    }
                }
            }
            NT::Not(data) => {
                self.expression(data.idx);
                emit!(self, Instr::Eqz);
            }
            NT::ArraySize(data) => {
                self.expression(data.idx);
                emit!(self, Instr::LibFunc("array_size"));
            }
            NT::Call(_) => self.call_statement(idx),
            _ => {
                assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
            }
        }
    }

    fn get_variable_local_idx(&mut self, data: &VariableData<'a>) -> usize {
        data.count
            + self.symbol_table.get_variable_index(
                self.fname
                    .expect("Function must have a name at this point."),
                data.st,
                data.depth,
            )
    }

    fn emit_set_local_pre_expr(&mut self, data: &VariableData<'a>) {
        match data.st {
            ST::Bool | ST::Int | ST::Real => {
                let local_idx = self.get_variable_local_idx(&data);
                if let Some(expr_idx) = data.array_idx {
                    emit!(self, Instr::GetLocal(WT::I32(Some(local_idx as i32))));
                    self.expression(expr_idx);
                    let string_idx = data
                        .string_idx
                        .expect("Variable should have some string index at set local pre_expr.");
                    emit!(self, Instr::Const(WT::I32(Some(string_idx as i32))));
                } else if data.is_ref {
                    emit!(self, Instr::GetLocal(WT::I32(Some(local_idx as i32))));
                }
            }
            ST::String
            | ST::ArrayBool(_)
            | ST::ArrayInt(_)
            | ST::ArrayReal(_)
            | ST::ArrayString(_)
            | ST::Undefined => {}
        }
    }

    fn emit_set_local_post_expr(&mut self, data: &VariableData<'a>) {
        match data.st {
            ST::Bool | ST::Int | ST::Real => {
                let instr = if data.array_idx.is_some() {
                    let func_name = if let ST::Real = data.st {
                        "array_assign_f"
                    } else {
                        "array_assign_i"
                    };
                    Instr::LibFunc(func_name)
                } else if data.is_ref {
                    let wt = if ST::Real == data.st {
                        WT::F32(None)
                    } else {
                        WT::I32(None)
                    };
                    Instr::MemStore(wt)
                } else {
                    let local_idx = self.get_variable_local_idx(&data);
                    Instr::SetLocal(WT::I32(Some(local_idx as i32)))
                };
                emit!(self, instr);
            }
            ST::String
            | ST::ArrayBool(_)
            | ST::ArrayInt(_)
            | ST::ArrayReal(_)
            | ST::ArrayString(_) => {
                emit!(self, Instr::LibFunc("copy_array"));
                emit!(self, Instr::Drop);
            }
            _ => {
                assert!(false, "Unexpected symbol type {:#?}", data);
            }
        }
    }

    fn is_bir(&self, st: ST) -> bool {
        ST::Bool == st || ST::Int == st || ST::Real == st
    }

    pub fn new(
        tree: &'b LcRsTree<NT<'a>>,
        symbol_table: &'b mut SymbolTable<'a>,
        instructions: &'b mut Vec<Instr<'a>>,
    ) -> Self {
        Stacker {
            tree: tree,
            symbol_table: symbol_table,
            fname: None,
            instructions: instructions,
            library_functions: HashSet::new(),
        }
    }
}

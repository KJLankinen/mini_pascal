use super::data_types::NodeType as NT;
use super::data_types::SymbolType as ST;
use super::data_types::TokenType as TT;
use super::data_types::WasmType as WT;
use super::data_types::{
    IdxIdx, Instruction, TokenIdx, TokenIdxIdx, TokenIdxIdxOptIdx, TokenIdxOptIdx, TokenOptIdx,
    VariableData,
};
use super::lcrs_tree::LcRsTree;
use super::symbol_table::SymbolTable;
use std::collections::{HashMap, HashSet};
use std::mem;

pub struct Stacker<'a, 'b> {
    tree: &'b LcRsTree<NT<'a>>,
    symbol_table: &'b mut SymbolTable<'a>,
    fname: Option<&'a str>,
    instructions: &'b mut Vec<Instruction<'a>>,
    library_functions: HashSet<&'a str>,
}

macro_rules! emit {
    ($stacker:expr, $item:expr) => {
        if let Instruction::LibFunc(name) = $item {
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

            emit!(self, Instruction::ProgramBegin(token.value));
            emit!(self, Instruction::Imports);
            if 0 < self.symbol_table.borrow_string_literals().len() {
                emit!(self, Instruction::DataSegment);
            }

            if let Some(idx) = data.opt_idx {
                self.subroutines(idx);
            }
            emit!(self, Instruction::FunctionBegin("_start"));
            self.fname = Some(token.value);

            for _ in 0..it {
                emit!(self, Instruction::Local(None, WT::I32(None)));
            }

            for _ in 0..ft {
                emit!(self, Instruction::Local(None, WT::F32(None)));
            }

            let ref_count = self.symbol_table.get_function_ref_count(token.value);
            if let Some(nr) = ref_count {
                // nr tells the maximum number of arguments that are references across all the
                // different function calls that are issued from this function.
                // The local variables that are passed as references are stored in linear
                // memory, the base address of which is stored in the local special variable "refs".
                if 0 < nr {
                    emit!(self, Instruction::Local(Some("refs"), WT::I32(None)));
                    emit!(self, Instruction::Const(WT::I32(Some((nr * 4) as i32)))); // 4 bytes per variable
                    emit!(self, Instruction::LibFunc("allocate"));
                    emit!(self, Instruction::SetLocal(WT::Str("refs")));
                }
            }

            emit!(self, Instruction::BlockBegin(Some("FB")));
            self.block(data.idx);
            emit!(self, Instruction::End);

            self.fname = None;
            emit!(self, Instruction::End);
            emit!(self, Instruction::End);
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

            emit!(self, Instruction::FunctionBegin(token.value));
            for param in &self
                .symbol_table
                .get_function_signature(token.value)
                .expect("Function should have a signature.")
                .parameters
            {
                if ST::Real == param.symbol_type && false == param.is_ref {
                    emit!(self, Instruction::Param(WT::F32(None)));
                } else {
                    emit!(self, Instruction::Param(WT::I32(None)));
                }
            }

            let return_type = self
                .symbol_table
                .get_function_signature(token.value)
                .expect("Function should have a signature.")
                .return_type;

            if let Some(rt) = return_type {
                if ST::Real == rt {
                    emit!(self, Instruction::Result(WT::F32(None)));
                } else {
                    emit!(self, Instruction::Result(WT::I32(None)));
                }
            }

            for _ in 0..it {
                emit!(self, Instruction::Local(None, WT::I32(None)));
            }

            for _ in 0..ft {
                emit!(self, Instruction::Local(None, WT::F32(None)));
            }

            if let Some(rt) = return_type {
                if ST::Real == rt {
                    emit!(self, Instruction::Local(Some("rv"), WT::F32(None)));
                } else {
                    emit!(self, Instruction::Local(Some("rv"), WT::I32(None)));
                }
            }

            let ref_count = self.symbol_table.get_function_ref_count(token.value);
            if let Some(nr) = ref_count {
                // nr tells the maximum number of arguments that are references across all the
                // different function calls that are issued from this function.
                // The local variables that are passed as references are stored in linear
                // memory, the base address of which is stored in the local special variable "refs".
                if 0 < nr {
                    emit!(self, Instruction::Local(Some("refs"), WT::I32(None)));
                    emit!(self, Instruction::Const(WT::I32(Some((nr * 4) as i32)))); // 4 bytes per variable
                    emit!(self, Instruction::LibFunc("allocate"));
                    emit!(self, Instruction::SetLocal(WT::Str("refs")));
                }
            }

            emit!(self, Instruction::BlockBegin(Some("FB")));
            self.block(data.idx);
            emit!(self, Instruction::End);

            if return_type.is_some() {
                emit!(self, Instruction::GetLocal(WT::Str("rv")));
            }

            emit!(self, Instruction::End);

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
        emit!(self, Instruction::BlockBegin(None));
        emit!(self, Instruction::BlockBegin(None));
        self.expression(data.idx);
        emit!(self, Instruction::Eqz);
        emit!(self, Instruction::BrIf(WT::I32(Some(0))));
        emit!(self, Instruction::Br(WT::I32(Some(1))));
        emit!(self, Instruction::End);
        emit!(self, Instruction::Const(WT::I32(Some(idx as i32))));
        emit!(self, Instruction::LibFunc("get_string_literal"));
        emit!(self, Instruction::LibFunc("write_string"));
        emit!(self, Instruction::Unreachable);
        emit!(self, Instruction::End);
    }

    fn call_statement(&mut self, idx: usize) {
        let add_ref_idx_to_stack = |stacker: &mut Stacker<'a, 'b>, value: i32| {
            stacker
                .instructions
                .push(Instruction::Const(WT::I32(Some(value))));
            stacker
                .instructions
                .push(Instruction::GetLocal(WT::Str("refs")));
            stacker.instructions.push(Instruction::Add(WT::I32(None)));
        };

        if let NT::Call(data) = self.tree[idx].data {
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
                                    emit!(
                                        self,
                                        Instruction::GetLocal(WT::I32(Some(local_idx as i32),))
                                    );
                                    self.expression(arr_idx);
                                    let string_idx = var_data.string_idx.expect(
                                        "Variable should have some string index at call statement.",
                                    );
                                    emit!(
                                        self,
                                        Instruction::Const(WT::I32(Some(string_idx as i32)))
                                    );
                                    emit!(self, Instruction::LibFunc("check_bounds"));
                                    // Get back to stack
                                    emit!(
                                        self,
                                        Instruction::GetLocal(WT::I32(Some(local_idx as i32),))
                                    );
                                    emit!(self, Instruction::Add(WT::I32(None)));
                                } else if var_data.is_ref {
                                    // We're passing a reference to a variable that we got
                                    // ourselves as a reference. Just pass the address on.
                                    // The variable at "local_idx" is a parameter we received and
                                    // it contains the address of the variable our caller gave us.
                                    emit!(
                                        self,
                                        Instruction::GetLocal(WT::I32(Some(local_idx as i32),))
                                    );
                                } else {
                                    // We're passing a local variable (non-reference parameter or
                                    // an ordinary local) as a reference.
                                    if let Some(&idx) = reference_map.get(var_token.value) {
                                        // Already passed this variable as a reference earlier, pass the
                                        // local address again.
                                        add_ref_idx_to_stack(self, idx);
                                    } else {
                                        // This is a new variable we're passing as a refence. Add
                                        // it to map.
                                        reference_map.insert(var_token.value, ref_idx);
                                        // Get address to stack
                                        add_ref_idx_to_stack(self, ref_idx);
                                        // The variable at "local_idx" contains just an ordinary value
                                        // for bool, int or real. Get the value to stack.
                                        emit!(
                                            self,
                                            Instruction::GetLocal(WT::I32(Some(local_idx as i32)),)
                                        );

                                        // Store the value at the ref index
                                        if ST::Real == symbol_type {
                                            emit!(self, Instruction::MemStore(WT::F32(None)));
                                        } else {
                                            emit!(self, Instruction::MemStore(WT::I32(None)));
                                        }

                                        // Get the ref index back to top of stack and pass it to the
                                        // callee.
                                        add_ref_idx_to_stack(self, ref_idx);
                                    }
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
                                emit!(
                                    self,
                                    Instruction::GetLocal(WT::I32(Some(local_idx as i32,)))
                                );
                            } else {
                                // Allocate a new array/string and pass its address: we don't want
                                // the callee to modify our data, since the array/string is passed
                                // by value.
                                let stride = if ST::String == symbol_type { 1 } else { 4 };
                                emit!(self, Instruction::Const(WT::I32(Some(stride))));
                                emit!(self, Instruction::LibFunc("new_array"));
                                emit!(
                                    self,
                                    Instruction::GetLocal(WT::I32(Some(local_idx as i32,)))
                                );
                                emit!(self, Instruction::LibFunc("copy_array"));
                            }
                        }
                        _ => {
                            assert!(false, "Unexpected symbol type {:#?}", var_data);
                        }
                    }
                } else {
                    // We're passing an rvalue, i.e. the result of an expression to a function.
                    // We don't have to care about updating our own values after the function call,
                    // since the function will only modify temporary variables (rvalues), but we
                    // still must save ordinary (non-array) values to our reference array, because
                    // the callee will handle all its reference parameters in the same way. So even
                    // if the contents are temporary, the handling will still involve memory loads.
                    if is_ref {
                        match symbol_type {
                            ST::Bool | ST::Int | ST::Real => {
                                // Get the address of this reference to top of stack
                                add_ref_idx_to_stack(self, ref_idx);
                            }
                            _ => {}
                        }
                    }

                    // Get the value of the expression to top of stack
                    self.expression(idx);

                    if is_ref {
                        match symbol_type {
                            ST::Bool | ST::Int | ST::Real => {
                                // Store the value of the expression to the local reference array
                                if ST::Real == symbol_type {
                                    emit!(self, Instruction::MemStore(WT::F32(None)));
                                } else {
                                    emit!(self, Instruction::MemStore(WT::I32(None)));
                                }
                                // Get address of stored variable back to stack
                                add_ref_idx_to_stack(self, ref_idx);
                            }
                            _ => {}
                        }
                    }
                }

                ref_idx += is_ref as i32;
                i += 1;
                next = self.tree[idx].right_sibling;
            }

            // Call the function with the arguments on stack
            emit!(self, Instruction::Call(WT::Str(token.value)));

            // Store any references back to locals
            next = data.opt_idx;
            while let Some(idx) = next {
                if let NT::Variable(var_data) = self.tree[idx].data {
                    if let Some(&idx) = reference_map
                        .get(var_data.token.expect("Variable is missing a token").value)
                    {
                        add_ref_idx_to_stack(self, idx);
                        match var_data.st {
                            ST::Bool | ST::Int => {
                                emit!(self, Instruction::MemLoad(WT::I32(None)));
                            }
                            ST::Real => {
                                emit!(self, Instruction::MemLoad(WT::F32(None)));
                            }
                            _ => {
                                assert!(
                                    false,
                                    "This symbol type should not lead here {:#?}",
                                    var_data
                                );
                            }
                        }
                        let local_idx = self.get_variable_local_idx(&var_data);
                        emit!(self, Instruction::SetLocal(WT::I32(Some(local_idx as i32))));
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
                        emit!(self, Instruction::Drop);
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
                    ST::Bool | ST::Int => {
                        emit!(self, Instruction::Const(WT::I32(Some(0))));
                    }
                    ST::Real => {
                        emit!(self, Instruction::Const(WT::F32(Some(0.0))));
                    }
                    ST::String => {
                        emit!(self, Instruction::Const(WT::I32(Some(1)))); // stride of the array, 1 byte
                        emit!(self, Instruction::LibFunc("new_array"));
                    }
                    ST::ArrayBool(expr_idx)
                    | ST::ArrayInt(expr_idx)
                    | ST::ArrayReal(expr_idx)
                    | ST::ArrayString(expr_idx) => {
                        let str_idx = data
                            .string_idx
                            .expect("Variable (at declaration) should have some string_idx.");
                        emit!(self, Instruction::BlockBegin(None));
                        self.expression(expr_idx);
                        emit!(self, Instruction::Const(WT::I32(Some(252)))); // 4xi32 take space at the start
                        emit!(self, Instruction::Less(WT::I32(None)));
                        emit!(self, Instruction::BrIf(WT::I32(Some(0))));
                        emit!(self, Instruction::Const(WT::I32(Some(str_idx as i32))));
                        emit!(self, Instruction::LibFunc("get_string_literal"));
                        emit!(self, Instruction::LibFunc("write_string"));
                        emit!(self, Instruction::Unreachable);
                        emit!(self, Instruction::End);
                        emit!(self, Instruction::Const(WT::I32(Some(4)))); // stride of the array, 4 bytes
                        emit!(self, Instruction::LibFunc("new_array"));
                    }
                    _ => {
                        assert!(false, "Unexpected symbol type {:#?}.", data);
                    }
                }
                emit!(self, Instruction::SetLocal(WT::I32(Some(local_idx as i32))));
            } else {
                break;
            }

            next = self.tree[idx].right_sibling;
        }
    }

    fn return_statement(&mut self, data: &TokenOptIdx<'a>) {
        if let Some(idx) = data.opt_idx {
            self.expression(idx);
            emit!(self, Instruction::SetLocal(WT::Str("rv")));
        }
        emit!(self, Instruction::Br(WT::Str("FB")));
    }

    fn read_statement(&mut self, idx: usize) {
        emit!(self, Instruction::LibFunc("read_input"));
        let mut next = Some(idx);
        while let Some(idx) = next {
            if let NT::Variable(variable_data) = self.tree[idx].data {
                self.emit_set_local_pre_expr(&variable_data);
                match variable_data.st {
                    ST::Bool => {
                        emit!(self, Instruction::LibFunc("bool_from_input"));
                    }
                    ST::Int => {
                        emit!(self, Instruction::LibFunc("i32_from_input"));
                    }
                    ST::Real => {
                        emit!(self, Instruction::LibFunc("f32_from_input"));
                    }
                    ST::String => {
                        emit!(self, Instruction::LibFunc("string_from_input"));
                    }
                    ST::ArrayBool(_)
                    | ST::ArrayInt(_)
                    | ST::ArrayReal(_)
                    | ST::ArrayString(_)
                    | ST::Undefined => {
                        assert!(false, "Unexpected symbol type {:#?}.", variable_data);
                    }
                }
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
        let mut next = Some(data.idx);
        let mut i = 0;
        while let Some(idx) = next {
            self.expression(idx);

            match arguments[i] {
                ST::Bool => {
                    emit!(self, Instruction::LibFunc("write_bool"));
                }
                ST::Int => {
                    emit!(self, Instruction::LibFunc("write_i32"));
                }
                ST::Real => {
                    emit!(self, Instruction::LibFunc("write_f32"));
                }
                ST::String => {
                    emit!(self, Instruction::LibFunc("write_string"));
                }
                ST::ArrayBool(_) | ST::ArrayInt(_) | ST::ArrayReal(_) | ST::ArrayString(_) => {
                    emit!(self, Instruction::LibFunc("write_i32"));
                }
                ST::Undefined => {
                    assert!(false, "Unexpected symbol type {:#?}.", data);
                }
            }

            next = self.tree[idx].right_sibling;
            i += 1;
        }
        assert_eq!(i, arguments.len(), "Not all arguments used.");
    }

    fn if_statement(&mut self, data: &TokenIdxIdxOptIdx<'a>) {
        let has_else = data.opt_idx.is_some();
        let br_if_label = if has_else { 1 } else { 0 };

        if has_else {
            emit!(self, Instruction::BlockBegin(None));
            emit!(self, Instruction::BlockBegin(None));
        }
        emit!(self, Instruction::BlockBegin(None));
        self.expression(data.idx);
        emit!(self, Instruction::Eqz);
        emit!(self, Instruction::BrIf(WT::I32(Some(br_if_label))));
        self.statement(data.idx2);
        emit!(self, Instruction::End);
        if has_else {
            emit!(self, Instruction::Br(WT::I32(Some(1))));
            emit!(self, Instruction::End);
            self.statement(data.opt_idx.unwrap());
            emit!(self, Instruction::End);
        }
    }

    fn while_statement(&mut self, data: &TokenIdxIdx<'a>) {
        emit!(self, Instruction::BlockBegin(None));
        emit!(self, Instruction::LoopBegin(None));
        self.expression(data.idx);
        emit!(self, Instruction::BrIf(WT::I32(Some(1))));
        self.statement(data.idx2);
        emit!(self, Instruction::Br(WT::I32(Some(0))));
        emit!(self, Instruction::End);
        emit!(self, Instruction::End);
    }

    fn expression(&mut self, idx: usize) {
        match self.tree[idx].data {
            NT::RelOp(data) => {
                let token = data.token.expect("Relation operator is missing a token.");
                let st = data.st;
                self.expression(data.idx);
                self.expression(data.idx2);

                match st {
                    ST::Bool => match token.token_type {
                        TT::OperatorEqual => {
                            emit!(self, Instruction::Eq(WT::I32(None)));
                        }
                        TT::OperatorNotEqual => {
                            emit!(self, Instruction::NEq(WT::I32(None)));
                        }
                        TT::OperatorGreater => {
                            emit!(self, Instruction::Great(WT::I32(None)));
                        }
                        TT::OperatorGreaterEqual => {
                            emit!(self, Instruction::GreatEq(WT::I32(None)));
                        }
                        TT::OperatorLess => {
                            emit!(self, Instruction::Less(WT::I32(None)));
                        }
                        TT::OperatorLessEqual => {
                            emit!(self, Instruction::LessEq(WT::I32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    ST::Int => match token.token_type {
                        TT::OperatorEqual => {
                            emit!(self, Instruction::Eq(WT::I32(None)));
                        }
                        TT::OperatorNotEqual => {
                            emit!(self, Instruction::NEq(WT::I32(None)));
                        }
                        TT::OperatorGreater => {
                            emit!(self, Instruction::Great(WT::I32(None)));
                        }
                        TT::OperatorGreaterEqual => {
                            emit!(self, Instruction::GreatEq(WT::I32(None)));
                        }
                        TT::OperatorLess => {
                            emit!(self, Instruction::Less(WT::I32(None)));
                        }
                        TT::OperatorLessEqual => {
                            emit!(self, Instruction::LessEq(WT::I32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    ST::Real => match token.token_type {
                        TT::OperatorEqual => {
                            emit!(self, Instruction::Eq(WT::F32(None)));
                        }
                        TT::OperatorNotEqual => {
                            emit!(self, Instruction::NEq(WT::F32(None)));
                        }
                        TT::OperatorGreater => {
                            emit!(self, Instruction::Great(WT::F32(None)));
                        }
                        TT::OperatorGreaterEqual => {
                            emit!(self, Instruction::GreatEq(WT::F32(None)));
                        }
                        TT::OperatorLess => {
                            emit!(self, Instruction::Less(WT::F32(None)));
                        }
                        TT::OperatorLessEqual => {
                            emit!(self, Instruction::LessEq(WT::F32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    ST::String => match token.token_type {
                        TT::OperatorEqual => {
                            emit!(self, Instruction::LibFunc("string_eq"));
                        }
                        TT::OperatorNotEqual => {
                            emit!(self, Instruction::LibFunc("string_neq"));
                        }
                        TT::OperatorGreater => {
                            emit!(self, Instruction::LibFunc("string_great"));
                        }
                        TT::OperatorGreaterEqual => {
                            emit!(self, Instruction::LibFunc("string_great_eq"));
                        }
                        TT::OperatorLess => {
                            emit!(self, Instruction::LibFunc("string_less"));
                        }
                        TT::OperatorLessEqual => {
                            emit!(self, Instruction::LibFunc("string_less_eq"));
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
            NT::AddOp(data) => {
                let token = data.token.expect("Add operator is missing a token.");
                let st = data.st;

                if let Some(idx) = data.opt_idx {
                    self.expression(data.idx);
                    self.expression(idx);

                    match token.token_type {
                        TT::OperatorPlus => match st {
                            ST::Int => {
                                emit!(self, Instruction::Add(WT::I32(None)));
                            }
                            ST::Real => {
                                emit!(self, Instruction::Add(WT::F32(None)));
                            }
                            ST::String => {
                                emit!(self, Instruction::LibFunc("string_concatenate"));
                            }
                            _ => {
                                assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                            }
                        },
                        TT::OperatorMinus => match st {
                            ST::Int => {
                                emit!(self, Instruction::Sub(WT::I32(None)));
                            }
                            ST::Real => {
                                emit!(self, Instruction::Sub(WT::F32(None)));
                            }
                            _ => {
                                assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                            }
                        },
                        TT::OperatorOr => {
                            emit!(self, Instruction::Or(WT::I32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    }
                } else {
                    // Operator is sign, and idx is add_op
                    self.expression(data.idx);

                    match token.token_type {
                        TT::OperatorPlus => {}
                        TT::OperatorMinus => match st {
                            ST::Int => {
                                emit!(self, Instruction::Const(WT::I32(Some(-1))));
                                emit!(self, Instruction::Mul(WT::I32(None)));
                            }
                            ST::Real => {
                                emit!(self, Instruction::Const(WT::F32(Some(-1.0))));
                                emit!(self, Instruction::Mul(WT::I32(None)));
                            }
                            _ => {
                                assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                            }
                        },
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    }
                }
            }
            NT::MulOp(data) => {
                let token = data.token.expect("Multiply operator is missing a token.");
                let st = data.st;
                self.expression(data.idx);
                self.expression(data.idx2);

                match token.token_type {
                    TT::OperatorMultiply => match st {
                        ST::Int => {
                            emit!(self, Instruction::Mul(WT::I32(None)));
                        }
                        ST::Real => {
                            emit!(self, Instruction::Mul(WT::F32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                        }
                    },
                    TT::OperatorDivide => match st {
                        ST::Int => {
                            emit!(self, Instruction::Div(WT::I32(None)));
                        }
                        ST::Real => {
                            emit!(self, Instruction::Div(WT::F32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                        }
                    },
                    TT::OperatorModulo => {
                        emit!(self, Instruction::Mod(WT::I32(None)));
                    }
                    TT::OperatorAnd => {
                        emit!(self, Instruction::And(WT::I32(None)));
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                    }
                }
            }
            NT::Variable(data) => {
                let local_idx = self.get_variable_local_idx(&data);
                emit!(self, Instruction::GetLocal(WT::I32(Some(local_idx as i32))));
                match data.st {
                    ST::Bool | ST::Int => {
                        if data.is_ref {
                            emit!(self, Instruction::MemLoad(WT::I32(None)));
                        }
                    }
                    ST::Real => {
                        if data.is_ref {
                            emit!(self, Instruction::MemLoad(WT::F32(None)));
                        }
                    }
                    ST::String => {
                        // String references pass their actual address, non-references pass the
                        // address of a copy. In either case, the value already points to the data.
                    }
                    ST::ArrayBool(_) | ST::ArrayInt(_) | ST::ArrayReal(_) | ST::ArrayString(_) => {
                        // Array references pass their actual address, non-references pass the
                        // address of a copy. In either case, the value already points to the data.
                        if let Some(arr_idx) = data.array_idx {
                            self.expression(arr_idx);
                            let string_idx = data
                                .string_idx
                                .expect("Variable should have some string index at array access.");
                            emit!(self, Instruction::Const(WT::I32(Some(string_idx as i32))));
                            if let ST::ArrayReal(_) = data.st {
                                emit!(self, Instruction::LibFunc("array_access_f"));
                            } else {
                                emit!(self, Instruction::LibFunc("array_access_i"));
                            }
                        }
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
                        if "true" == token.value {
                            emit!(self, Instruction::Const(WT::I32(Some(1))));
                        } else {
                            emit!(self, Instruction::Const(WT::I32(Some(0))));
                        }
                    }
                    TT::LiteralInt => {
                        let literal = token
                            .value
                            .parse::<i32>()
                            .expect("Literal int str should be possible to parse to an integer.");
                        emit!(self, Instruction::Const(WT::I32(Some(literal))));
                    }
                    TT::LiteralReal => {
                        let literal = token
                            .value
                            .parse::<f32>()
                            .expect("Literal real str should be possible to parse to a float.");
                        emit!(self, Instruction::Const(WT::F32(Some(literal))));
                    }
                    TT::LiteralString => {
                        // The idx is the "ranking" of the string literal. In other words, if this
                        // is the fifth literal string that is found in the program, idx is 5.
                        // Literal strings are stored consecutively in memory and the accessing
                        // functions uses this idx to find the address of the string literal.
                        let str_idx = data
                            .opt_idx
                            .expect("Literal string should have some opt_idx.");
                        emit!(self, Instruction::Const(WT::I32(Some(str_idx as i32))));
                        emit!(self, Instruction::LibFunc("get_string_literal"));
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", data);
                    }
                }
            }
            NT::Not(data) => {
                self.expression(data.idx);
                emit!(self, Instruction::Eqz);
            }
            NT::ArraySize(data) => {
                self.expression(data.idx);
                emit!(self, Instruction::LibFunc("array_size"));
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
        let local_idx = self.get_variable_local_idx(&data);
        match data.st {
            ST::Bool | ST::Int | ST::Real => {
                if data.is_ref {
                    emit!(self, Instruction::GetLocal(WT::I32(Some(local_idx as i32))));
                }
            }
            ST::String => {
                emit!(self, Instruction::GetLocal(WT::I32(Some(local_idx as i32))));
            }
            ST::ArrayBool(_) | ST::ArrayInt(_) | ST::ArrayReal(_) | ST::ArrayString(_) => {
                emit!(self, Instruction::GetLocal(WT::I32(Some(local_idx as i32))));

                if let Some(expr_idx) = data.array_idx {
                    self.expression(expr_idx);
                    let string_idx = data
                        .string_idx
                        .expect("Variable should have some string index at set local pre_expr.");
                    emit!(self, Instruction::Const(WT::I32(Some(string_idx as i32))));
                }
            }
            _ => {
                assert!(false, "Unexpected symbol type {:#?}", data);
            }
        }
    }

    fn emit_set_local_post_expr(&mut self, data: &VariableData<'a>) {
        match data.st {
            ST::Bool | ST::Int | ST::Real => {
                if data.is_ref {
                    if ST::Real == data.st {
                        emit!(self, Instruction::MemStore(WT::F32(None)));
                    } else {
                        emit!(self, Instruction::MemStore(WT::I32(None)));
                    }
                } else {
                    let local_idx = self.get_variable_local_idx(&data);
                    emit!(self, Instruction::SetLocal(WT::I32(Some(local_idx as i32))));
                }
            }
            ST::String => {
                emit!(self, Instruction::LibFunc("copy_array"));
                emit!(self, Instruction::Drop);
            }
            ST::ArrayBool(_) | ST::ArrayInt(_) | ST::ArrayReal(_) | ST::ArrayString(_) => {
                if data.array_idx.is_some() {
                    if let ST::ArrayReal(_) = data.st {
                        emit!(self, Instruction::LibFunc("array_assign_f"));
                    } else {
                        emit!(self, Instruction::LibFunc("array_assign_i"));
                    }
                } else {
                    emit!(self, Instruction::LibFunc("copy_array"));
                    emit!(self, Instruction::Drop);
                }
            }
            _ => {
                assert!(false, "Unexpected symbol type {:#?}", data);
            }
        }
    }

    pub fn new(
        tree: &'b LcRsTree<NT<'a>>,
        symbol_table: &'b mut SymbolTable<'a>,
        instructions: &'b mut Vec<Instruction<'a>>,
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

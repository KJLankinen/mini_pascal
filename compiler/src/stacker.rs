use super::data_types::{
    IdxIdx, NodeType, SymbolType, TokenIdx, TokenIdxIdx, TokenIdxIdxOptIdx, TokenIdxOptIdx,
    TokenOptIdx, TokenSymbolBoolIdxIdxOptIdx, TokenType,
};
use super::lcrs_tree::LcRsTree;
use super::symbol_table::SymbolTable;
use std::collections::HashMap;

pub struct Stacker<'a, 'b> {
    tree: &'b LcRsTree<NodeType<'a>>,
    symbol_table: &'b mut SymbolTable<'a>,
    fname: Option<&'a str>,
    instructions: Vec<Instruction<'a>>,
}

#[derive(Debug)]
enum WasmType<'a> {
    I32(Option<i32>),
    F32(Option<f32>),
    Str(&'a str),
}

#[derive(Debug)]
enum Instruction<'a> {
    ProgramBegin(&'a str),
    FunctionBegin(&'a str),
    BlockBegin(Option<&'a str>),
    LoopBegin(Option<&'a str>),
    End,
    Param(WasmType<'a>),
    Result(WasmType<'a>),
    Local(Option<&'a str>, WasmType<'a>),
    GetLocal(WasmType<'a>),
    SetLocal(WasmType<'a>),
    MemLoad(WasmType<'a>),
    MemStore(WasmType<'a>),
    Const(WasmType<'a>),
    Call(WasmType<'a>),
    Eqz,
    Br(WasmType<'a>),
    BrIf(WasmType<'a>),
    Unreachable,
    Drop,
    Eq(WasmType<'a>),
    NEq(WasmType<'a>),
    GreatEq(WasmType<'a>),
    Great(WasmType<'a>),
    LessEq(WasmType<'a>),
    Less(WasmType<'a>),
    Add(WasmType<'a>),
    Sub(WasmType<'a>),
    Mul(WasmType<'a>),
    Div(WasmType<'a>),
    Mod(WasmType<'a>),
    And(WasmType<'a>),
    Or(WasmType<'a>),
}

impl<'a, 'b> Stacker<'a, 'b> {
    pub fn stack_ir(&mut self) {
        self.program(0);
        println!("{:#?}", self.instructions);
    }

    // ---------------------------------------------------------------------
    // Functions that handle emitting instructions
    // ---------------------------------------------------------------------
    fn program(&mut self, idx: usize) {
        if let NodeType::Program(data) = self.tree[idx].data {
            let token = data.token.expect("Program is missing a token.");
            self.instructions
                .push(Instruction::ProgramBegin(token.value));
            if let Some(idx) = data.opt_idx {
                self.subroutines(idx);
            }
            self.instructions.push(Instruction::FunctionBegin("_start"));
            self.fname = Some(token.value);
            self.block(data.idx);
            self.fname = None;
            self.instructions.push(Instruction::End);
            self.instructions.push(Instruction::End);
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
            let token = data.token.expect("Function is missing a token.");
            self.fname = Some(token.value);
            let (it, ft) = self
                .symbol_table
                .get_local_variable_totals(token.value)
                .expect("Function should have totals.");

            self.instructions
                .push(Instruction::FunctionBegin(token.value));
            for param in &self
                .symbol_table
                .get_function_signature(token.value)
                .expect("Function should have a signature.")
                .parameters
            {
                if SymbolType::Real == param.symbol_type && false == param.is_ref {
                    self.instructions
                        .push(Instruction::Param(WasmType::F32(None)));
                } else {
                    self.instructions
                        .push(Instruction::Param(WasmType::I32(None)));
                }
            }

            let return_type = self
                .symbol_table
                .get_function_signature(token.value)
                .expect("Function should have a signature.")
                .return_type;

            if let Some(rt) = return_type {
                if SymbolType::Real == rt {
                    self.instructions
                        .push(Instruction::Result(WasmType::F32(None)));
                } else {
                    self.instructions
                        .push(Instruction::Result(WasmType::I32(None)));
                }
            }

            for _ in 0..=it {
                self.instructions
                    .push(Instruction::Local(None, WasmType::I32(None)));
            }

            for _ in 0..=ft {
                self.instructions
                    .push(Instruction::Local(None, WasmType::F32(None)));
            }

            if let Some(rt) = return_type {
                if SymbolType::Real == rt {
                    self.instructions
                        .push(Instruction::Local(Some("rv"), WasmType::F32(None)));
                } else {
                    self.instructions
                        .push(Instruction::Local(Some("rv"), WasmType::I32(None)));
                }
            }

            if let Some(nr) = self.symbol_table.get_function_ref_count(token.value) {
                // nr tells the maximum number of arguments that are references across all the
                // different function calls that are issued from this function.
                // The local variables that are passed as references are stored in linear
                // memory, the base address of which is stored in the local special variable "refs".
                if 0 < nr {
                    self.instructions
                        .push(Instruction::Local(Some("refs"), WasmType::I32(None)));
                    self.instructions
                        .push(Instruction::Const(WasmType::I32(Some((nr * 4) as i32)))); // 4 bytes per variable
                    self.instructions
                        .push(Instruction::Call(WasmType::Str("allocate")));
                    self.instructions
                        .push(Instruction::SetLocal(WasmType::Str("refs")));
                }
            }

            self.instructions.push(Instruction::BlockBegin(Some("FB")));
            self.block(data.idx);
            self.instructions.push(Instruction::End);

            if return_type.is_some() {
                self.instructions
                    .push(Instruction::GetLocal(WasmType::Str("rv")));
            }

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
            NodeType::Call(_) => self.call_statement(idx),
            NodeType::Declaration(data) => self.declaration(&data),
            NodeType::Return(data) => self.return_statement(&data),
            NodeType::Read(idx) => self.read_statement(idx),
            NodeType::Write(data) => self.write_statement(&data),
            NodeType::If(data) => self.if_statement(&data),
            NodeType::While(data) => self.while_statement(&data),
            _ => assert!(false, "Unexpected node {:#?}.", self.tree[idx]),
        };
    }

    fn assign_statement(&mut self, data: &IdxIdx) {
        if let NodeType::Variable(variable_data) = self.tree[data.idx].data {
            self.emit_set_local_pre_expr(&variable_data);
            self.expression(data.idx2);
            self.emit_set_local_post_expr(&variable_data);
        } else {
            assert!(false, "Unexpected node {:#?}.", self.tree[data.idx]);
        }
    }

    fn assert_statement(&mut self, data: &TokenIdxOptIdx<'a>) {
        self.instructions.push(Instruction::BlockBegin(None));
        self.instructions.push(Instruction::BlockBegin(None));
        self.expression(data.idx);
        self.instructions.push(Instruction::Eqz);
        self.instructions
            .push(Instruction::BrIf(WasmType::I32(Some(0))));
        self.instructions
            .push(Instruction::Br(WasmType::I32(Some(1))));
        self.instructions.push(Instruction::End);
        let idx = data.opt_idx.expect("Assert should have some opt_idx.");
        self.instructions
            .push(Instruction::Const(WasmType::I32(Some(idx as i32))));
        self.instructions
            .push(Instruction::Call(WasmType::Str("get_string_literal")));
        self.instructions
            .push(Instruction::Call(WasmType::Str("write_str")));
        self.instructions.push(Instruction::Unreachable);
        self.instructions.push(Instruction::End);
    }

    fn call_statement(&mut self, idx: usize) {
        let add_ref_idx_to_stack = |stacker: &mut Stacker<'a, 'b>, value: i32| {
            stacker
                .instructions
                .push(Instruction::Const(WasmType::I32(Some(value))));
            stacker
                .instructions
                .push(Instruction::GetLocal(WasmType::Str("refs")));
            stacker
                .instructions
                .push(Instruction::Add(WasmType::I32(None)));
        };

        if let NodeType::Call(data) = self.tree[idx].data {
            let token = data.token.expect("Call statement is missing a token.");
            let is_ref = self
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
                if let NodeType::Variable(var_data) = self.tree[idx].data {
                    // We're passing a locally defined variable to the function (an lvalue)
                    let local_idx = self.get_variable_local_idx(&var_data);
                    let var_token = var_data.token.expect("Variable is missing a token.");
                    match is_ref[i].1 {
                        SymbolType::Bool | SymbolType::Int | SymbolType::Real => {
                            if is_ref[i].0 {
                                if let Some(arr_idx) = var_data.opt_idx {
                                    self.instructions.push(Instruction::GetLocal(WasmType::I32(
                                        Some(local_idx as i32),
                                    )));
                                    self.expression(arr_idx);
                                    // "check_bounds":
                                    // args: arr, idx
                                    // checks that idx is within bounds of arr
                                    // returns idx if yes, unreachable if not
                                    self.instructions
                                        .push(Instruction::Call(WasmType::Str("check_bounds")));
                                    // Get back to stack
                                    self.instructions.push(Instruction::GetLocal(WasmType::I32(
                                        Some(local_idx as i32),
                                    )));
                                    self.instructions
                                        .push(Instruction::Add(WasmType::I32(None)));
                                } else if var_data.b {
                                    // We're passing a reference to a variable that we got
                                    // ourselves as a reference. Just pass the address on.
                                    // The variable at "local_idx" is a parameter we received and
                                    // it contains the address of the variable our caller gave us.
                                    self.instructions.push(Instruction::GetLocal(WasmType::I32(
                                        Some(local_idx as i32),
                                    )));
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
                                        self.instructions.push(Instruction::GetLocal(
                                            WasmType::I32(Some(local_idx as i32)),
                                        ));

                                        // Store the value at the ref index
                                        if SymbolType::Real == is_ref[i].1 {
                                            self.instructions
                                                .push(Instruction::MemStore(WasmType::F32(None)));
                                        } else {
                                            self.instructions
                                                .push(Instruction::MemStore(WasmType::I32(None)));
                                        }

                                        // Get the ref index back to top of stack and pass it to the
                                        // callee.
                                        add_ref_idx_to_stack(self, ref_idx);
                                    }
                                }
                            } else {
                                // We're not passing a reference, but just an ordinary bool int or
                                // real by value. The variable may still be something we got as
                                // reference, but that is handled by "expression".
                                self.expression(idx);
                            }
                        }
                        SymbolType::String
                        | SymbolType::ArrayBool(_)
                        | SymbolType::ArrayInt(_)
                        | SymbolType::ArrayReal(_)
                        | SymbolType::ArrayString(_) => {
                            // Arrays and strings are always passed as pointers,
                            // not as pointers to pointers.
                            if is_ref[i].0 {
                                // If we're passing a reference,
                                // we can just pass the pointer, i.e. the actual address.
                                self.instructions
                                    .push(Instruction::GetLocal(WasmType::I32(Some(
                                        local_idx as i32,
                                    ))));
                            } else {
                                // Allocate a new array/string and pass its address: we don't want
                                // the callee to modify our data, since the array/string is passed
                                // by value.

                                if SymbolType::String == is_ref[i].1 {
                                    // New string:
                                    // allocates space for three i32 values: pointer to data, length and
                                    // capacity. Allocates some capacity at data. Sets length to
                                    // 0.
                                    // Returns a pointer to the first of the three i32 values
                                    self.instructions
                                        .push(Instruction::Call(WasmType::Str("new_string")));
                                } else {
                                    // New array:
                                    // allocates space for three i32 values: pointer to data, length and
                                    // capacity. Allocates some capacity at data. Sets length to
                                    // argument.
                                    // Returns a pointer to the first of the three i32 values
                                    self.instructions
                                        .push(Instruction::Const(WasmType::I32(Some(1024))));
                                    self.instructions
                                        .push(Instruction::Call(WasmType::Str("new_array")));
                                }
                                self.instructions
                                    .push(Instruction::GetLocal(WasmType::I32(Some(
                                        local_idx as i32,
                                    ))));
                                if SymbolType::String == is_ref[i].1 {
                                    self.instructions
                                        .push(Instruction::Call(WasmType::Str("copy_string")));
                                } else {
                                    self.instructions
                                        .push(Instruction::Call(WasmType::Str("copy_array")));
                                }
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
                    if is_ref[i].0 {
                        match is_ref[i].1 {
                            SymbolType::Bool | SymbolType::Int | SymbolType::Real => {
                                // Get the address of this reference to top of stack
                                add_ref_idx_to_stack(self, ref_idx);
                            }
                            _ => {}
                        }
                    }

                    // Get the value of the expression to top of stack
                    self.expression(idx);

                    if is_ref[i].0 {
                        match is_ref[i].1 {
                            SymbolType::Bool | SymbolType::Int | SymbolType::Real => {
                                // Store the value of the expression to the local reference array
                                if SymbolType::Real == is_ref[i].1 {
                                    self.instructions
                                        .push(Instruction::MemStore(WasmType::F32(None)));
                                } else {
                                    self.instructions
                                        .push(Instruction::MemStore(WasmType::I32(None)));
                                }
                                // Get address of stored variable back to stack
                                add_ref_idx_to_stack(self, ref_idx);
                            }
                            _ => {}
                        }
                    }
                }

                ref_idx += is_ref[i].0 as i32;
                i += 1;
                next = self.tree[idx].right_sibling;
            }

            // Call the function with the arguments on stack
            self.instructions
                .push(Instruction::Call(WasmType::Str(token.value)));

            // Store any references back to locals
            next = data.opt_idx;
            while let Some(idx) = next {
                if let NodeType::Variable(var_data) = self.tree[idx].data {
                    if let Some(&idx) = reference_map
                        .get(var_data.token.expect("Variable is missing a token").value)
                    {
                        add_ref_idx_to_stack(self, idx);
                        match var_data.st {
                            SymbolType::Bool | SymbolType::Int => {
                                self.instructions
                                    .push(Instruction::MemLoad(WasmType::I32(None)));
                            }
                            SymbolType::Real => {
                                self.instructions
                                    .push(Instruction::MemLoad(WasmType::F32(None)));
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
                        self.instructions
                            .push(Instruction::SetLocal(WasmType::I32(Some(local_idx as i32))));
                    }
                }
                next = self.tree[idx].right_sibling;
            }

            // If current function has no return type and this call is a statement (= not part of an
            // expression), drop the value from stack.
            match self.tree[self.tree[idx].parent.unwrap()].data {
                NodeType::Block(_) | NodeType::If(_) | NodeType::While(_) => {
                    let return_type = self
                        .symbol_table
                        .get_function_signature(token.value)
                        .expect("Function should have a signature.")
                        .return_type;

                    if return_type.is_some() {
                        self.instructions.push(Instruction::Drop);
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
            if let NodeType::Variable(data) = self.tree[idx].data {
                let local_idx = self.get_variable_local_idx(&data);
                match data.st {
                    SymbolType::Bool | SymbolType::Int => {
                        self.instructions
                            .push(Instruction::Const(WasmType::I32(Some(0))));
                    }
                    SymbolType::Real => {
                        self.instructions
                            .push(Instruction::Const(WasmType::F32(Some(0.0))));
                    }
                    SymbolType::String => {
                        // New string:
                        // allocates space for three i32 values: pointer to data, length and
                        // capacity. Allocates some capacity at data. Sets length to 0.
                        // Returns a pointer to the first of the three i32 values
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("new_string")));
                    }
                    SymbolType::ArrayBool(expr_idx)
                    | SymbolType::ArrayInt(expr_idx)
                    | SymbolType::ArrayReal(expr_idx)
                    | SymbolType::ArrayString(expr_idx) => {
                        // must allocate according to the expr_idx
                        // set e.g. 1024 as max allocation size for safety

                        self.instructions.push(Instruction::BlockBegin(None));
                        self.expression(expr_idx);
                        self.instructions
                            .push(Instruction::Const(WasmType::I32(Some(1024))));
                        self.instructions
                            .push(Instruction::Less(WasmType::I32(None)));
                        self.instructions
                            .push(Instruction::BrIf(WasmType::I32(Some(0))));
                        let str_idx = data
                            .opt_idx
                            .expect("Variable (at declaration) should have some opt_idx.");
                        self.instructions
                            .push(Instruction::Const(WasmType::I32(Some(str_idx as i32))));
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("get_string_literal")));
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("write_str")));
                        self.instructions.push(Instruction::Unreachable);
                        self.instructions.push(Instruction::End);
                        self.expression(expr_idx);
                        // New array:
                        // allocates space for three i32 values: pointer to data, length and
                        // capacity. Allocates some capacity at data. Sets length to expr_idx.
                        // Returns a pointer to the first of the three i32 values
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("new_array")));
                    }
                    _ => {
                        assert!(false, "Unexpected symbol type {:#?}.", data);
                    }
                }
                self.instructions
                    .push(Instruction::SetLocal(WasmType::I32(Some(local_idx as i32))));
            } else {
                break;
            }

            next = self.tree[idx].right_sibling;
        }
    }

    fn return_statement(&mut self, data: &TokenOptIdx<'a>) {
        if let Some(idx) = data.opt_idx {
            self.expression(idx);
            self.instructions
                .push(Instruction::SetLocal(WasmType::Str("rv")));
        }
        self.instructions.push(Instruction::Br(WasmType::Str("FB")));
    }

    fn read_statement(&mut self, idx: usize) {
        self.instructions
            .push(Instruction::Call(WasmType::Str("read_input")));
        let mut next = Some(idx);
        while let Some(idx) = next {
            if let NodeType::Variable(variable_data) = self.tree[idx].data {
                self.emit_set_local_pre_expr(&variable_data);
                match variable_data.st {
                    SymbolType::Bool => {
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("bool_from_input")));
                    }
                    SymbolType::Int => {
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("i32_from_input")));
                    }
                    SymbolType::Real => {
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("f32_from_input")));
                    }
                    SymbolType::String => {
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("string_from_input")));
                    }
                    SymbolType::ArrayBool(_)
                    | SymbolType::ArrayInt(_)
                    | SymbolType::ArrayReal(_)
                    | SymbolType::ArrayString(_)
                    | SymbolType::Undefined => {
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
                SymbolType::Bool => {
                    self.instructions
                        .push(Instruction::Call(WasmType::Str("write_bool")));
                }
                SymbolType::Int => {
                    self.instructions
                        .push(Instruction::Call(WasmType::Str("write_i32")));
                }
                SymbolType::Real => {
                    self.instructions
                        .push(Instruction::Call(WasmType::Str("write_f32")));
                }
                SymbolType::String => {
                    self.instructions
                        .push(Instruction::Call(WasmType::Str("write_string")));
                }
                SymbolType::ArrayBool(_)
                | SymbolType::ArrayInt(_)
                | SymbolType::ArrayReal(_)
                | SymbolType::ArrayString(_) => {
                    self.instructions
                        .push(Instruction::Call(WasmType::Str("write_i32")));
                }
                SymbolType::Undefined => {
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
            self.instructions.push(Instruction::BlockBegin(None));
            self.instructions.push(Instruction::BlockBegin(None));
        }
        self.instructions.push(Instruction::BlockBegin(None));
        self.expression(data.idx);
        self.instructions.push(Instruction::Eqz);
        self.instructions
            .push(Instruction::BrIf(WasmType::I32(Some(br_if_label))));
        self.statement(data.idx2);
        self.instructions.push(Instruction::End);
        if has_else {
            self.instructions
                .push(Instruction::Br(WasmType::I32(Some(1))));
            self.instructions.push(Instruction::End);
            self.statement(data.opt_idx.unwrap());
            self.instructions.push(Instruction::End);
        }
    }

    fn while_statement(&mut self, data: &TokenIdxIdx<'a>) {
        self.instructions.push(Instruction::BlockBegin(None));
        self.instructions.push(Instruction::LoopBegin(None));
        self.expression(data.idx);
        self.instructions
            .push(Instruction::BrIf(WasmType::I32(Some(1))));
        self.statement(data.idx2);
        self.instructions
            .push(Instruction::Br(WasmType::I32(Some(0))));
        self.instructions.push(Instruction::End);
        self.instructions.push(Instruction::End);
    }

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
                            self.instructions.push(Instruction::Eq(WasmType::I32(None)));
                        }
                        TokenType::OperatorNotEqual => {
                            self.instructions
                                .push(Instruction::NEq(WasmType::I32(None)));
                        }
                        TokenType::OperatorGreater => {
                            self.instructions
                                .push(Instruction::Great(WasmType::I32(None)));
                        }
                        TokenType::OperatorGreaterEqual => {
                            self.instructions
                                .push(Instruction::GreatEq(WasmType::I32(None)));
                        }
                        TokenType::OperatorLess => {
                            self.instructions
                                .push(Instruction::Less(WasmType::I32(None)));
                        }
                        TokenType::OperatorLessEqual => {
                            self.instructions
                                .push(Instruction::LessEq(WasmType::I32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    SymbolType::Int => match token.token_type {
                        TokenType::OperatorEqual => {
                            self.instructions.push(Instruction::Eq(WasmType::I32(None)));
                        }
                        TokenType::OperatorNotEqual => {
                            self.instructions
                                .push(Instruction::NEq(WasmType::I32(None)));
                        }
                        TokenType::OperatorGreater => {
                            self.instructions
                                .push(Instruction::Great(WasmType::I32(None)));
                        }
                        TokenType::OperatorGreaterEqual => {
                            self.instructions
                                .push(Instruction::GreatEq(WasmType::I32(None)));
                        }
                        TokenType::OperatorLess => {
                            self.instructions
                                .push(Instruction::Less(WasmType::I32(None)));
                        }
                        TokenType::OperatorLessEqual => {
                            self.instructions
                                .push(Instruction::LessEq(WasmType::I32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    SymbolType::Real => match token.token_type {
                        TokenType::OperatorEqual => {
                            self.instructions.push(Instruction::Eq(WasmType::F32(None)));
                        }
                        TokenType::OperatorNotEqual => {
                            self.instructions
                                .push(Instruction::NEq(WasmType::F32(None)));
                        }
                        TokenType::OperatorGreater => {
                            self.instructions
                                .push(Instruction::Great(WasmType::F32(None)));
                        }
                        TokenType::OperatorGreaterEqual => {
                            self.instructions
                                .push(Instruction::GreatEq(WasmType::F32(None)));
                        }
                        TokenType::OperatorLess => {
                            self.instructions
                                .push(Instruction::Less(WasmType::F32(None)));
                        }
                        TokenType::OperatorLessEqual => {
                            self.instructions
                                .push(Instruction::LessEq(WasmType::F32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                        }
                    },
                    SymbolType::String => match token.token_type {
                        TokenType::OperatorEqual => {
                            self.instructions
                                .push(Instruction::Call(WasmType::Str("string_eq")));
                        }
                        TokenType::OperatorNotEqual => {
                            self.instructions
                                .push(Instruction::Call(WasmType::Str("string_neq")));
                        }
                        TokenType::OperatorGreater => {
                            self.instructions
                                .push(Instruction::Call(WasmType::Str("string_great")));
                        }
                        TokenType::OperatorGreaterEqual => {
                            self.instructions
                                .push(Instruction::Call(WasmType::Str("string_great_eq")));
                        }
                        TokenType::OperatorLess => {
                            self.instructions
                                .push(Instruction::Call(WasmType::Str("string_less")));
                        }
                        TokenType::OperatorLessEqual => {
                            self.instructions
                                .push(Instruction::Call(WasmType::Str("string_less_eq")));
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
                        TokenType::OperatorPlus => match st {
                            SymbolType::Int => {
                                self.instructions
                                    .push(Instruction::Add(WasmType::I32(None)));
                            }
                            SymbolType::Real => {
                                self.instructions
                                    .push(Instruction::Add(WasmType::F32(None)));
                            }
                            SymbolType::String => {
                                self.instructions
                                    .push(Instruction::Call(WasmType::Str("string_concatenate")));
                            }
                            _ => {
                                assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                            }
                        },
                        TokenType::OperatorMinus => match st {
                            SymbolType::Int => {
                                self.instructions
                                    .push(Instruction::Sub(WasmType::I32(None)));
                            }
                            SymbolType::Real => {
                                self.instructions
                                    .push(Instruction::Sub(WasmType::F32(None)));
                            }
                            _ => {
                                assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                            }
                        },
                        TokenType::OperatorOr => {
                            self.instructions.push(Instruction::Or(WasmType::I32(None)));
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
                        TokenType::OperatorMinus => match st {
                            SymbolType::Int => {
                                self.instructions
                                    .push(Instruction::Const(WasmType::I32(Some(-1))));
                                self.instructions
                                    .push(Instruction::Mul(WasmType::I32(None)));
                            }
                            SymbolType::Real => {
                                self.instructions
                                    .push(Instruction::Const(WasmType::F32(Some(-1.0))));
                                self.instructions
                                    .push(Instruction::Mul(WasmType::I32(None)));
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
            NodeType::MulOp(data) => {
                let token = data.token.expect("Multiply operator is missing a token.");
                let st = data.st;
                self.expression(data.idx);
                self.expression(data.idx2);

                match token.token_type {
                    TokenType::OperatorMultiply => match st {
                        SymbolType::Int => {
                            self.instructions
                                .push(Instruction::Mul(WasmType::I32(None)));
                        }
                        SymbolType::Real => {
                            self.instructions
                                .push(Instruction::Mul(WasmType::F32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                        }
                    },
                    TokenType::OperatorDivide => match st {
                        SymbolType::Int => {
                            self.instructions
                                .push(Instruction::Div(WasmType::I32(None)));
                        }
                        SymbolType::Real => {
                            self.instructions
                                .push(Instruction::Div(WasmType::F32(None)));
                        }
                        _ => {
                            assert!(false, "Unexpected symbol type {:#?}.", self.tree[idx]);
                        }
                    },
                    TokenType::OperatorModulo => {
                        self.instructions
                            .push(Instruction::Mod(WasmType::I32(None)));
                    }
                    TokenType::OperatorAnd => {
                        self.instructions
                            .push(Instruction::And(WasmType::I32(None)));
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", self.tree[idx]);
                    }
                }
            }
            NodeType::Variable(data) => {
                let local_idx = self.get_variable_local_idx(&data);
                self.instructions
                    .push(Instruction::GetLocal(WasmType::I32(Some(local_idx as i32))));
                match data.st {
                    SymbolType::Bool | SymbolType::Int => {
                        if data.b {
                            self.instructions
                                .push(Instruction::MemLoad(WasmType::I32(None)));
                        }
                    }
                    SymbolType::Real => {
                        if data.b {
                            self.instructions
                                .push(Instruction::MemLoad(WasmType::F32(None)));
                        }
                    }
                    SymbolType::String => {
                        // string references pass their actual address, non-references pass the
                        // address of a copy. In either case, the value already points to the data.
                    }
                    SymbolType::ArrayBool(_)
                    | SymbolType::ArrayInt(_)
                    | SymbolType::ArrayReal(_)
                    | SymbolType::ArrayString(_) => {
                        // array references pass their actual address, non-references pass the
                        // address of a copy. In either case, the value already points to the data.

                        if let Some(arr_idx) = data.opt_idx {
                            self.expression(arr_idx);
                            // args: arr, idx
                            self.instructions
                                .push(Instruction::Call(WasmType::Str("array_access")));
                        }
                    }
                    _ => {
                        assert!(false, "Unexpected symbol type {:#?}", data);
                    }
                }
            }
            NodeType::Literal(data) => {
                let token = data.token.unwrap();
                match token.token_type {
                    TokenType::LiteralBoolean => {
                        if "true" == token.value {
                            self.instructions
                                .push(Instruction::Const(WasmType::I32(Some(1))));
                        } else {
                            self.instructions
                                .push(Instruction::Const(WasmType::I32(Some(0))));
                        }
                    }
                    TokenType::LiteralInt => {
                        let literal = token
                            .value
                            .parse::<i32>()
                            .expect("Literal int str should be possible to parse to an integer.");
                        self.instructions
                            .push(Instruction::Const(WasmType::I32(Some(literal))));
                    }
                    TokenType::LiteralReal => {
                        let literal = token
                            .value
                            .parse::<f32>()
                            .expect("Literal real str should be possible to parse to a float.");
                        self.instructions
                            .push(Instruction::Const(WasmType::F32(Some(literal))));
                    }
                    TokenType::LiteralString => {
                        // The idx is the "ranking" of the string literal. In other words, if this
                        // is the fifth literal string that is found in the program, idx is 5.
                        // Literal strings are stored consecutively in memory and the accessing
                        // functions uses this idx to find the address of the string literal.
                        let str_idx = data
                            .opt_idx
                            .expect("Literal string should have some opt_idx.");
                        self.instructions
                            .push(Instruction::Const(WasmType::I32(Some(str_idx as i32))));
                        self.instructions
                            .push(Instruction::Call(WasmType::Str("get_string_literal")));
                    }
                    _ => {
                        assert!(false, "Unexpected token {:#?}.", data);
                    }
                }
            }
            NodeType::Not(data) => {
                self.expression(data.idx);
                self.instructions.push(Instruction::Eqz);
            }
            NodeType::ArraySize(data) => {
                self.expression(data.idx);
                self.instructions
                    .push(Instruction::Call(WasmType::Str("array_size")));
            }
            NodeType::Call(_) => self.call_statement(idx),
            _ => {
                assert!(false, "Unexpected node {:#?}.", self.tree[idx]);
            }
        }
    }

    fn get_variable_local_idx(&mut self, data: &TokenSymbolBoolIdxIdxOptIdx<'a>) -> usize {
        data.idx
            + self.symbol_table.get_variable_index(
                self.fname
                    .expect("Function must have a name at this point."),
                data.st,
                data.idx2,
            )
    }

    fn emit_set_local_pre_expr(&mut self, data: &TokenSymbolBoolIdxIdxOptIdx<'a>) {
        let local_idx = self.get_variable_local_idx(&data);
        match data.st {
            SymbolType::Bool | SymbolType::Int | SymbolType::Real => {
                if data.b {
                    self.instructions
                        .push(Instruction::GetLocal(WasmType::I32(Some(local_idx as i32))));
                }
            }
            SymbolType::String => {
                self.instructions
                    .push(Instruction::GetLocal(WasmType::I32(Some(local_idx as i32))));
            }
            SymbolType::ArrayBool(_)
            | SymbolType::ArrayInt(_)
            | SymbolType::ArrayReal(_)
            | SymbolType::ArrayString(_) => {
                self.instructions
                    .push(Instruction::GetLocal(WasmType::I32(Some(local_idx as i32))));

                if let Some(expr_idx) = data.opt_idx {
                    self.expression(expr_idx);
                }
            }
            _ => {
                assert!(false, "Unexpected symbol type {:#?}", data);
            }
        }
    }

    fn emit_set_local_post_expr(&mut self, data: &TokenSymbolBoolIdxIdxOptIdx<'a>) {
        match data.st {
            SymbolType::Bool | SymbolType::Int | SymbolType::Real => {
                if data.b {
                    if SymbolType::Real == data.st {
                        self.instructions
                            .push(Instruction::MemStore(WasmType::F32(None)));
                    } else {
                        self.instructions
                            .push(Instruction::MemStore(WasmType::I32(None)));
                    }
                } else {
                    let local_idx = self.get_variable_local_idx(&data);
                    self.instructions
                        .push(Instruction::SetLocal(WasmType::I32(Some(local_idx as i32))));
                }
            }
            SymbolType::String => {
                // args: dst_str, src_str
                // must perform a deep copy
                // if enough capacity, copy, if not, allocate new
                // returns: dst_str
                self.instructions
                    .push(Instruction::Call(WasmType::Str("copy_string")));
                self.instructions.push(Instruction::Drop);
            }
            SymbolType::ArrayBool(_)
            | SymbolType::ArrayInt(_)
            | SymbolType::ArrayReal(_)
            | SymbolType::ArrayString(_) => {
                if data.opt_idx.is_some() {
                    // args: arr, idx, value
                    self.instructions
                        .push(Instruction::Call(WasmType::Str("array_assign")));
                } else {
                    // args: dst_arr, src_arr
                    // must perform a deep copy
                    // if enough capacity, copy, if not, allocate new
                    // returns: dst_arr
                    self.instructions
                        .push(Instruction::Call(WasmType::Str("copy_array")));
                    self.instructions.push(Instruction::Drop);
                }
            }
            _ => {
                assert!(false, "Unexpected symbol type {:#?}", data);
            }
        }
    }

    pub fn new(tree: &'b LcRsTree<NodeType<'a>>, symbol_table: &'b mut SymbolTable<'a>) -> Self {
        Stacker {
            tree: tree,
            symbol_table: symbol_table,
            fname: None,
            instructions: vec![],
        }
    }
}

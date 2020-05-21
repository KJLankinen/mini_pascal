use super::data_types::{Instruction, WasmType};
use super::symbol_table::SymbolTable;

pub struct Wasmer<'a, 'b> {
    instructions: &'b Vec<Instruction<'a>>,
    wasm_string: &'b mut String,
    symbol_table: &'b SymbolTable<'a>,
    next_free_byte: usize,
    stat_str_byte: usize,
    imported_contents: &'b str,
    lib_name: &'a str,
}

impl<'a, 'b> Wasmer<'a, 'b> {
    pub fn instructions_to_wasm(&mut self) {
        // This function converts the IR instruction stack to wasm instructions
        // and pushes them to a string.
        let mut identation: i32 = 0;
        for instr in self.instructions {
            let mut start_from_new_line = true;
            let mut change_identation = 0;
            let temp_string = match instr {
                Instruction::ProgramBegin(name) => {
                    start_from_new_line = false;
                    change_identation = 1;
                    format!("(module ${}", name)
                }
                Instruction::FunctionBegin(name) => {
                    change_identation = 1;
                    format!("(func ${} (export \"{}\") ", name, name)
                }
                Instruction::BlockBegin(opt_name) => {
                    change_identation = 1;
                    if let Some(name) = opt_name {
                        format!("(block ${}", name)
                    } else {
                        format!("(block ")
                    }
                }
                Instruction::LoopBegin(opt_name) => {
                    change_identation = 1;
                    if let Some(name) = opt_name {
                        format!("(loop ${}", name)
                    } else {
                        format!("(loop ")
                    }
                }
                Instruction::End => {
                    change_identation = -1;
                    start_from_new_line = false;
                    String::from(")")
                }
                Instruction::DataSegment => self.literal_string_segment(),
                Instruction::Imports => self.imports(),
                Instruction::GlobalPointers => self.global_store(),
                Instruction::Param(wtype) => {
                    start_from_new_line = false;
                    format!("(param {}) ", wtype)
                }
                Instruction::Result(wtype) => {
                    start_from_new_line = false;
                    format!("(result {})", wtype)
                }
                Instruction::Local(opt_name, wtype) => {
                    if let Some(name) = opt_name {
                        format!("(local ${} {})", name, wtype)
                    } else {
                        format!("(local {})", wtype)
                    }
                }
                Instruction::GetLocal(wtype) => match wtype {
                    WasmType::I32(s) => format!(
                        "local.get {}",
                        s.expect("GetLocal with I32 should contain some i32 value.")
                    ),
                    WasmType::F32(_) => {
                        assert!(false, "GetLocal should not contain a F32 value.");
                        String::from("")
                    }
                    WasmType::Str(s) => format!("local.get ${}", s),
                },
                Instruction::SetLocal(wtype) => match wtype {
                    WasmType::I32(s) => format!(
                        "local.set {}",
                        s.expect("SetLocal with I32 should contain some i32 value.")
                    ),
                    WasmType::F32(_) => {
                        assert!(false, "SetLocal should not contain a F32 value.");
                        String::from("")
                    }
                    WasmType::Str(s) => format!("local.set ${}", s),
                },
                Instruction::MemLoad(wtype) => format!("{}.load", wtype),
                Instruction::MemStore(wtype) => format!("{}.store", wtype),
                Instruction::Const(wtype) => match wtype {
                    WasmType::I32(s) => format!(
                        "{}.const {}",
                        wtype,
                        s.expect("Const should contain some value.")
                    ),
                    WasmType::F32(s) => format!(
                        "{}.const {}",
                        wtype,
                        s.expect("Const should contain some value.")
                    ),
                    WasmType::Str(_) => {
                        assert!(false, "Const should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::Call(wtype) => match wtype {
                    WasmType::I32(s) => {
                        format!("call {}", s.expect("Call I32 should contain some value."))
                    }
                    WasmType::F32(_) => {
                        assert!(false, "Call should not contain a F32 value.");
                        String::from("")
                    }
                    WasmType::Str(s) => format!("call ${}", s),
                },
                Instruction::LibFunc(name) => format!("call ${}", name),
                Instruction::Eqz => format!("i32.eqz"),
                Instruction::Br(wtype) => match wtype {
                    WasmType::I32(s) => {
                        format!("br {}", s.expect("Br I32 should contain some value."))
                    }
                    WasmType::F32(_) => {
                        assert!(false, "Br should not contain a F32 value.");
                        String::from("")
                    }
                    WasmType::Str(s) => format!("br ${}", s),
                },
                Instruction::BrIf(wtype) => match wtype {
                    WasmType::I32(s) => {
                        format!("br_if {}", s.expect("BrIf I32 should contain some value."))
                    }
                    WasmType::F32(_) => {
                        assert!(false, "BrIf should not contain a F32 value.");
                        String::from("")
                    }
                    WasmType::Str(s) => format!("br_if ${}", s),
                },
                Instruction::Unreachable => format!("unreachable"),
                Instruction::Drop => format!("drop"),
                Instruction::Eq(wtype) => match wtype {
                    WasmType::I32(_) | WasmType::F32(_) => format!("{}.eq", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "Eq should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::NEq(wtype) => match wtype {
                    WasmType::I32(_) | WasmType::F32(_) => format!("{}.ne", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "NEq should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::GreatEq(wtype) => match wtype {
                    WasmType::I32(_) => format!("{}.ge_s", wtype),
                    WasmType::F32(_) => format!("{}.ge", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "GreatEq should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::Great(wtype) => match wtype {
                    WasmType::I32(_) => format!("{}.gt_s", wtype),
                    WasmType::F32(_) => format!("{}.gt", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "Great should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::LessEq(wtype) => match wtype {
                    WasmType::I32(_) => format!("{}.le_s", wtype),
                    WasmType::F32(_) => format!("{}.le", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "LessEq should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::Less(wtype) => match wtype {
                    WasmType::I32(_) => format!("{}.lt_s", wtype),
                    WasmType::F32(_) => format!("{}.lt", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "Less should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::Add(wtype) => match wtype {
                    WasmType::I32(_) | WasmType::F32(_) => format!("{}.add", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "Add should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::Sub(wtype) => match wtype {
                    WasmType::I32(_) | WasmType::F32(_) => format!("{}.sub", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "Sub should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::Mul(wtype) => match wtype {
                    WasmType::I32(_) | WasmType::F32(_) => format!("{}.mul", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "Mul should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::Div(wtype) => match wtype {
                    WasmType::I32(_) => format!("{}.div_s", wtype),
                    WasmType::F32(_) => format!("{}.div", wtype),
                    WasmType::Str(_) => {
                        assert!(false, "Div should not contain a Str value.");
                        String::from("")
                    }
                },
                Instruction::Mod(wtype) => match wtype {
                    WasmType::I32(_) => format!("{}.rem_s", wtype),
                    WasmType::F32(_) | WasmType::Str(_) => {
                        assert!(false, "Mod should not contain a F32 or a Str value.");
                        String::from("")
                    }
                },
                Instruction::And(wtype) => match wtype {
                    WasmType::I32(_) => format!("{}.and", wtype),
                    WasmType::F32(_) | WasmType::Str(_) => {
                        assert!(false, "And should not contain a F32 or a Str value.");
                        String::from("")
                    }
                },
                Instruction::Or(wtype) => match wtype {
                    WasmType::I32(_) => format!("{}.or", wtype),
                    WasmType::F32(_) | WasmType::Str(_) => {
                        assert!(false, "Or should not contain a F32 or a Str value.");
                        String::from("")
                    }
                },
            };

            if start_from_new_line {
                self.wasm_string.push_str("\n");
                self.wasm_string
                    .push_str(" ".repeat(identation as usize).as_str());
            }
            self.wasm_string.push_str(temp_string.as_str());

            if 0 != change_identation {
                identation += change_identation * 2;
                assert!(identation >= 0, "Identation can't be negative.");
            }
        }
        self.wasm_string.push_str("\n");
    }

    fn literal_string_segment(&mut self) -> String {
        // This function write the literal string data segment to a string that's ready to be
        // written to a wast file. It takes the start of the string data segment as input and
        // returns the location as output. Naturally both values must be in bytes.
        let swap_bytes = |v: i32| -> i32 {
            let mut swapped = 0;
            swapped = swapped | (0xff & v) << 24;
            swapped = swapped | (0xff << 8 & v) << 8;
            swapped = swapped | (0xff << 16 & v) >> 8;
            swapped = swapped | (0xff << 24 & v) >> 24;
            swapped
        };

        let mut data_segment = String::new();
        let num_strings = self.symbol_table.borrow_string_literal_bytes().len();
        if 0 < num_strings {
            // Get the pointer to the first free byte so we may write the static string data to that
            // address. It is (should be) stored in the library after the magic word 'RUST_PARSE_DONT_REMOVE'
            let magic_word = "RUST_PARSE_DONT_REMOVE";
            for line in self.imported_contents.lines() {
                if line.contains(magic_word) {
                    self.next_free_byte = line.split(magic_word).collect::<Vec<&str>>()[1]
                        .parse::<usize>()
                        .expect("This should be a valid number.");
                }
            }

            self.stat_str_byte = self.next_free_byte;

            data_segment.push_str("(data (i32.const ");
            data_segment.push_str(self.next_free_byte.to_string().as_str());
            data_segment.push_str(") \"");

            // Write pdata (= "start"), len (= "len"), capacity (= len) and stride (= 1) as bytes in little endian order.
            // "start" is the actual, raw address in linear memory where the string data recides.
            for (start, len) in self.symbol_table.borrow_string_literal_bytes() {
                let start = (*start + self.next_free_byte + 16 * num_strings) as i32;
                format!("{:08X}", swap_bytes(start))
                    .chars()
                    .chain(format!("{:08X}", swap_bytes(*len as i32)).chars())
                    .chain(format!("{:08X}", swap_bytes(*len as i32)).chars())
                    .chain(format!("{:08X}", swap_bytes(1 as i32)).chars())
                    .enumerate()
                    .for_each(|(i, c)| {
                        if 0 == i % 2 {
                            data_segment.push_str("\\");
                        }
                        data_segment.push(c);
                    });
            }

            // Change " to \" inside the literals
            self.symbol_table
                .borrow_string_literals()
                .chars()
                .for_each(|c| {
                    if '\"' == c {
                        data_segment.push_str("\\");
                    }
                    data_segment.push(c);
                });
            data_segment.push_str("\")");
        }

        self.next_free_byte = self.next_free_byte + data_segment.as_bytes().len();
        data_segment
    }

    fn imports(&mut self) -> String {
        // This function goes through the library file and imports all the functions that the
        // compiled program uses.
        let mut imports_string = String::new();
        if 0 < self.symbol_table.library_functions.len() {
            // Import memory
            imports_string.push_str("(import \"");
            imports_string.push_str(self.lib_name);
            imports_string.push_str("\" \"memory\" (memory 0))");

            // Import globals
            imports_string.push_str("\n  (import \"");
            imports_string.push_str(self.lib_name);
            imports_string.push_str("\" \"global\" (global $dyn_mem_ptr))");

            imports_string.push_str("\n  (import \"");
            imports_string.push_str(self.lib_name);
            imports_string.push_str("\" \"global\" (global $static_str_ptrs))");

            imports_string.push_str("\n  (import \"");
            imports_string.push_str(self.lib_name);
            imports_string.push_str("\" \"global\" (global $offset_length))");

            for fname in &self.symbol_table.library_functions {
                if let Some(line) = self.imported_contents.lines().find(|l| l.contains(fname)) {
                    imports_string.push_str("\n  (import \"");
                    imports_string.push_str(self.lib_name);
                    imports_string.push_str("\" \"");
                    imports_string.push_str(fname);
                    imports_string.push_str("\" (func $");
                    imports_string.push_str(fname);

                    let mut result_idx = None;
                    if let Some(start) = line.find("param") {
                        imports_string.push_str(" (param");
                        result_idx = line.find("result");
                        let line = match result_idx {
                            Some(idx) => &line[start..idx],
                            None => &line[start..line.len()],
                        };

                        for s in line.split_ascii_whitespace() {
                            if s.contains("i32") {
                                imports_string.push_str(" i32");
                            } else if s.contains("f32") {
                                imports_string.push_str(" f32");
                            }
                        }

                        imports_string.push(')');
                    }

                    if let Some(idx) = result_idx {
                        imports_string.push_str(" (result");
                        let line = &line[idx..line.len()];

                        if line.contains("i32") {
                            imports_string.push_str(" i32");
                        } else if line.contains("f32") {
                            imports_string.push_str(" f32");
                        }

                        imports_string.push(')');
                    }

                    imports_string.push(')');
                    imports_string.push(')');
                }
            }
        }

        imports_string
    }

    fn global_store(&mut self) -> String {
        let mut store_string = String::new();
        // dynamic memory pointer
        store_string.push_str("global.get $dyn_mem_ptr");
        store_string.push_str("\n    i32.const ");
        store_string.push_str(self.next_free_byte.to_string().as_str());
        store_string.push_str("\n    i32.store");

        // static string pointers
        store_string.push_str("\n    global.get $static_str_ptrs");
        store_string.push_str("\n    i32.const ");
        store_string.push_str(self.stat_str_byte.to_string().as_str());
        store_string.push_str("\n    i32.store");

        // number of static strings
        store_string.push_str("\n    global.get $static_str_ptrs");
        store_string.push_str("\n    global.get $offset_length");
        store_string.push_str("\n    i32.add");
        store_string.push_str("\n    i32.const ");
        store_string.push_str(
            self.symbol_table
                .borrow_string_literal_bytes()
                .len()
                .to_string()
                .as_str(),
        );
        store_string.push_str("\n    i32.store");
        store_string
    }

    pub fn new(
        instructions: &'b Vec<Instruction<'a>>,
        wasm_string: &'b mut String,
        symbol_table: &'b SymbolTable<'a>,
        imported_contents: &'b str,
        lib_name: &'a str,
    ) -> Self {
        Wasmer {
            instructions: instructions,
            wasm_string: wasm_string,
            symbol_table: symbol_table,
            next_free_byte: 0,
            stat_str_byte: 0,
            imported_contents: imported_contents,
            lib_name: lib_name,
        }
    }
}

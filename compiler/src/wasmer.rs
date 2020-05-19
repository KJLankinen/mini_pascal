use super::data_types::{Instruction, WasmType};
use super::symbol_table::SymbolTable;

pub struct Wasmer<'a, 'b> {
    instructions: &'b Vec<Instruction<'a>>,
    wasm_string: &'b mut String,
    symbol_table: &'b SymbolTable<'a>,
    next_free_byte: usize,
    _imported_contents: &'b str,
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
            swapped = swapped | (0xff << 8 & v) << 16;
            swapped = swapped | (0xff << 16 & v) << 8;
            swapped = swapped | (0xff << 24 & v);
            swapped
        };

        let mut data_segment = String::new();
        let num_strings = self.symbol_table.borrow_string_literal_bytes().len();
        if 0 < num_strings {
            data_segment.push_str("(data (i32.const ");
            data_segment.push_str(self.next_free_byte.to_string().as_str());
            data_segment.push_str(") \"");

            // Write the "start" and "len" numbers as bytes in little endian order.
            // "start" is the actual, raw address in linear memory where the string data recides.
            for (start, len) in self.symbol_table.borrow_string_literal_bytes() {
                format!(
                    "{:08X}",
                    swap_bytes((*start + self.next_free_byte + 8 * num_strings) as i32)
                )
                .chars()
                .chain(format!("{:08X}", swap_bytes(*len as i32)).chars())
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

    pub fn new(
        instructions: &'b Vec<Instruction<'a>>,
        wasm_string: &'b mut String,
        symbol_table: &'b SymbolTable<'a>,
        imported_contents: &'b str,
    ) -> Self {
        // TODO: get next_free_byte from somewhere
        Wasmer {
            instructions: instructions,
            wasm_string: wasm_string,
            symbol_table: symbol_table,
            next_free_byte: 0,
            _imported_contents: imported_contents,
        }
    }
}

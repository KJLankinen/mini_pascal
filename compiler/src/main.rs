use compiler::read_program_to_string;
use compiler::Scanner;
use compiler::TokenType;
use std::env;
use std::process;

struct Parser<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Parser<'a> {
    pub fn parse(&mut self) {
        self.process_program();
    }

    fn process_program(&mut self) {
        self.process_statement_list();
        assert!(
            TokenType::EndOfProgram == self.scanner.step().unwrap().token_type,
            "{:?}",
            self.scanner.current_token.unwrap()
        );
    }

    fn process_statement_list(&mut self) {
        self.process_statement();
        self.process_end_of_statement();

        if TokenType::EndOfProgram != self.scanner.next_token.unwrap().token_type
            && "end" != self.scanner.next_token.unwrap().value
        {
            self.process_statement_list();
        }
    }

    fn process_statement(&mut self) {
        let token = self.scanner.step().unwrap();
        match token.token_type {
            TokenType::Keyword => match token.value {
                "var" => {
                    assert!(
                        TokenType::Identifier == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    assert!(
                        TokenType::TypeSeparator == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_type();

                    if TokenType::Assignment == self.scanner.next_token.unwrap().token_type {
                        self.scanner.step();
                        self.process_expression();
                    }
                }
                "for" => {
                    assert!(
                        TokenType::Identifier == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    assert!(
                        "in" == self.scanner.step().unwrap().value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_expression();
                    assert!(
                        TokenType::Range == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_expression();
                    assert!(
                        "do" == self.scanner.step().unwrap().value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_statement_list();
                    assert!(
                        "end" == self.scanner.step().unwrap().value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    assert!(
                        "for" == self.scanner.step().unwrap().value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                }
                "read" => {
                    assert!(
                        TokenType::Identifier == self.scanner.step().unwrap().token_type,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                }
                "print" => {
                    self.process_expression();
                }
                "assert" => {
                    let token = self.scanner.step().unwrap();
                    assert!(
                        "(" == token.value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                    self.process_expression();
                    let token = self.scanner.step().unwrap();
                    assert!(
                        ")" == token.value,
                        "{:?}",
                        self.scanner.current_token.unwrap()
                    );
                }
                _ => {
                    assert!(
                        false,
                        "Unhandled statement case {:?}",
                        self.scanner.current_token.unwrap()
                    );
                }
            },
            TokenType::Identifier => {
                let token = self.scanner.step().unwrap();
                assert!(
                    TokenType::Assignment == token.token_type,
                    "{:?}",
                    self.scanner.current_token.unwrap()
                );
                self.process_expression();
            }
            _ => assert!(false, "{:?}", self.scanner.current_token.unwrap()),
        }
    }

    fn process_end_of_statement(&mut self) {
        assert!(
            TokenType::EndOfStatement == self.scanner.step().unwrap().token_type,
            "{:?}",
            self.scanner.current_token.unwrap()
        );
    }

    fn process_type(&mut self) {
        let token = self.scanner.step().unwrap();
        assert!(
            TokenType::Keyword == token.token_type,
            "{:?}",
            self.scanner.current_token.unwrap()
        );
        assert!(
            "int" == token.value || "string" == token.value || "bool" == token.value,
            "{:?}",
            self.scanner.current_token.unwrap()
        );
    }

    fn process_expression(&mut self) {
        if TokenType::Operator == self.scanner.next_token.unwrap().token_type {
            let token = self.scanner.step().unwrap();
            assert!(
                "!" == token.value,
                "{:?}",
                self.scanner.current_token.unwrap()
            );
            self.process_operand();
        } else {
            self.process_operand();
            if TokenType::Operator == self.scanner.next_token.unwrap().token_type {
                let token = self.scanner.step().unwrap();
                // Need to take into account operator overloading
                // and suitable operands on both sides
                assert!(
                    TokenType::Operator == token.token_type,
                    "{:?}",
                    self.scanner.current_token.unwrap()
                );
                self.process_operand();
            }
        }
    }

    fn process_operand(&mut self) {
        let token = self.scanner.step().unwrap();
        if TokenType::Paren == token.token_type {
            assert!(
                "(" == token.value,
                "{:?}",
                self.scanner.current_token.unwrap()
            );
            self.process_expression();
            let token = self.scanner.step().unwrap();
            assert!(
                ")" == token.value,
                "{:?}",
                self.scanner.current_token.unwrap()
            );
        } else {
            assert!(
                TokenType::IntLiteral == token.token_type
                    || TokenType::StrLiteral == token.token_type
                    || TokenType::Identifier == token.token_type
                    || TokenType::BoolLiteral == token.token_type,
                "{:?}",
                self.scanner.current_token.unwrap()
            );
        }
    }
}

fn run() {
    let source_str = match read_program_to_string(env::args().collect()) {
        Ok(source_str) => source_str,
        Err(err) => {
            eprintln!("Application error: {}", err);
            process::exit(1);
        }
    };

    let mut parser = Parser {
        scanner: Scanner {
            column: 0,
            line: 1,
            chars: source_str.char_indices().peekable(),
            source_str: &source_str,
            current_token: None,
            next_token: None,
            keywords: [
                ("var", true),
                ("for", true),
                ("end", true),
                ("in", true),
                ("do", true),
                ("read", true),
                ("print", true),
                ("int", true),
                ("string", true),
                ("bool", true),
                ("assert", true),
            ]
            .iter()
            .cloned()
            .collect(),
        },
    };

    parser.parse();
}

fn main() {
    run();
}

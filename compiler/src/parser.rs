use super::scanner::Scanner;
use super::scanner::TokenType;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
}

// A recursive descent parser
impl<'a> Parser<'a> {
    pub fn new(source_str: &'a str) -> Parser<'a> {
        Parser {
            scanner: Scanner::new(source_str),
        }
    }

    pub fn parse(&mut self) {
        self.process_program();
    }

    fn process_program(&mut self) {
        self.process_statement_list();
        assert!(
            TokenType::EndOfProgram == self.scanner.next().unwrap().token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );
    }

    fn process_statement_list(&mut self) {
        let token = self.scanner.peek().expect("Next token is None.");
        // Stop if we've reached end of program or the end of 'for' block
        if TokenType::EndOfProgram != token.token_type && TokenType::KeywordEnd != token.token_type
        {
            self.process_statement();
            self.process_statement_list();
        }
    }

    fn process_statement(&mut self) {
        self.process_statement_prefix();
        self.process_end_of_statement();
    }

    fn process_statement_prefix(&mut self) {
        let token = self.scanner.next().unwrap();
        match token.token_type {
            TokenType::KeywordVar => {
                assert!(
                    TokenType::Identifier == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                assert!(
                    TokenType::TypeSeparator == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_type();

                if TokenType::Assignment
                    == self.scanner.peek().expect("Next token is None").token_type
                {
                    self.scanner.next();
                    self.process_expression();
                }
            }
            TokenType::KeywordFor => {
                assert!(
                    TokenType::Identifier == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                assert!(
                    TokenType::KeywordIn == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_expression();
                assert!(
                    TokenType::Range == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_expression();
                assert!(
                    TokenType::KeywordDo == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_statement_list();
                assert!(
                    TokenType::KeywordEnd == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                assert!(
                    TokenType::KeywordFor == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
            }
            TokenType::KeywordRead => {
                assert!(
                    TokenType::Identifier == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
            }
            TokenType::KeywordPrint => {
                self.process_expression();
            }
            TokenType::KeywordAssert => {
                assert!(
                    TokenType::LParen == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_expression();
                assert!(
                    TokenType::RParen == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
            }
            TokenType::Identifier => {
                assert!(
                    TokenType::Assignment == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.process_expression();
            }
            _ => assert!(
                false,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            ),
        }
    }

    fn process_end_of_statement(&mut self) {
        assert!(
            TokenType::EndOfStatement == self.scanner.next().unwrap().token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );
    }

    fn process_type(&mut self) {
        let token = self.scanner.next().unwrap();
        assert!(
            TokenType::TypeInt == token.token_type
                || TokenType::TypeBool == token.token_type
                || TokenType::TypeString == token.token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );
    }

    fn process_expression(&mut self) {
        // Starts with unary op
        if TokenType::OperatorNot == self.scanner.peek().expect("Next token is None").token_type {
            self.scanner.next().unwrap();
            self.process_operand();
        } else {
            // Either just a single operand or operand operator operand
            self.process_operand();
            let mut is_operator = true;
            match self.scanner.peek().expect("Next token is None").token_type {
                TokenType::OperatorPlus => {}
                TokenType::OperatorMinus => {}
                TokenType::OperatorMultiply => {}
                TokenType::OperatorDivide => {}
                TokenType::OperatorLessThan => {}
                TokenType::OperatorEqual => {}
                TokenType::OperatorAnd => {}
                _ => is_operator = false, // Not any operator
            }

            if is_operator {
                self.scanner.next();
                self.process_operand();
            }
        }
    }

    fn process_operand(&mut self) {
        let token = self.scanner.next().unwrap();
        if TokenType::LParen == token.token_type {
            self.process_expression();
            assert!(
                TokenType::RParen == self.scanner.next().unwrap().token_type,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            );
        } else {
            assert!(
                TokenType::LiteralInt == token.token_type
                    || TokenType::LiteralString == token.token_type
                    || TokenType::LiteralBool == token.token_type
                    || TokenType::Identifier == token.token_type,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            );
        }
    }
}

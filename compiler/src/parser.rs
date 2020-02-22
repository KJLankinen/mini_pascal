use super::scanner::Scanner;
use super::scanner::TokenData;
use super::scanner::TokenType;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
}

// A recursive descent parser
impl<'a> Parser<'a> {
    pub fn new(source_str: &'a str) -> Self {
        Parser {
            scanner: Scanner::new(source_str),
        }
    }

    pub fn parse(&mut self) {
        self.process_program();
    }

    fn process_program(&mut self) {}

    fn process_end_of_program(&mut self) {
        let token = self.scanner.next().copied().unwrap();
    }

    fn process_statement_list(&mut self) {
        let token = self.scanner.peek().copied().unwrap();

        // Stop if we've reached end of program or the end of 'for' block
        if TokenType::EndOfProgram != token.token_type && TokenType::KeywordEnd != token.token_type
        {
            self.process_statement();
            self.process_statement_list();
        }
    }

    fn process_statement(&mut self) {
        let token = self.scanner.peek().copied().unwrap();
        self.process_statement_prefix();
        self.process_end_of_statement();
    }

    fn process_statement_prefix(&mut self) {
        let token = self.scanner.peek().copied().unwrap();

        match token.token_type {
            TokenType::KeywordVar => {
                // 'var'
                let token = self.scanner.next().copied().unwrap();

                // identifier
                let token = self.scanner.next().copied().unwrap();

                // ':'
                let token = self.scanner.next().copied().unwrap();

                // type
                self.process_type();

                // The statement can be longer
                if TokenType::Assignment
                    == self.scanner.peek().expect("Next token is None").token_type
                {
                    // ':='
                    let token = self.scanner.next().copied().unwrap();

                    // expr
                    self.process_expression();
                }
            }
            TokenType::KeywordFor => {
                // 'for'
                let token = self.scanner.next().copied().unwrap();

                // identifier
                let token = self.scanner.next().copied().unwrap();

                // 'in'
                let token = self.scanner.next().copied().unwrap();

                // expr
                self.process_expression();

                // '..'
                let token = self.scanner.next().copied().unwrap();

                // expr
                self.process_expression();

                // 'do'
                let token = self.scanner.next().copied().unwrap();

                // statement list
                self.process_statement_list();

                // 'end'
                let token = self.scanner.next().copied().unwrap();

                // 'for'
                let token = self.scanner.next().copied().unwrap();
            }
            TokenType::KeywordRead => {
                // 'read'
                let token = self.scanner.next().copied().unwrap();

                // identifier
                let token = self.scanner.next().copied().unwrap();
            }
            TokenType::KeywordPrint => {
                // 'print'
                let token = self.scanner.next().copied().unwrap();

                // expr
                self.process_expression();
            }
            TokenType::KeywordAssert => {
                // 'assert'
                let token = self.scanner.next().copied().unwrap();

                // '('
                let token = self.scanner.next().copied().unwrap();

                // expr
                self.process_expression();

                // ')'
                let token = self.scanner.next().copied().unwrap();
            }
            TokenType::Identifier => {
                // identifier
                let token = self.scanner.next().copied().unwrap();

                // ':='
                let token = self.scanner.next().copied().unwrap();

                // expr
                self.process_expression();
            }
            _ => {}
        }
    }

    fn process_end_of_statement(&mut self) {
        let token = self.scanner.next().copied().unwrap();
    }

    fn process_type(&mut self) {
        let token = self.scanner.next().copied().unwrap();
        let expected = TokenType::TypeInt == token.token_type
            || TokenType::TypeBool == token.token_type
            || TokenType::TypeString == token.token_type;
    }

    fn process_expression(&mut self) {
        // Starts with unary op
        if TokenType::OperatorNot == self.scanner.peek().expect("Next token is None").token_type {
            let token = self.scanner.next().copied().unwrap();
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
                let token = self.scanner.next().copied().unwrap();
                self.process_operand();
            }
        }
    }

    fn process_operand(&mut self) {
        let token = self.scanner.next().copied().unwrap();

        // Operand is '(' <expr> ')'
        if TokenType::LParen == token.token_type {
            self.process_expression();

            let token = self.scanner.next().copied().unwrap();
        } else {
            // Operand is a literal or identifier
        }
    }
}

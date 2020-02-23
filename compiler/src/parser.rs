use super::scanner::Scanner;
use super::scanner::TokenData;
use super::scanner::TokenType;

pub struct Parser<'a> {
    scanner: Scanner<'a>,
}

enum Expression<'a> {
    Expr(&'a str),
    Error,
}

enum Operand<'a> {
    Expr(Expression<'a>),
    Int(&'a str),
    String(&'a str),
    Identifier(&'a str),
    Error,
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

    fn match_token(&mut self, token_type: TokenType) -> &TokenData<'a> {
        let token = self.scanner.next().unwrap();
        if token_type == token.token_type {
            println!("Dandy! {:?}", token);
        } else {
            println!("Unexpected token {:?}", token);
        }

        token
    }

    fn process_program(&mut self) {
        self.process_statement_list();
        self.process_end_of_program();
    }

    fn process_end_of_program(&mut self) {
        if self.scanner.unmatched_multiline_comment_prefixes.is_empty() {
            self.match_token(TokenType::EndOfProgram);
        } else {
            for (line, col) in &self.scanner.unmatched_multiline_comment_prefixes {
                println!("Runaway multi line comment:");
                self.scanner.print_line(*line as usize, *col as usize);
            }
        }
    }

    fn process_statement_list(&mut self) {
        // Stop recursion if the next token is end of program
        // or 'end' keyword that ends the for loop.
        match self.scanner.peek().unwrap().token_type {
            TokenType::EndOfProgram | TokenType::KeywordEnd => {}
            _ => {
                self.process_statement();
                self.process_statement_list();
            }
        }
    }

    fn process_statement(&mut self) {
        self.process_statement_prefix();
        self.match_token(TokenType::EndOfStatement);
    }

    fn process_statement_prefix(&mut self) {
        match self.scanner.peek().unwrap().token_type {
            TokenType::KeywordVar => {
                // Declaration with an optional assignment
                self.match_token(TokenType::KeywordVar);
                self.match_token(TokenType::Identifier);
                self.match_token(TokenType::TypeSeparator);
                self.match_token(TokenType::Type);

                // The statement can be longer
                if TokenType::Assignment == self.scanner.peek().unwrap().token_type {
                    self.match_token(TokenType::Assignment);
                    self.process_expression();
                }
            }
            TokenType::KeywordFor => {
                // For loop
                self.match_token(TokenType::KeywordFor);
                self.match_token(TokenType::Identifier);
                self.match_token(TokenType::KeywordIn);
                self.process_expression();
                self.match_token(TokenType::Range);
                self.process_expression();
                self.match_token(TokenType::KeywordDo);
                self.process_statement_list();
                self.match_token(TokenType::KeywordEnd);
                self.match_token(TokenType::KeywordFor);
            }
            TokenType::KeywordRead => {
                // Read statement
                self.match_token(TokenType::KeywordRead);
                self.match_token(TokenType::Identifier);
            }
            TokenType::KeywordPrint => {
                // Print statement
                self.match_token(TokenType::KeywordPrint);
                self.process_expression();
            }
            TokenType::KeywordAssert => {
                // Assertion statement
                self.match_token(TokenType::KeywordAssert);
                self.match_token(TokenType::LParen);
                self.process_expression();
                self.match_token(TokenType::RParen);
            }
            TokenType::Identifier => {
                // Assignment to a variable
                self.match_token(TokenType::Identifier);
                self.match_token(TokenType::Assignment);
                self.process_expression();
            }
            _ => {
                println!("Invalid start of statement!");
            }
        }
    }

    fn process_expression(&mut self) -> Expression {
        // Starts with unary op
        if TokenType::OperatorNot == self.scanner.peek().unwrap().token_type {
            self.match_token(TokenType::OperatorNot);
            self.process_operand();
        } else {
            // Either a single operand or operand, operator and operand
            self.process_operand();
            match self.scanner.peek().unwrap().token_type {
                token_type @ TokenType::OperatorPlus
                | token_type @ TokenType::OperatorMinus
                | token_type @ TokenType::OperatorMultiply
                | token_type @ TokenType::OperatorDivide
                | token_type @ TokenType::OperatorLessThan
                | token_type @ TokenType::OperatorEqual
                | token_type @ TokenType::OperatorAnd => {
                    self.match_token(token_type);
                    self.process_operand();
                }
                _ => {}
            }
        }

        Expression::Error
    }

    fn process_operand(&mut self) -> Operand {
        let mut operand = Operand::Error;
        match self.scanner.peek().unwrap().token_type {
            token_type @ TokenType::LParen => {
                self.match_token(token_type);
                let expr = self.process_expression();
                operand = Operand::Expr(expr);
                self.match_token(TokenType::RParen);
            }
            token_type @ TokenType::LiteralInt => {
                operand = Operand::Int(self.match_token(token_type).value);
            }
            token_type @ TokenType::LiteralString => {
                operand = Operand::String(self.match_token(token_type).value);
            }
            token_type @ TokenType::Identifier => {
                operand = Operand::Identifier(self.match_token(token_type).value);
            }
            _ => {
                println!("Missing operand!");
            }
        }

        operand
    }
}

use super::scanner::Scanner;
use super::scanner::TokenData;
use super::scanner::TokenType;

#[derive(Debug)]
struct ParseNode<'a> {
    parent: usize,
    left_child: usize,
    right_sibling: usize,
    token: TokenData<'a>,
}

impl<'a> ParseNode<'a> {
    fn new(
        parent: usize,
        left_child: usize,
        right_sibling: usize,
        token: TokenData<'a>,
    ) -> ParseNode<'a> {
        ParseNode {
            parent: parent,
            left_child: left_child,
            right_sibling: right_sibling,
            token: token,
        }
    }
}

impl<'a> Default for ParseNode<'a> {
    fn default() -> ParseNode<'a> {
        ParseNode::new(!0, !0, !0, TokenData::default())
    }
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    parse_tree: Vec<ParseNode<'a>>,
}

// A recursive descent parser
impl<'a> Parser<'a> {
    pub fn new(source_str: &'a str) -> Parser<'a> {
        Parser {
            scanner: Scanner::new(source_str),
            parse_tree: vec![],
        }
    }

    pub fn parse(&mut self) {
        self.process_program();
        println!("{:#?}", self.parse_tree);
    }

    fn add_node(&mut self, parent: usize, previous: usize) -> usize {
        let next_of_prev: &mut usize;
        let id = self.parse_tree.len();
        self.parse_tree.push(ParseNode::default());
        self.parse_tree[id].parent = parent;
        let previous_node = &mut self.parse_tree[previous];

        // This node is the first child of the parent node
        if previous == parent {
            next_of_prev = &mut previous_node.left_child;
        } else {
            next_of_prev = &mut previous_node.right_sibling;
        }

        *next_of_prev = id;
        id
    }

    fn process_program(&mut self) {
        let me = 0;
        let previous = self.add_node(me, me);
        let previous = self.process_statement_list(me, previous);
        self.process_end_of_program(me, previous);
    }

    fn process_end_of_program(&mut self, parent: usize, previous: usize) {
        assert!(
            TokenType::EndOfProgram == self.scanner.next().unwrap().token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );
        self.add_node(parent, previous);
    }

    fn process_statement_list(&mut self, parent: usize, previous: usize) -> usize {
        let me = self.add_node(parent, previous);
        let token = self.scanner.peek().expect("Next token is None.");
        // Stop if we've reached end of program or the end of 'for' block
        if TokenType::EndOfProgram != token.token_type && TokenType::KeywordEnd != token.token_type
        {
            let previous = self.process_statement(me, me);
            self.process_statement_list(me, previous);
        }

        me
    }

    fn process_statement(&mut self, parent: usize, previous: usize) -> usize {
        let me = self.add_node(parent, previous);
        let previous = self.process_statement_prefix(me, me);
        self.process_end_of_statement(me, previous);

        me
    }

    fn process_statement_prefix(&mut self, parent: usize, previous: usize) -> usize {
        let me = self.add_node(parent, previous);
        let previous = self.add_node(me, me);
        let token_type = self.scanner.next().unwrap().token_type;
        match token_type {
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
                let previous = self.add_node(me, previous);
                let previous = self.add_node(me, previous);
                let previous = self.process_type(me, previous);

                if TokenType::Assignment
                    == self.scanner.peek().expect("Next token is None").token_type
                {
                    self.scanner.next();
                    let previous = self.add_node(me, previous);
                    self.process_expression(me, previous);
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

                let previous = self.add_node(me, previous);
                let previous = self.add_node(me, previous);
                let previous = self.process_expression(me, previous);

                assert!(
                    TokenType::Range == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );

                let previous = self.add_node(me, previous);
                let previous = self.process_expression(me, previous);

                assert!(
                    TokenType::KeywordDo == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );

                let previous = self.add_node(me, previous);
                let previous = self.process_statement_list(me, previous);

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

                let previous = self.add_node(me, previous);
                let previous = self.add_node(me, previous);
            }
            TokenType::KeywordRead => {
                assert!(
                    TokenType::Identifier == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.add_node(me, previous);
            }
            TokenType::KeywordPrint => {
                self.process_expression(me, previous);
            }
            TokenType::KeywordAssert => {
                assert!(
                    TokenType::LParen == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                let previous = self.add_node(me, previous);
                let previous = self.process_expression(me, previous);
                assert!(
                    TokenType::RParen == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                self.add_node(me, previous);
            }
            TokenType::Identifier => {
                assert!(
                    TokenType::Assignment == self.scanner.next().unwrap().token_type,
                    "{:?}",
                    self.scanner.current().expect("Current token is None.")
                );
                let previous = self.add_node(me, previous);
                self.process_expression(me, previous);
            }
            _ => assert!(
                false,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            ),
        }

        me
    }

    fn process_end_of_statement(&mut self, parent: usize, previous: usize) -> usize {
        let me = self.add_node(parent, previous);
        assert!(
            TokenType::EndOfStatement == self.scanner.next().unwrap().token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );

        me
    }

    fn process_type(&mut self, parent: usize, previous: usize) -> usize {
        let me = self.add_node(parent, previous);
        let token = self.scanner.next().unwrap();
        assert!(
            TokenType::TypeInt == token.token_type
                || TokenType::TypeBool == token.token_type
                || TokenType::TypeString == token.token_type,
            "{:?}",
            self.scanner.current().expect("Current token is None.")
        );

        me
    }

    fn process_expression(&mut self, parent: usize, previous: usize) -> usize {
        let me = self.add_node(parent, previous);
        // Starts with unary op
        if TokenType::OperatorNot == self.scanner.peek().expect("Next token is None").token_type {
            self.scanner.next().unwrap();
            let previous = self.add_node(me, me);
            self.process_operand(me, previous);
        } else {
            // Either just a single operand or operand operator operand
            let previous = self.process_operand(me, me);
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
                let previous = self.add_node(me, previous);
                self.process_operand(me, previous);
            }
        }

        me
    }

    fn process_operand(&mut self, parent: usize, previous: usize) -> usize {
        let me = self.add_node(parent, previous);
        let token_type = self.scanner.next().unwrap().token_type;
        let previous = self.add_node(me, me);

        if TokenType::LParen == token_type {
            let previous = self.process_expression(me, previous);
            self.add_node(me, previous);
            assert!(
                TokenType::RParen == self.scanner.next().unwrap().token_type,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            );
        } else {
            assert!(
                TokenType::LiteralInt == token_type
                    || TokenType::LiteralString == token_type
                    || TokenType::LiteralBool == token_type
                    || TokenType::Identifier == token_type,
                "{:?}",
                self.scanner.current().expect("Current token is None.")
            );
        }

        me
    }
}

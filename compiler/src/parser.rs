use super::lcrs_tree::{LcRsTree, Update};
use super::scanner::{Scanner, TokenData, TokenType};
use serde::Serialize;
use std::collections::{HashMap, HashSet};

// ---------------------------------------------------------------------
// Type definition for the recursive descent parser
// ---------------------------------------------------------------------
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    tree: LcRsTree<NodeData<'a>>,
    recursion_depth: usize,
    recovery_tokens: HashMap<TokenType, HashSet<usize>>,
    unexpected_tokens: Vec<(TokenData<'a>, Vec<TokenType>)>,
}

// ---------------------------------------------------------------------
// Method implementations for the parser
// ---------------------------------------------------------------------
impl<'a> Parser<'a> {
    fn match_token(&mut self, token_types: &[TokenType]) -> Result<TokenData<'a>, ErrorType> {
        let token_type = self.scanner.peek().token_type;
        for tt in token_types {
            if token_type == *tt {
                return Ok(self.scanner.next());
            }
        }

        self.unexpected_tokens
            .push((*self.scanner.peek(), token_types.to_vec()));
        // Token was not any of the expected tokens, find the point of recovery
        loop {
            let tt = self.scanner.peek().token_type;
            if let Some(set) = self.recovery_tokens.get(&tt) {
                if false == set.is_empty() {
                    let mut max_depth = 0;
                    for depth in set.iter() {
                        if depth > &max_depth {
                            max_depth = *depth;
                        }
                    }
                    return Err(ErrorType {
                        depth: max_depth,
                        token_type: tt,
                    });
                }
            }
            self.scanner.next();

            assert!(
                TokenType::EndOfProgram != tt,
                "EndOfProgram should be a recovery token."
            );
        }
    }

    fn process<T, R>(
        &mut self,
        func: fn(&mut Self, T) -> Result<R, ErrorType>,
        arg1: T,
        recovery_tokens: &[TokenType],
        recovery_index: &mut Option<usize>,
    ) -> Result<Option<R>, ErrorType> {
        // This function handles the errors

        // Add recovery tokens
        self.recursion_depth += 1;
        for tt in recovery_tokens {
            match self.recovery_tokens.get_mut(tt) {
                Some(set) => {
                    set.insert(self.recursion_depth);
                }
                None => {
                    self.recovery_tokens
                        .insert(*tt, [self.recursion_depth].iter().cloned().collect());
                }
            };
        }

        let result = match func(self, arg1) {
            Ok(o) => Ok(Some(o)),
            Err(e) => {
                if e.depth == self.recursion_depth {
                    let mut idx = !0;
                    for i in 0..recovery_tokens.len() {
                        if e.token_type == recovery_tokens[i] {
                            idx = i;
                            break;
                        }
                    }
                    if !0 != idx {
                        *recovery_index = Some(idx);
                        Ok(None)
                    } else {
                        // This should never happen. It's a bug if it does.
                        assert!(false, "Recovery token not found.");
                        Ok(None)
                    }
                } else {
                    // Handle at a higher level, i.e. smaller recursion depth
                    assert!(
                        e.depth < self.recursion_depth,
                        "The depth at which error should be handled can only be smaller."
                    );
                    Err(e)
                }
            }
        };

        // Remove the recovery tokens
        for tt in recovery_tokens {
            match self.recovery_tokens.get_mut(tt) {
                Some(set) => {
                    set.remove(&self.recursion_depth);
                }
                None => {}
            }
        }
        self.recursion_depth -= 1;

        return result;
    }

    // ---------------------------------------------------------------------
    // Recursive processing functions
    // ---------------------------------------------------------------------

    fn program(&mut self, _parent: Option<usize>) -> Result<(), ErrorType> {
        let my_id = self.tree.add_child(None);
        self.tree.update_data(
            my_id,
            NodeData {
                node_type: Some(NodeType::Program),
                token: None,
            },
        );
        let mut recovery_index = None;
        self.process(
            Parser::statement_list,
            my_id,
            &[TokenType::EndOfProgram],
            &mut recovery_index,
        )?;
        self.process(Parser::end_of_program, None, &[], &mut recovery_index)?;

        Ok(())
    }

    fn end_of_program(&mut self, _parent: Option<usize>) -> Result<(), ErrorType> {
        self.match_token(&[TokenType::EndOfProgram])?;
        Ok(())
    }

    fn statement_list(&mut self, parent: Option<usize>) -> Result<(), ErrorType> {
        // Stop recursion if the next token is end of program
        // or 'end' keyword that ends the for loop.
        match self.scanner.peek().token_type {
            TokenType::EndOfProgram | TokenType::KeywordEnd => {}
            _ => {
                let mut recovery_index = None;
                self.process(
                    Parser::statement,
                    parent,
                    &[
                        TokenType::KeywordVar,
                        TokenType::KeywordFor,
                        TokenType::KeywordRead,
                        TokenType::KeywordPrint,
                        TokenType::KeywordAssert,
                        TokenType::Identifier,
                    ],
                    &mut recovery_index,
                )?;
                self.process(Parser::statement_list, parent, &[], &mut recovery_index)?;
            }
        }
        Ok(())
    }

    fn statement(&mut self, parent: Option<usize>) -> Result<(), ErrorType> {
        let mut recovery_index = None;
        self.process(
            Parser::statement_prefix,
            parent,
            &[TokenType::EndOfStatement],
            &mut recovery_index,
        )?;
        // TODO: Change ? to match to find missing end of statements
        self.match_token(&[TokenType::EndOfStatement])?;
        Ok(())
    }

    fn statement_prefix(&mut self, parent: Option<usize>) -> Result<(), ErrorType> {
        match self.scanner.peek().token_type {
            TokenType::KeywordVar => {
                // Declaration with an optional assignment
                let my_id = self.tree.add_child(parent);
                self.match_token(&[TokenType::KeywordVar])?;

                let token = self.match_token(&[TokenType::Identifier])?;
                let id = self.tree.add_child(my_id);
                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );

                self.match_token(&[TokenType::TypeSeparator])?;
                let token = self.match_token(&[
                    TokenType::TypeInt,
                    TokenType::TypeString,
                    TokenType::TypeBool,
                ])?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Declaration),
                        token: Some(token),
                    },
                );

                // The statement can contain an assignment. If it does, use the value the user
                // provided as the right sibling. Otherwise use some default.
                if TokenType::Assignment == self.scanner.peek().token_type {
                    let mut recovery_index = None;
                    self.match_token(&[TokenType::Assignment])?;
                    self.process(Parser::expression, my_id, &[], &mut recovery_index)?;
                } else {
                    let my_id = self.tree.add_child(my_id);
                    self.tree.update_data(
                        my_id,
                        NodeData {
                            node_type: Some(NodeType::Operand),
                            token: None,
                        },
                    );
                }
            }
            TokenType::KeywordFor => {
                // For loop
                let my_id = self.tree.add_child(parent);
                let token = self.match_token(&[TokenType::KeywordFor])?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::For),
                        token: Some(token),
                    },
                );

                let id = self.tree.add_child(my_id);
                let token = self.match_token(&[TokenType::Identifier])?;
                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );

                self.match_token(&[TokenType::KeywordIn])?;

                // The range token '..' is the parent of the two expressions
                let range_id = self.tree.add_child(my_id);
                let mut recovery_index = None;
                self.process(Parser::expression, range_id, &[], &mut recovery_index)?;
                let token = self.match_token(&[TokenType::Range])?;
                self.tree.update_data(
                    range_id,
                    NodeData {
                        node_type: Some(NodeType::Range),
                        token: Some(token),
                    },
                );
                self.process(Parser::expression, range_id, &[], &mut recovery_index)?;

                self.match_token(&[TokenType::KeywordDo])?;
                // Process however many statements there are inside the for block
                self.process(Parser::statement_list, my_id, &[], &mut recovery_index)?;
                self.match_token(&[TokenType::KeywordEnd])?;
                self.match_token(&[TokenType::KeywordFor])?;
            }
            TokenType::KeywordRead => {
                // Read statement
                let my_id = self.tree.add_child(parent);
                let token = self.match_token(&[TokenType::KeywordRead])?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Read),
                        token: Some(token),
                    },
                );

                let token = self.match_token(&[TokenType::Identifier])?;
                let id = self.tree.add_child(my_id);
                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );
            }
            TokenType::KeywordPrint => {
                // Print statement
                let my_id = self.tree.add_child(parent);
                let token = self.match_token(&[TokenType::KeywordPrint])?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Print),
                        token: Some(token),
                    },
                );

                let mut recovery_index = None;
                self.process(Parser::expression, my_id, &[], &mut recovery_index)?;
            }
            TokenType::KeywordAssert => {
                // Assertion statement
                let my_id = self.tree.add_child(parent);
                let token = self.match_token(&[TokenType::KeywordAssert])?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Assert),
                        token: Some(token),
                    },
                );

                let mut recovery_index = None;
                let recovery_tokens = [
                    TokenType::OperatorNot,
                    TokenType::LiteralInt,
                    TokenType::LiteralString,
                    TokenType::Identifier,
                    TokenType::LParen,
                    TokenType::RParen,
                ];
                self.process(
                    Parser::match_token,
                    &[TokenType::LParen],
                    &recovery_tokens,
                    &mut recovery_index,
                )?;

                if let Some(i) = recovery_index {
                    if recovery_tokens.len() > i {
                        match recovery_tokens[i] {
                            TokenType::RParen => (),
                            _ => {
                                self.process(
                                    Parser::expression,
                                    my_id,
                                    &[TokenType::RParen],
                                    &mut recovery_index,
                                )?;
                            }
                        }
                    }
                }
                self.match_token(&[TokenType::RParen])?;
            }
            TokenType::Identifier => {
                // Assignment to a variable
                let my_id = self.tree.add_child(parent);
                let id = self.tree.add_child(my_id);
                let token = self.match_token(&[TokenType::Identifier])?;

                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );

                let mut recovery_index = None;
                let token = self.process(
                    Parser::match_token,
                    &[TokenType::Assignment],
                    &[
                        TokenType::OperatorNot,
                        TokenType::LiteralInt,
                        TokenType::LiteralString,
                        TokenType::Identifier,
                        TokenType::LParen,
                    ],
                    &mut recovery_index,
                )?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Assignment),
                        token: token,
                    },
                );

                self.process(Parser::expression, my_id, &[], &mut recovery_index)?;
            }
            _ => {
                self.match_token(&[])?;
            }
        }
        Ok(())
    }

    fn expression(&mut self, parent: Option<usize>) -> Result<(), ErrorType> {
        let my_id = self.tree.add_child(parent);
        let mut recovery_index = None;

        // First case: Unary operator and operand
        if TokenType::OperatorNot == self.scanner.peek().token_type {
            let token = self.match_token(&[TokenType::OperatorNot])?;
            self.tree.update_data(
                my_id,
                NodeData {
                    node_type: Some(NodeType::Expression),
                    token: Some(token),
                },
            );
            self.process(Parser::operand, my_id, &[], &mut recovery_index)?;

            return Ok(());
        }

        // Second and third case start with an operand
        self.process(
            Parser::operand,
            my_id,
            &[
                TokenType::OperatorPlus,
                TokenType::OperatorMinus,
                TokenType::OperatorMultiply,
                TokenType::OperatorDivide,
                TokenType::OperatorLessThan,
                TokenType::OperatorEqual,
                TokenType::OperatorAnd,
            ],
            &mut recovery_index,
        )?;

        match self.scanner.peek().token_type {
            token_type @ TokenType::OperatorPlus
            | token_type @ TokenType::OperatorMinus
            | token_type @ TokenType::OperatorMultiply
            | token_type @ TokenType::OperatorDivide
            | token_type @ TokenType::OperatorLessThan
            | token_type @ TokenType::OperatorEqual
            | token_type @ TokenType::OperatorAnd => {
                // Second case: operand, binary operator, operand
                let token = self.match_token(&[token_type])?;
                self.process(Parser::operand, my_id, &[], &mut recovery_index)?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Expression),
                        token: Some(token),
                    },
                );

                return Ok(());
            }
            _ => {
                // Third case: a single operand
                // This expression should have only one child so this node can be removed to
                // simplify the AST. Otherwise nested parenthesis will cause a long chain of
                // expr(opnd(expr(opnd))) nodes, which serve no purpose.
                if 1 == self.tree.count_children(my_id) {
                    self.tree.remove_node(my_id);
                }
            }
        }
        Ok(())
    }

    fn operand(&mut self, parent: Option<usize>) -> Result<(), ErrorType> {
        let mut recovery_index = None;
        if TokenType::LParen == self.scanner.peek().token_type {
            self.match_token(&[TokenType::LParen])?;
            self.process(
                Parser::expression,
                parent,
                &[TokenType::RParen],
                &mut recovery_index,
            )?;
            self.match_token(&[TokenType::RParen])?;
        } else {
            let token = self.match_token(&[
                TokenType::LiteralInt,
                TokenType::LiteralString,
                TokenType::Identifier,
            ])?;
            let my_id = self.tree.add_child(parent);
            self.tree.update_data(
                my_id,
                NodeData {
                    node_type: Some(NodeType::Operand),
                    token: Some(token),
                },
            );
        }
        Ok(())
    }

    // ---------------------------------------------------------------------
    // Public utility functions
    // ---------------------------------------------------------------------

    pub fn new(source_str: &'a str) -> Self {
        Parser {
            scanner: Scanner::new(source_str),
            tree: LcRsTree::new(),
            recursion_depth: 0,
            recovery_tokens: HashMap::new(),
            unexpected_tokens: vec![],
        }
    }

    pub fn parse(&mut self) -> bool {
        let mut recovery_index = None;
        match self.process(Parser::program, None, &[], &mut recovery_index) {
            Ok(_) => {}
            Err(e) => assert!(
                false,
                "Unhandled error for {:#?}, recovery tokens: {:#?}",
                e, self.recovery_tokens
            ),
        }

        for (line, col) in &self.scanner.unmatched_multiline_comment_prefixes {
            println!("Runaway multi line comment:");
            self.scanner.print_line(*line as usize, *col as usize);
        }

        if 0 < self.unexpected_tokens.len() {
            println!("Unexpected token(s)!");
        }
        for (token, expected_types) in &self.unexpected_tokens {
            if 1 < expected_types.len() {
                print!("\nExpected one of ");
                for tt in expected_types {
                    print!("\"{}\", ", tt);
                }
                println!("but got \"{}\" instead.", token.token_type);
            } else if 1 == expected_types.len() {
                println!(
                    "\nExpected \"{}\" but got \"{}\" instead.",
                    expected_types[0], token.value
                );
            } else {
                if TokenType::EndOfStatement == token.token_type {
                    println!("\nEmpty statements are not legal, encountered an end of statement.");
                } else {
                    println!("\n\"{}\" is not a legal token.", token.value);
                }
            }
            self.scanner
                .print_line(token.line as usize, token.column as usize);
        }

        return self.scanner.unmatched_multiline_comment_prefixes.is_empty()
            && self.unexpected_tokens.is_empty();
    }

    pub fn serialize(&mut self) -> Option<serde_json::Value> {
        self.tree.serialize()
    }
}

// ---------------------------------------------------------------------
// Type definitions and methods for the nodes of the AST
// ---------------------------------------------------------------------
#[derive(Serialize, Debug, Clone, Copy)]
enum NodeType {
    Program,
    Operand,
    Expression,
    Declaration,
    Assignment,
    For,
    Range,
    Read,
    Print,
    Assert,
}

#[derive(Serialize, Copy, Clone)]
struct NodeData<'a> {
    node_type: Option<NodeType>,
    #[serde(flatten)]
    token: Option<TokenData<'a>>,
}

impl<'a> Default for NodeData<'a> {
    fn default() -> Self {
        NodeData {
            node_type: None,
            token: None,
        }
    }
}

impl<'a> Update for NodeData<'a> {
    fn update(&mut self, data: Self) {
        if data.node_type.is_some() {
            self.node_type = data.node_type;
        }

        if data.token.is_some() {
            self.token = data.token;
        }
    }
}

#[derive(Debug)]
struct ErrorType {
    depth: usize,
    token_type: TokenType,
}

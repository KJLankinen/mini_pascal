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
    // ---------------------------------------------------------------------
    // match_token() and process() are part of the error handling of the parser.
    // Pretty much all the code inside each function is for handling different kind of errors.
    // ---------------------------------------------------------------------
    fn match_token(&mut self, token_types: &[TokenType]) -> Result<TokenData<'a>, ErrorType> {
        // If the next token matches any of the given expected tokens, return it
        let token_type = self.scanner.peek().token_type;
        for tt in token_types {
            if token_type == *tt {
                return Ok(self.scanner.next());
            }
        }

        // Token was not any of the expected tokens. Store the token for later syntax error
        // messages and find the point of recovery.
        self.unexpected_tokens
            .push((*self.scanner.peek(), token_types.to_vec()));
        loop {
            // Match a token to a recovery token at the earliest possible chance. I.e. take the
            // first token that matches any of the recovery tokens on any recursion level.
            // Then, if that token has been set as a recovery token for multiple levels,
            // find the deepest level.
            let tt = self.scanner.peek().token_type;
            if let Some(set) = self.recovery_tokens.get(&tt) {
                if 0 < set.len() {
                    // Find the deepest level of recursion where the given recovery token was
                    // found. I.e. find the maximum value in the set.
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

            // There should always be at least one recovery token, and that is the EndOfProgram.
            // If there is not, a wild bug has appeared.
            assert!(
                TokenType::EndOfProgram != tt,
                "EndOfProgram should be a recovery token. {:#?}",
                self.recovery_tokens
            );
        }
    }

    fn process<T, R>(
        &mut self,
        func: fn(&mut Self, T) -> Result<R, ErrorType>,
        arg1: T,
        recovery_tokens: &[TokenType],
        recovery_token: &mut Option<TokenType>,
    ) -> Result<Option<R>, ErrorType> {
        // Add recovery tokens to the library.
        // Recovery tokens are tokens specified around the recursive functions.
        // If an error in parsing is encountered (due to an unexpected token),
        // a recovery token is searched for. Then the recursive parsing stack is unwound
        // until the specified depth is reached, at which point the function that supplied
        // the hit recovery token takes care of the error and continues parsing from that
        // recovery point.
        self.recursion_depth += 1;
        for tt in recovery_tokens {
            match self.recovery_tokens.get_mut(tt) {
                Some(set) => {
                    // This token has been specified earlier, add a new depth
                    set.insert(self.recursion_depth);
                }
                None => {
                    // This token has not been specified yet, add a new set containing the current depth.
                    self.recovery_tokens
                        .insert(*tt, [self.recursion_depth].iter().cloned().collect());
                }
            };
        }

        // Call the given function pointer. If everything is Ok, everything is Ok.
        // If the function returns an error, we should handle it, if the recovery token that was
        // found corresponds to our depth, i.e. it is one of the tokens that were added to the
        // library above. If the recovery token was set at an earlier point, propagate the error forward.
        let result = match func(self, arg1) {
            Ok(o) => Ok(Some(o)),
            Err(e) => {
                if e.depth == self.recursion_depth {
                    *recovery_token = Some(e.token_type);
                    // Note that we return Ok instead of an error, because the caller of this
                    // function uses the "self.process(...)?" syntax. The '?' token means that
                    // when the function returns, if the returned value was Ok, proceed normally,
                    // otherwise immediately return the Error to our caller. I.e. it automatically
                    // propagates the error upwards if it should be handled above.
                    Ok(None)
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

        // Remove the recovery tokens of this level.
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
        let mut recovery_token = None;
        self.process(
            Parser::statement_list,
            my_id,
            &[TokenType::EndOfProgram],
            &mut recovery_token,
        )?;
        self.process(
            Parser::end_of_program,
            None,
            &[TokenType::EndOfProgram],
            &mut recovery_token,
        )?;

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
                // If the statement encounters an error it can't handle at any earlier level, the
                // parsing can at least continue from the start of the next statement.
                let mut recovery_token = None;
                self.process(
                    Parser::statement,
                    parent,
                    &[
                        TokenType::KeywordVar,
                        TokenType::KeywordFor,
                        TokenType::KeywordRead,
                        TokenType::KeywordPrint,
                        TokenType::KeywordAssert,
                    ],
                    &mut recovery_token,
                )?;
                self.statement_list(parent)?;
            }
        }
        Ok(())
    }

    fn statement(&mut self, parent: Option<usize>) -> Result<(), ErrorType> {
        // Statement errors can be recovered at least at ';' if such is found.
        let mut recovery_token = None;
        match self.process(
            Parser::statement_prefix,
            parent,
            &[TokenType::EndOfStatement],
            &mut recovery_token,
        ) {
            Ok(_) => {
                // Either the statement was parsed successfully or we hit ';' as a recovery point.
                // Regardless, parse ';'.
                self.match_token(&[TokenType::EndOfStatement])?;
            }
            Err(_) => {
                // There was an error during the statement and also ';' is missing. Since the next
                // possible recovery point is at the start of a statement and everything between
                // the error and the next recovery point is ignored, the missing ';' is never
                // recorded by the match_token() function, as it usually is. Thus, record it here.
                self.unexpected_tokens
                    .push((*self.scanner.peek(), vec![TokenType::EndOfStatement]));
            }
        }
        Ok(())
    }

    fn statement_prefix(&mut self, parent: Option<usize>) -> Result<(), ErrorType> {
        // First match at each of the arms is always a certainty due to the peek.
        // All the possible closures inside the statement arms are written in the reverse order.
        // E.g. "do", "expr" ".." "expr" "in" "identifier" "for". This is because the closures can
        // call later closures, and for that to be possible, the later closures have to be defined
        // earlier. In other words, "in" closure can set "do" closure as a possible recovery point,
        // and for that to be possible, "do" closure has to be defined first.
        match self.scanner.peek().token_type {
            TokenType::KeywordVar => {
                // Declaration with an optional assignment
                let my_id = self.tree.add_child(parent);
                self.match_token(&[TokenType::KeywordVar])?;

                // Revovery point for expression
                let expr_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    parser.process(Parser::expression, my_id, &[], &mut recovery_token)?;
                    Ok(())
                };

                // Recovery point for ':=' token
                let assignment_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    // The statement can contain an assignment. If it does, use the value the user
                    // provided as the right sibling. Otherwise use some default.
                    if TokenType::Assignment == parser.scanner.peek().token_type {
                        let mut recovery_token = None;
                        parser.process(
                            Parser::match_token,
                            &[TokenType::Assignment],
                            &[
                                TokenType::OperatorNot,
                                TokenType::LParen,
                                TokenType::LiteralInt,
                                TokenType::LiteralString,
                                TokenType::Identifier,
                            ],
                            &mut recovery_token,
                        )?;
                        expr_closure(parser)?;
                    } else {
                        let my_id = parser.tree.add_child(my_id);
                        parser.tree.update_data(
                            my_id,
                            NodeData {
                                node_type: Some(NodeType::Operand),
                                token: None,
                            },
                        );
                    }
                    Ok(())
                };

                // Recovery point for 'int', 'string' and 'bool' tokens
                let type_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    let token = parser.process(
                        Parser::match_token,
                        &[
                            TokenType::TypeInt,
                            TokenType::TypeString,
                            TokenType::TypeBool,
                        ],
                        &[
                            TokenType::Assignment,
                            TokenType::OperatorNot,
                            TokenType::LParen,
                            TokenType::LiteralInt,
                            TokenType::LiteralString,
                            TokenType::Identifier,
                        ],
                        &mut recovery_token,
                    )?;

                    parser.tree.update_data(
                        my_id,
                        NodeData {
                            node_type: Some(NodeType::Declaration),
                            token: token,
                        },
                    );

                    if token.is_some() {
                        // Proceed normally
                        assignment_closure(parser)?;
                    } else {
                        // Recover
                        if let Some(tt) = recovery_token {
                            match tt {
                                TokenType::Assignment => assignment_closure(parser)?,
                                _ => expr_closure(parser)?,
                            };
                        } else {
                            assert!(false, "Recovery token must be some token.");
                        }
                    }

                    Ok(())
                };

                // Recovery point for ':' token
                let separator_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    let result = parser.process(
                        Parser::match_token,
                        &[TokenType::TypeSeparator],
                        &[
                            TokenType::TypeInt,
                            TokenType::TypeString,
                            TokenType::TypeBool,
                            TokenType::Assignment,
                            TokenType::OperatorNot,
                            TokenType::LParen,
                            TokenType::LiteralInt,
                            TokenType::LiteralString,
                            TokenType::Identifier,
                        ],
                        &mut recovery_token,
                    )?;

                    if result.is_some() {
                        // Proceed normally
                        type_closure(parser)?;
                    } else {
                        // Recover
                        if let Some(tt) = recovery_token {
                            match tt {
                                TokenType::TypeInt
                                | TokenType::TypeString
                                | TokenType::TypeBool => type_closure(parser)?,
                                TokenType::Assignment => assignment_closure(parser)?,
                                _ => expr_closure(parser)?,
                            };
                        } else {
                            assert!(false, "Recovery token must be some token.");
                        }
                    }
                    Ok(())
                };

                // Start by processing the identifier
                let mut recovery_token = None;
                let token = self.process(
                    Parser::match_token,
                    &[TokenType::Identifier],
                    &[
                        TokenType::TypeSeparator,
                        TokenType::TypeInt,
                        TokenType::TypeString,
                        TokenType::TypeBool,
                        TokenType::Assignment,
                        TokenType::OperatorNot,
                        TokenType::LParen,
                        TokenType::LiteralInt,
                        TokenType::LiteralString,
                        TokenType::Identifier,
                    ],
                    &mut recovery_token,
                )?;
                let id = self.tree.add_child(my_id);
                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: token,
                    },
                );

                if token.is_some() {
                    // Proceed normally
                    separator_closure(self)?;
                } else {
                    // Recover
                    if let Some(tt) = recovery_token {
                        match tt {
                            TokenType::TypeSeparator => separator_closure(self)?,
                            TokenType::TypeInt | TokenType::TypeString | TokenType::TypeBool => {
                                type_closure(self)?
                            }
                            TokenType::Assignment => assignment_closure(self)?,
                            _ => expr_closure(self)?,
                        };
                    } else {
                        assert!(false, "Recovery token must be some token.");
                    }
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

                // Recovery point for the last 'for' token
                let for_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    parser.match_token(&[TokenType::KeywordFor])?;
                    Ok(())
                };

                // Recovery point for the 'end' token
                let end_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordEnd],
                        &[TokenType::KeywordFor],
                        &mut recovery_token,
                    )?;

                    for_closure(parser)
                };

                // Recovery point for the 'do' token
                let do_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    parser.match_token(&[TokenType::KeywordDo])?;

                    let mut recovery_token = None;
                    parser.process(
                        Parser::statement_list,
                        my_id,
                        &[TokenType::KeywordEnd],
                        &mut recovery_token,
                    )?;
                    end_closure(parser)
                };

                // Recovery point for the second expression
                let expr_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    parser.process(
                        Parser::expression,
                        my_id,
                        &[TokenType::KeywordDo],
                        &mut recovery_token,
                    )?;
                    do_closure(parser)
                };

                // Recovery point for the '..' token
                let range_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    if parser
                        .process(
                            Parser::match_token,
                            &[TokenType::Range],
                            &[
                                TokenType::KeywordDo,
                                TokenType::OperatorNot,
                                TokenType::LParen,
                                TokenType::LiteralInt,
                                TokenType::LiteralString,
                                TokenType::Identifier,
                            ],
                            &mut recovery_token,
                        )?
                        .is_some()
                    {
                        expr_closure(parser)
                    } else {
                        // Error with parsing '..'
                        if let Some(tt) = recovery_token {
                            match tt {
                                TokenType::KeywordDo => do_closure(parser)?,
                                _ => expr_closure(parser)?,
                            };
                        } else {
                            assert!(false, "Recovery token must be some token.");
                        }
                        Ok(())
                    }
                };

                // Recovery point for the first expression
                let first_expr_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    if parser
                        .process(
                            Parser::expression,
                            my_id,
                            &[
                                TokenType::KeywordDo,
                                TokenType::Range,
                                TokenType::OperatorNot,
                                TokenType::LParen,
                                TokenType::LiteralInt,
                                TokenType::LiteralString,
                                TokenType::Identifier,
                            ],
                            &mut recovery_token,
                        )?
                        .is_some()
                    {
                        range_closure(parser)
                    } else {
                        if let Some(tt) = recovery_token {
                            match tt {
                                TokenType::KeywordDo => do_closure(parser)?,
                                TokenType::Range => range_closure(parser)?,
                                _ => expr_closure(parser)?,
                            };
                        } else {
                            assert!(false, "Recovery token must be some token.");
                        }
                        Ok(())
                    }
                };

                // Recovery point for the 'in' token
                let in_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    if parser
                        .process(
                            Parser::match_token,
                            &[TokenType::KeywordIn],
                            &[TokenType::Range, TokenType::KeywordDo],
                            &mut recovery_token,
                        )?
                        .is_some()
                    {
                        first_expr_closure(parser)
                    } else {
                        if let Some(tt) = recovery_token {
                            match tt {
                                TokenType::Range => range_closure(parser)?,
                                _ => do_closure(parser)?,
                            };
                        } else {
                            assert!(false, "Recovery token must be some token.");
                        }
                        Ok(())
                    }
                };

                // Start by processing the identifier
                let mut recovery_token = None;
                let token = self.process(
                    Parser::match_token,
                    &[TokenType::Identifier],
                    &[TokenType::KeywordIn, TokenType::Range, TokenType::KeywordDo],
                    &mut recovery_token,
                )?;
                let id = self.tree.add_child(my_id);
                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: token,
                    },
                );

                if token.is_some() {
                    // Identifier was successfully matched
                    in_closure(self)?
                } else {
                    // Identifier was not matched, recover at a later point
                    if let Some(tt) = recovery_token {
                        match tt {
                            TokenType::KeywordIn => in_closure(self)?,
                            TokenType::KeywordDo => do_closure(self)?,
                            _ => range_closure(self)?,
                        };
                    } else {
                        assert!(false, "Recovery token must be some token.");
                    }
                }
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

                // No point in trying to recover from
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

                // No point in recovery tokens, since the expression is the last part of this
                // statement.
                self.expression(my_id)?;
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

                // Recovery point for the ')' token
                let paren_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    parser.match_token(&[TokenType::RParen])?;
                    Ok(())
                };

                // Recovery point for the expression
                let expr_closure = |parser: &mut Self| -> Result<(), ErrorType> {
                    let mut recovery_token = None;
                    parser.process(
                        Parser::expression,
                        my_id,
                        &[TokenType::RParen],
                        &mut recovery_token,
                    )?;

                    paren_closure(parser)
                };

                // Start by processing the '(' token. Recover at a start of an expression or at the
                // matching ')' token.
                let mut recovery_token = None;
                let recovery_tokens = [
                    TokenType::OperatorNot,
                    TokenType::LiteralInt,
                    TokenType::LiteralString,
                    TokenType::Identifier,
                    TokenType::LParen,
                    TokenType::RParen,
                ];

                let result = self.process(
                    Parser::match_token,
                    &[TokenType::LParen],
                    &recovery_tokens,
                    &mut recovery_token,
                )?;

                if result.is_some() {
                    // Proceed normally
                    expr_closure(self)?;
                } else {
                    // Recovery
                    if let Some(tt) = recovery_token {
                        match tt {
                            TokenType::RParen => paren_closure(self)?,
                            _ => expr_closure(self)?,
                        };
                    } else {
                        assert!(false, "Recovery token must be some token.");
                    }
                }
            }
            TokenType::Identifier => {
                // Assignment to a variable
                let token = self.match_token(&[TokenType::Identifier])?;
                let my_id = self.tree.add_child(parent);
                let id = self.tree.add_child(my_id);

                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );

                // We can recover at the start of an expression, if ':=' is not found.
                let mut recovery_token = None;
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
                    &mut recovery_token,
                )?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Assignment),
                        token: token,
                    },
                );

                self.expression(my_id)?;
            }
            _ => {
                // Unknown start of statement. Match to nothing, yielding a syntax error. Empty
                // statements and lexical errors lead to here.
                self.match_token(&[])?;
            }
        }
        Ok(())
    }

    fn expression(&mut self, parent: Option<usize>) -> Result<(), ErrorType> {
        let my_id = self.tree.add_child(parent);

        // First case: Unary operator and operand
        if TokenType::OperatorNot == self.scanner.peek().token_type {
            // We just peeked this token, so the match is a certainty.
            let token = self.match_token(&[TokenType::OperatorNot])?;
            self.tree.update_data(
                my_id,
                NodeData {
                    node_type: Some(NodeType::Expression),
                    token: Some(token),
                },
            );
            // There's nothing left of this expression after the operand,
            // so there's no point in having any recovery tokens at this level.
            self.operand(my_id)?;
            return Ok(());
        }

        // Second and third case start with an operand. If we hit an operator, we might be able to
        // recover. It is also possible in some cases, that the recovery is a false positive, i.e.
        // that the expression meant by the user doesn't contain any operators, but a later
        // expression does. But if a recovery token is hit, the given source code contains
        // some syntax errors in any case, so we don't care about the possible false positive here.
        let mut recovery_token = None;
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
            &mut recovery_token,
        )?;

        match self.scanner.peek().token_type {
            token_type @ TokenType::OperatorPlus
            | token_type @ TokenType::OperatorMinus
            | token_type @ TokenType::OperatorMultiply
            | token_type @ TokenType::OperatorDivide
            | token_type @ TokenType::OperatorLessThan
            | token_type @ TokenType::OperatorEqual
            | token_type @ TokenType::OperatorAnd => {
                // Second case: operand, binary operator, operand. Again the match is a certainty,
                // because we just peeked at the next token and got here. There's again no point
                // in adding any recovery tokens for this level, because the operand is the
                // last part of the expression, so there's no possible recovery at this level, if
                // it goes wrong.
                let token = self.match_token(&[token_type])?;
                self.operand(my_id)?;
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
        let mut recovery_token = None;
        if TokenType::LParen == self.scanner.peek().token_type {
            // The match is a certainty due to the peek, and we can recover at the matching ')'.
            self.match_token(&[TokenType::LParen])?;
            self.process(
                Parser::expression,
                parent,
                &[TokenType::RParen],
                &mut recovery_token,
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
        // Start the parsing and print out syntax errors and never ending
        // multiline comments if there were any.
        let mut recovery_token = None;
        match self.process(Parser::program, None, &[], &mut recovery_token) {
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
                } else if TokenType::Undefined == token.token_type {
                    println!("\n\"{}\" is not a legal token.", token.value);
                } else {
                    println!("\nUnexpected token \"{}\".", token.value);
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

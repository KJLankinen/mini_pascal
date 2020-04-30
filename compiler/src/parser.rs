use super::data_types::{
    ErrorType, IdxIdx, IdxIdxOptIdx, NodeType, SymbolType, TokenData, TokenIdx, TokenIdxBool,
    TokenIdxIdx, TokenIdxOptIdx, TokenIdxOptIdxOptIdx, TokenOptIdx, TokenType,
};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use super::scanner::Scanner;
use std::collections::{HashMap, HashSet};
use std::{fs, io::BufWriter};

type ParseResult<T> = Result<T, ParseError>;
#[derive(Debug)]
pub struct ParseError {
    depth: usize,
    token_type: TokenType,
}

// ---------------------------------------------------------------------
// Type definition for the recursive descent parser
// ---------------------------------------------------------------------
pub struct Parser<'a, 'b> {
    scanner: Scanner<'a>,
    tree: &'b mut LcRsTree<NodeType<'a>>,
    recursion_depth: usize,
    recovery_tokens: HashMap<TokenType, HashSet<usize>>,
    logger: &'b mut Logger<'a>,
}

// ---------------------------------------------------------------------
// Method implementations for the parser
// ---------------------------------------------------------------------
impl<'a, 'b> Parser<'a, 'b> {
    // ---------------------------------------------------------------------
    // fn match_token() and fn process() are part of the error handling of the parser.
    // Pretty much all the code inside each function is for handling different kind of errors.
    // Much of the handling code calls these functions with a function pointer.
    // ---------------------------------------------------------------------
    fn match_token(&mut self, token_types: &[TokenType]) -> ParseResult<TokenData<'a>> {
        // If the next token matches any of the given expected tokens, return it
        let token_type = self.scanner.peek().token_type;
        for tt in token_types {
            if token_type == *tt {
                return Ok(self.scanner.next());
            }
        }

        // Token was none of the expected tokens.
        // If the token is a known token, but not undefined, it is a syntax error.
        if TokenType::Undefined != token_type {
            self.logger.add_error(ErrorType::SyntaxError(
                *self.scanner.peek(),
                token_types.to_vec(),
            ));
        }

        loop {
            // Match a token to a recovery token at the earliest possible chance. I.e. take the
            // first token that matches any of the recovery tokens on any recursion level.
            // Then, if that token has been set as a recovery token for multiple levels,
            // find the deepest level.
            if TokenType::Identifier == self.scanner.peek().token_type {
                // If the token that triggered this error is identified as an identifier by the
                // scanner, but it's not expected here, it's probably a typoed keyword. Let's skip
                // it and continue recovery from the next token.
                self.scanner.next();
            }

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

                    return Err(ParseError {
                        depth: max_depth,
                        token_type: tt,
                    });
                }
            } else {
                // The token wasn't in the recovery tokens. Still, we should check if the token is
                // garbage, i.e. check the token for lexical errors.
                if TokenType::Undefined == tt {
                    self.logger
                        .add_error(ErrorType::LexicalError(*self.scanner.peek()));
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
        func: fn(&mut Self, T) -> ParseResult<R>,
        arg1: T,
        recovery_tokens: &[TokenType],
        recovery_token: &mut Option<TokenType>,
    ) -> ParseResult<Option<R>> {
        // Add recovery tokens to the map.
        // Recovery tokens are tokens specified around the recursive functions.
        // If an error in parsing is encountered (due to an unexpected token),
        // a recovery token is searched for. Then the recursive parsing stack is unwound
        // until the specified depth is reached, at which point the function that supplied
        // the recovery token that was hit takes care of the error and continues parsing from that
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
        // map above. If the recovery token was set at an earlier point, propagate the error forward.
        let result = match func(self, arg1) {
            Ok(o) => Ok(Some(o)),
            Err(e) => {
                if e.depth == self.recursion_depth {
                    *recovery_token = Some(e.token_type);
                    Ok(None) // Return Ok, since the error should be handled at caller, not propagated by '?' syntax.
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
    fn program(&mut self) -> ParseResult<()> {
        let my_idx = self.tree.add_child(None);
        let mut node_data = TokenIdxOptIdx {
            token: None,
            idx: !0,
            opt_idx: None,
        };

        // This inner function is called recursively.
        fn parse_program<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut TokenIdxOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;

            // Each arm parses a small fraction of the entire "program" definition and then passes
            // on the next token at which to continue parsing. Then this function is called
            // recursively. This continues until all the parts are parsed. The node of the program
            // is filled as the parsing continues.
            let tt = match tt {
                TokenType::KeywordProgram => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordProgram],
                        &[
                            TokenType::StatementSeparator,
                            TokenType::KeywordProcedure,
                            TokenType::KeywordFunction,
                            TokenType::KeywordBegin,
                            TokenType::EndOfProgram,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| TokenType::Identifier)
                }
                TokenType::Identifier => {
                    let token = parser.process(
                        Parser::match_token,
                        &[TokenType::Identifier],
                        &[
                            TokenType::StatementSeparator,
                            TokenType::KeywordProcedure,
                            TokenType::KeywordFunction,
                            TokenType::KeywordBegin,
                            TokenType::EndOfProgram,
                        ],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| {
                        node_data.token = token;
                        TokenType::StatementSeparator
                    })
                }
                TokenType::StatementSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::StatementSeparator],
                        &[
                            TokenType::KeywordProcedure,
                            TokenType::KeywordFunction,
                            TokenType::KeywordBegin,
                            TokenType::EndOfProgram,
                        ],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::KeywordFunction)
                }
                TokenType::KeywordFunction | TokenType::KeywordProcedure => {
                    node_data.opt_idx = parser
                        .process(
                            Parser::subroutines,
                            my_idx,
                            &[TokenType::KeywordBegin, TokenType::EndOfProgram],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| None);
                    recovery_token.unwrap_or_else(|| TokenType::KeywordBegin)
                }
                TokenType::KeywordBegin => {
                    node_data.idx = parser
                        .process(
                            Parser::block,
                            my_idx,
                            &[TokenType::EndOfProgram],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);

                    TokenType::EndOfProgram
                }
                TokenType::EndOfProgram => {
                    parser.match_token(&[TokenType::EndOfProgram])?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_program(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_program(self, TokenType::KeywordProgram, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::Program(node_data);

        Ok(())
    }

    fn subroutines(&mut self, parent: usize) -> ParseResult<Option<usize>> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut tt = self.scanner.peek().token_type;
        while TokenType::KeywordFunction == tt || TokenType::KeywordProcedure == tt {
            let mut recovery_token = None;
            self.process(
                Parser::function,
                my_idx,
                &[TokenType::KeywordFunction, TokenType::KeywordProcedure],
                &mut recovery_token,
            )?;
            tt = self.scanner.peek().token_type;
        }

        let idx = if self.tree[my_idx].left_child.is_some() {
            self.tree[my_idx].data = NodeType::Subroutines(self.tree[my_idx].left_child.unwrap());
            Some(my_idx)
        } else {
            // No subroutines, remove this node
            self.tree.remove_node(my_idx);
            None
        };

        Ok(idx)
    }

    fn function(&mut self, parent: usize) -> ParseResult<()> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdxOptIdxOptIdx {
            token: None,
            idx: !0,
            opt_idx: None,
            opt_idx2: None,
        };

        // This inner function is called recursively.
        fn parse_function<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut TokenIdxOptIdxOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;

            let tt = match tt {
                // Handle "function" and "procedure" both here.
                TokenType::KeywordFunction => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordProcedure, TokenType::KeywordFunction],
                        &[
                            TokenType::LParen,
                            TokenType::RParen,
                            TokenType::KeywordVar,
                            TokenType::StatementSeparator,
                            TokenType::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| TokenType::Identifier)
                }
                TokenType::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TokenType::Identifier],
                        &[
                            TokenType::LParen,
                            TokenType::RParen,
                            TokenType::KeywordVar,
                            TokenType::StatementSeparator,
                            TokenType::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| TokenType::LParen)
                }
                TokenType::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::LParen],
                        &[
                            TokenType::RParen,
                            TokenType::KeywordVar,
                            TokenType::StatementSeparator,
                            TokenType::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| match parser.scanner.peek().token_type {
                        TokenType::KeywordVar | TokenType::Identifier => TokenType::KeywordVar,
                        _ => TokenType::RParen,
                    })
                }
                TokenType::RParen => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::RParen],
                        &[
                            TokenType::TypeSeparator,
                            TokenType::Type,
                            TokenType::StatementSeparator,
                            TokenType::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| {
                        if TokenType::TypeSeparator == parser.scanner.peek().token_type {
                            TokenType::TypeSeparator
                        } else {
                            TokenType::StatementSeparator
                        }
                    })
                }
                TokenType::KeywordVar => {
                    node_data.opt_idx = parser
                        .process(
                            Parser::parameter_list,
                            my_idx,
                            &[
                                TokenType::RParen,
                                TokenType::StatementSeparator,
                                TokenType::KeywordBegin,
                            ],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| None);
                    recovery_token.unwrap_or_else(|| TokenType::RParen)
                }
                TokenType::TypeSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::TypeSeparator],
                        &[
                            TokenType::Type,
                            TokenType::StatementSeparator,
                            TokenType::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::Type)
                }
                TokenType::Type => {
                    node_data.opt_idx2 = parser.process(
                        Parser::var_type,
                        my_idx,
                        &[TokenType::StatementSeparator, TokenType::KeywordBegin],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::StatementSeparator)
                }
                TokenType::StatementSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::StatementSeparator],
                        &[TokenType::KeywordBegin],
                        &mut recovery_token,
                    )?;
                    TokenType::KeywordBegin
                }
                TokenType::KeywordBegin => {
                    node_data.idx = parser
                        .process(
                            Parser::block,
                            my_idx,
                            &[TokenType::StatementSeparator],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);

                    parser.match_token(&[TokenType::StatementSeparator])?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_function(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_function(self, TokenType::KeywordFunction, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::Function(node_data);

        Ok(())
    }

    fn parameter_list(&mut self, parent: usize) -> ParseResult<Option<usize>> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut tt = self.scanner.peek().token_type;
        while TokenType::KeywordVar == tt || TokenType::Identifier == tt {
            let mut recovery_token = None;
            self.process(
                Parser::parameter,
                my_idx,
                &[
                    TokenType::ListSeparator,
                    TokenType::KeywordVar,
                    TokenType::Identifier,
                ],
                &mut recovery_token,
            )?;

            match self.scanner.peek().token_type {
                TokenType::KeywordVar | TokenType::Identifier => {
                    // Mising list separator
                    self.logger.add_error(ErrorType::SyntaxError(
                        *self.scanner.peek(),
                        vec![TokenType::ListSeparator],
                    ));
                }
                TokenType::ListSeparator => {
                    self.match_token(&[TokenType::ListSeparator])?;
                }
                _ => {}
            }
            tt = self.scanner.peek().token_type;
        }

        let idx = if self.tree[my_idx].left_child.is_some() {
            self.tree[my_idx].data = NodeType::ParamList(self.tree[my_idx].left_child.unwrap());
            Some(my_idx)
        } else {
            // No parameters, remove this node
            self.tree.remove_node(my_idx);
            None
        };

        Ok(idx)
    }

    fn parameter(&mut self, parent: usize) -> ParseResult<()> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdxBool {
            token: None,
            idx: !0,
            b: false,
        };

        fn parse_parameter<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut TokenIdxBool<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::KeywordVar => {
                    if TokenType::Identifier == parser.scanner.peek().token_type {
                        TokenType::Identifier
                    } else {
                        node_data.b = true;
                        parser.process(
                            Parser::match_token,
                            &[TokenType::KeywordVar],
                            &[TokenType::TypeSeparator, TokenType::Type],
                            &mut recovery_token,
                        )?;
                        recovery_token.unwrap_or_else(|| TokenType::Identifier)
                    }
                }
                TokenType::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TokenType::Identifier],
                        &[TokenType::TypeSeparator, TokenType::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::TypeSeparator)
                }
                TokenType::TypeSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::TypeSeparator],
                        &[TokenType::Type],
                        &mut recovery_token,
                    )?;
                    TokenType::Type
                }
                TokenType::Type => {
                    node_data.idx = parser.var_type(my_idx)?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_parameter(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_parameter(self, TokenType::KeywordVar, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::Parameter(node_data);
        Ok(())
    }

    fn var_type(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut symbol_type = SymbolType::Undefined;
        let mut expr_idx = None;

        fn parse_type<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            symbol_type: &mut SymbolType,
            expr_idx: &mut Option<usize>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::KeywordArray => {
                    if TokenType::Type == parser.scanner.peek().token_type {
                        TokenType::Type
                    } else {
                        parser.process(
                            Parser::match_token,
                            &[TokenType::KeywordArray],
                            &[
                                TokenType::LBracket,
                                TokenType::RBracket,
                                TokenType::KeywordOf,
                                TokenType::Type,
                            ],
                            &mut recovery_token,
                        )?;
                        recovery_token.unwrap_or_else(|| TokenType::LBracket)
                    }
                }
                TokenType::LBracket => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::LBracket],
                        &[TokenType::RBracket, TokenType::KeywordOf, TokenType::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                }
                TokenType::LiteralInt => {
                    // LiteralInt might not actually be the next token. We're only using this as a
                    // label for this arm, which we get to from LBracket.
                    *expr_idx = parser.process(
                        Parser::simple_expression,
                        my_idx,
                        &[TokenType::RBracket, TokenType::KeywordOf, TokenType::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::RBracket)
                }
                TokenType::RBracket => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::RBracket],
                        &[TokenType::KeywordOf, TokenType::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::KeywordOf)
                }
                TokenType::KeywordOf => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordOf],
                        &[TokenType::Type],
                        &mut recovery_token,
                    )?;
                    TokenType::Type
                }
                TokenType::Type => {
                    let type_str = parser.match_token(&[TokenType::Type])?.value;

                    *symbol_type = match expr_idx {
                        Some(idx) => match type_str {
                            "Boolean" => SymbolType::ArrayBool(*idx),
                            "integer" => SymbolType::ArrayInt(*idx),
                            "real" => SymbolType::ArrayReal(*idx),
                            "string" => SymbolType::ArrayString(*idx),
                            _ => SymbolType::Undefined,
                        },
                        None => match type_str {
                            "Boolean" => SymbolType::Bool,
                            "integer" => SymbolType::Int,
                            "real" => SymbolType::Real,
                            "string" => SymbolType::String,
                            _ => SymbolType::Undefined,
                        },
                    };
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_type(parser, tt, my_idx, symbol_type, expr_idx)?;
            }
            Ok(())
        }

        parse_type(
            self,
            TokenType::KeywordArray,
            my_idx,
            &mut symbol_type,
            &mut expr_idx,
        )?;
        self.tree[my_idx].data = NodeType::VariableType(symbol_type);
        Ok(my_idx)
    }

    fn block(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut recovery_token = None;
        self.process(
            Parser::match_token,
            &[TokenType::KeywordBegin],
            &[
                TokenType::KeywordVar,
                TokenType::KeywordBegin,
                TokenType::KeywordIf,
                TokenType::KeywordWhile,
                TokenType::KeywordRead,
                TokenType::KeywordWrite,
                TokenType::KeywordReturn,
                TokenType::KeywordAssert,
                TokenType::Identifier,
                TokenType::KeywordEnd,
            ],
            &mut recovery_token,
        )?;

        if recovery_token.is_none() || TokenType::KeywordEnd != recovery_token.unwrap() {
            self.statement_list(my_idx)?;
        }

        self.tree[my_idx].data =
            NodeType::Block(self.tree[my_idx].left_child.unwrap_or_else(|| !0));
        self.match_token(&[TokenType::KeywordEnd])?;

        Ok(my_idx)
    }

    fn statement_list(&mut self, parent: usize) -> ParseResult<()> {
        let mut recovery_token = None;
        self.process(
            Parser::statement,
            parent,
            &[
                TokenType::KeywordVar,
                TokenType::KeywordBegin,
                TokenType::KeywordIf,
                TokenType::KeywordWhile,
                TokenType::KeywordRead,
                TokenType::KeywordWrite,
                TokenType::KeywordReturn,
                TokenType::KeywordAssert,
                TokenType::StatementSeparator,
            ],
            &mut recovery_token,
        )?;

        match self.scanner.peek().token_type {
            TokenType::KeywordVar
            | TokenType::KeywordBegin
            | TokenType::KeywordIf
            | TokenType::KeywordWhile
            | TokenType::KeywordRead
            | TokenType::KeywordWrite
            | TokenType::KeywordReturn
            | TokenType::KeywordAssert
            | TokenType::Identifier => {
                // Mising statement separator
                self.logger.add_error(ErrorType::SyntaxError(
                    *self.scanner.peek(),
                    vec![TokenType::StatementSeparator],
                ));
            }
            TokenType::StatementSeparator => {
                self.match_token(&[TokenType::StatementSeparator])?;
            }
            _ => {}
        }

        if TokenType::KeywordEnd != self.scanner.peek().token_type {
            self.statement_list(parent)
        } else {
            Ok(())
        }
    }

    fn statement(&mut self, parent: usize) -> ParseResult<usize> {
        match self.scanner.peek().token_type {
            TokenType::KeywordVar => self.declaration(parent),
            TokenType::KeywordBegin => self.block(parent),
            TokenType::KeywordIf => self.if_statement(parent),
            TokenType::KeywordWhile => self.while_statement(parent),
            TokenType::KeywordReturn => self.return_statement(parent),
            TokenType::KeywordWrite => self.write_statement(parent),
            TokenType::KeywordRead => self.read_statement(parent),
            TokenType::KeywordAssert => self.assert_statement(parent),
            TokenType::Identifier => {
                if TokenType::LParen == self.scanner.peek_at(1).token_type {
                    self.call(parent)
                } else {
                    self.assignment(parent)
                }
            }
            _ => {
                // Unknown start of statement. Match to nothing, yielding a syntax error. Empty
                // statements and lexical errors lead to here.
                println!("test");
                self.match_token(&[])?;
                Ok(!0)
            }
        }
    }

    fn id_list(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut recovery_token = None;
        self.tree[my_idx].data = NodeType::Identifier(self.process(
            Parser::match_token,
            &[TokenType::Identifier],
            &[TokenType::ListSeparator],
            &mut recovery_token,
        )?);

        let tt = self.scanner.peek().token_type;
        if TokenType::ListSeparator == tt {
            self.match_token(&[TokenType::ListSeparator])?;
        } else if TokenType::Identifier == tt {
            // Mising list separator
            self.logger.add_error(ErrorType::SyntaxError(
                *self.scanner.peek(),
                vec![TokenType::ListSeparator],
            ));
        }

        if TokenType::Identifier == self.scanner.peek().token_type {
            self.id_list(parent)?;
        }

        Ok(my_idx)
    }

    fn argument_list(&mut self, parent: usize) -> ParseResult<usize> {
        let mut recovery_token = None;
        let expr_idx = self
            .process(
                Parser::expression,
                parent,
                &[TokenType::ListSeparator],
                &mut recovery_token,
            )?
            .unwrap_or_else(|| !0);

        match self.scanner.peek().token_type {
            TokenType::ListSeparator => {
                self.match_token(&[TokenType::ListSeparator])?;
            }
            TokenType::OperatorPlus
            | TokenType::OperatorMinus
            | TokenType::OperatorNot
            | TokenType::LParen
            | TokenType::Identifier
            | TokenType::LiteralInt
            | TokenType::LiteralBoolean
            | TokenType::LiteralReal
            | TokenType::LiteralString => {
                // Mising list separator
                self.logger.add_error(ErrorType::SyntaxError(
                    *self.scanner.peek(),
                    vec![TokenType::ListSeparator],
                ));
            }
            _ => {
                return Ok(expr_idx);
            }
        }

        self.argument_list(parent)?;
        Ok(expr_idx)
    }

    fn variable(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenOptIdx {
            token: None,
            opt_idx: None,
        };

        fn parse_variable<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut TokenOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TokenType::Identifier],
                        &[TokenType::LBracket, TokenType::RBracket],
                        &mut recovery_token,
                    )?;
                    if TokenType::LBracket == parser.scanner.peek().token_type {
                        TokenType::LBracket
                    } else {
                        TokenType::Undefined
                    }
                }
                TokenType::LBracket => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::LBracket],
                        &[TokenType::RBracket],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                }
                TokenType::LiteralInt => {
                    // Label for array expression
                    node_data.opt_idx = Some(
                        parser
                            .process(
                                Parser::expression,
                                my_idx,
                                &[TokenType::RBracket],
                                &mut recovery_token,
                            )?
                            .unwrap_or_else(|| !0),
                    );
                    TokenType::RBracket
                }
                TokenType::RBracket => {
                    parser.match_token(&[TokenType::RBracket])?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_variable(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_variable(self, TokenType::Identifier, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::Variable(node_data);
        Ok(my_idx)
    }

    fn variable_list(&mut self, parent: usize) -> ParseResult<usize> {
        let mut recovery_token = None;
        let variable_idx = self
            .process(
                Parser::variable,
                parent,
                &[TokenType::ListSeparator],
                &mut recovery_token,
            )?
            .unwrap_or_else(|| !0);

        match self.scanner.peek().token_type {
            TokenType::ListSeparator => {
                self.match_token(&[TokenType::ListSeparator])?;
            }
            TokenType::Identifier => {
                // Mising list separator
                self.logger.add_error(ErrorType::SyntaxError(
                    *self.scanner.peek(),
                    vec![TokenType::ListSeparator],
                ));
            }
            _ => {
                return Ok(variable_idx);
            }
        }

        self.variable_list(parent)?;
        Ok(variable_idx)
    }

    // ---------------------------------------------------------------------
    // Functions for each of the possible statements.
    // fn statement() leads to one of these
    // ---------------------------------------------------------------------
    fn declaration(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = IdxIdx { idx: !0, idx2: !0 };

        fn parse_declaration<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut IdxIdx,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::KeywordVar => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordVar],
                        &[TokenType::TypeSeparator, TokenType::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::Identifier)
                }
                TokenType::Identifier => {
                    node_data.idx = parser
                        .process(
                            Parser::id_list,
                            my_idx,
                            &[TokenType::TypeSeparator, TokenType::Type],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    recovery_token.unwrap_or_else(|| TokenType::TypeSeparator)
                }
                TokenType::TypeSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::TypeSeparator],
                        &[TokenType::Type],
                        &mut recovery_token,
                    )?;
                    TokenType::Type
                }
                TokenType::Type => {
                    node_data.idx2 = parser.var_type(my_idx)?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_declaration(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_declaration(self, TokenType::KeywordVar, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::Declaration(node_data);

        Ok(my_idx)
    }

    fn if_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = IdxIdxOptIdx {
            idx: !0,
            idx2: !0,
            opt_idx: None,
        };

        fn parse_if<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut IdxIdxOptIdx,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::KeywordIf => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordIf],
                        &[TokenType::KeywordThen, TokenType::KeywordElse],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                }
                TokenType::KeywordThen => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordThen],
                        &[TokenType::KeywordElse],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralString)
                }
                TokenType::KeywordElse => {
                    parser.match_token(&[TokenType::KeywordElse])?;
                    TokenType::LiteralBoolean
                }
                TokenType::LiteralInt => {
                    // Label for expression
                    node_data.idx = parser
                        .process(
                            Parser::expression,
                            my_idx,
                            &[TokenType::KeywordThen, TokenType::KeywordElse],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    recovery_token.unwrap_or_else(|| TokenType::KeywordThen)
                }
                TokenType::LiteralString => {
                    // Label for first statement
                    node_data.idx2 = parser
                        .process(
                            Parser::statement,
                            my_idx,
                            &[TokenType::KeywordElse],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);

                    if TokenType::KeywordElse == parser.scanner.peek().token_type {
                        TokenType::KeywordElse
                    } else {
                        TokenType::Undefined
                    }
                }
                TokenType::LiteralBoolean => {
                    // Label for second statement
                    node_data.opt_idx = Some(parser.statement(my_idx)?);
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_if(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_if(self, TokenType::KeywordIf, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::If(node_data);

        Ok(my_idx)
    }

    fn while_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = IdxIdx { idx: !0, idx2: !0 };

        fn parse_while<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut IdxIdx,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::KeywordWhile => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordWhile],
                        &[TokenType::KeywordDo],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                }
                TokenType::KeywordDo => {
                    parser.match_token(&[TokenType::KeywordDo])?;
                    TokenType::LiteralString
                }
                TokenType::LiteralInt => {
                    // Label for expression
                    node_data.idx = parser
                        .process(
                            Parser::expression,
                            my_idx,
                            &[TokenType::KeywordDo],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TokenType::KeywordDo
                }
                TokenType::LiteralString => {
                    // Label for statement
                    node_data.idx2 = parser.statement(my_idx)?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_while(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_while(self, TokenType::KeywordWhile, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::While(node_data);

        Ok(my_idx)
    }

    fn return_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        self.match_token(&[TokenType::KeywordReturn])?;
        self.tree[my_idx].data = NodeType::Return(self.expression(my_idx)?);
        Ok(my_idx)
    }

    fn write_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut arg = !0;

        fn parse_write<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            arg: &mut usize,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::KeywordWrite => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordWrite],
                        &[TokenType::LParen, TokenType::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LParen)
                }
                TokenType::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::LParen],
                        &[TokenType::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                }
                TokenType::LiteralInt => {
                    // Label for argument list
                    *arg = parser
                        .process(
                            Parser::argument_list,
                            my_idx,
                            &[TokenType::RParen],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TokenType::RParen
                }
                TokenType::RParen => {
                    parser.match_token(&[TokenType::RParen])?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_write(parser, tt, my_idx, arg)?;
            }
            Ok(())
        }

        parse_write(self, TokenType::KeywordWrite, my_idx, &mut arg)?;
        self.tree[my_idx].data = NodeType::Write(arg);

        Ok(my_idx)
    }

    fn read_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut arg = !0;

        fn parse_read<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            arg: &mut usize,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::KeywordRead => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordRead],
                        &[TokenType::LParen, TokenType::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LParen)
                }
                TokenType::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::LParen],
                        &[TokenType::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                }
                TokenType::LiteralInt => {
                    // Label for argument list
                    *arg = parser
                        .process(
                            Parser::variable_list,
                            my_idx,
                            &[TokenType::RParen],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TokenType::RParen
                }
                TokenType::RParen => {
                    parser.match_token(&[TokenType::RParen])?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_read(parser, tt, my_idx, arg)?;
            }
            Ok(())
        }

        parse_read(self, TokenType::KeywordRead, my_idx, &mut arg)?;
        self.tree[my_idx].data = NodeType::Read(arg);

        Ok(my_idx)
    }

    fn assert_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut arg = !0;

        fn parse_assert<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            arg: &mut usize,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::KeywordAssert => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::KeywordAssert],
                        &[TokenType::LParen, TokenType::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LParen)
                }
                TokenType::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::LParen],
                        &[TokenType::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                }
                TokenType::LiteralInt => {
                    // Label for argument list
                    *arg = parser
                        .process(
                            Parser::expression,
                            my_idx,
                            &[TokenType::RParen],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TokenType::RParen
                }
                TokenType::RParen => {
                    parser.match_token(&[TokenType::RParen])?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_assert(parser, tt, my_idx, arg)?;
            }
            Ok(())
        }

        parse_assert(self, TokenType::KeywordAssert, my_idx, &mut arg)?;
        self.tree[my_idx].data = NodeType::Assert(arg);

        Ok(my_idx)
    }

    fn call(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdx {
            token: None,
            idx: !0,
        };

        fn parse_call<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut TokenIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TokenType::Identifier],
                        &[TokenType::LParen, TokenType::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LParen)
                }
                TokenType::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::LParen],
                        &[TokenType::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                }
                TokenType::LiteralInt => {
                    // Label for argument list
                    node_data.idx = parser
                        .process(
                            Parser::argument_list,
                            my_idx,
                            &[TokenType::RParen],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TokenType::RParen
                }
                TokenType::RParen => {
                    parser.match_token(&[TokenType::RParen])?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_call(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_call(self, TokenType::Identifier, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::Call(node_data);

        Ok(my_idx)
    }

    fn assignment(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdxOptIdx {
            token: None,
            idx: !0,
            opt_idx: None,
        };

        fn parse_assignment<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_idx: usize,
            node_data: &mut TokenIdxOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TokenType::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TokenType::Identifier],
                        &[
                            TokenType::LBracket,
                            TokenType::RBracket,
                            TokenType::Assignment,
                        ],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TokenType::LBracket)
                }
                TokenType::LBracket => {
                    if TokenType::LBracket == parser.scanner.peek().token_type {
                        parser.process(
                            Parser::match_token,
                            &[TokenType::LBracket],
                            &[TokenType::RBracket, TokenType::Assignment],
                            &mut recovery_token,
                        )?;
                        recovery_token.unwrap_or_else(|| TokenType::LiteralInt)
                    } else {
                        TokenType::Assignment
                    }
                }
                TokenType::LiteralInt => {
                    // Label for array expression
                    node_data.opt_idx = Some(
                        parser
                            .process(
                                Parser::expression,
                                my_idx,
                                &[TokenType::RBracket, TokenType::Assignment],
                                &mut recovery_token,
                            )?
                            .unwrap_or_else(|| !0),
                    );
                    recovery_token.unwrap_or_else(|| TokenType::RBracket)
                }
                TokenType::RBracket => {
                    parser.process(
                        Parser::match_token,
                        &[TokenType::RBracket],
                        &[TokenType::Assignment],
                        &mut recovery_token,
                    )?;
                    TokenType::Assignment
                }
                TokenType::Assignment => {
                    parser.match_token(&[TokenType::Assignment])?;
                    TokenType::LiteralString
                }
                TokenType::LiteralString => {
                    // Label for expression
                    node_data.idx = parser.expression(my_idx)?;
                    TokenType::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TokenType::Undefined
                }
            };

            if TokenType::Undefined != tt {
                parse_assignment(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_assignment(self, TokenType::Identifier, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NodeType::Assignment(node_data);
        Ok(my_idx)
    }

    // ---------------------------------------------------------------------
    // Functions for expressions
    // ---------------------------------------------------------------------
    fn expression(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut recovery_token = None;
        let first_expr = self
            .process(
                Parser::simple_expression,
                my_idx,
                &[
                    TokenType::OperatorEqual,
                    TokenType::OperatorNotEqual,
                    TokenType::OperatorGreaterEqual,
                    TokenType::OperatorGreater,
                    TokenType::OperatorLessEqual,
                    TokenType::OperatorLess,
                ],
                &mut recovery_token,
            )?
            .unwrap_or_else(|| !0);

        match self.scanner.peek().token_type {
            TokenType::OperatorEqual
            | TokenType::OperatorNotEqual
            | TokenType::OperatorGreaterEqual
            | TokenType::OperatorGreater
            | TokenType::OperatorLessEqual
            | TokenType::OperatorLess => {
                let token = self.match_token(&[
                    TokenType::OperatorEqual,
                    TokenType::OperatorNotEqual,
                    TokenType::OperatorGreaterEqual,
                    TokenType::OperatorGreater,
                    TokenType::OperatorLessEqual,
                    TokenType::OperatorLess,
                ])?;
                let second_expr = self.simple_expression(my_idx)?;
                self.tree[my_idx].data = NodeType::Expression(TokenIdxIdx {
                    token: Some(token),
                    idx: first_expr,
                    idx2: second_expr,
                });
            }
            _ => {
                // Remove this node as there was only one simple expression
                self.tree.remove_node(my_idx);
                return Ok(first_expr);
            }
        }
        Ok(my_idx)
    }

    fn simple_expression(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));

        fn parse_term<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            parent: usize,
            is_head: bool,
        ) -> ParseResult<Option<usize>> {
            let token = match parser.scanner.peek().token_type {
                TokenType::OperatorPlus | TokenType::OperatorMinus => {
                    Some(parser.match_token(&[TokenType::OperatorPlus, TokenType::OperatorMinus])?)
                }
                TokenType::OperatorOr => {
                    if is_head {
                        None
                    } else {
                        Some(parser.match_token(&[TokenType::OperatorOr])?)
                    }
                }
                _ => None,
            };

            if is_head || token.is_some() {
                let my_idx = parser.tree.add_child(Some(parent));
                let mut node_data = TokenIdx {
                    token: token,
                    idx: !0,
                };

                let mut recovery_token = None;
                node_data.idx = parser
                    .process(
                        Parser::term,
                        my_idx,
                        &[
                            TokenType::OperatorPlus,
                            TokenType::OperatorMinus,
                            TokenType::OperatorOr,
                        ],
                        &mut recovery_token,
                    )?
                    .unwrap_or_else(|| !0);

                if 1 == parser.tree.count_children(my_idx) && token.is_none() {
                    parser.tree.remove_node(my_idx);
                } else {
                    parser.tree[my_idx].data = NodeType::Term(node_data);
                }
                parse_term(parser, parent, false)?;

                Ok(Some(my_idx))
            } else {
                Ok(None)
            }
        }

        let idx = parse_term(self, my_idx, true)?.unwrap_or_else(|| !0);
        if 1 == self.tree.count_children(my_idx) {
            self.tree.remove_node(my_idx);
        } else {
            self.tree[my_idx].data = NodeType::SimpleExpression(idx);
        }
        Ok(my_idx)
    }

    fn term(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdx {
            token: None,
            idx: !0,
        };

        let operators = [
            TokenType::OperatorMultiply,
            TokenType::OperatorDivide,
            TokenType::OperatorModulo,
            TokenType::OperatorAnd,
        ];
        let mut recovery_token = None;
        node_data.idx = self
            .process(Parser::factor, my_idx, &operators, &mut recovery_token)?
            .unwrap_or_else(|| !0);

        fn parse_factor<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            parent: usize,
            operators: &[TokenType],
        ) -> ParseResult<()> {
            // If the next token is one of the given operators, parse factor
            if let Some(tt) = operators
                .iter()
                .find(|&&op| op == parser.scanner.peek().token_type)
            {
                let my_idx = parser.tree.add_child(Some(parent));
                let mut node_data = TokenIdx {
                    token: Some(parser.match_token(&[*tt])?),
                    idx: !0,
                };

                let mut recovery_token = None;
                node_data.idx = parser
                    .process(Parser::factor, my_idx, operators, &mut recovery_token)?
                    .unwrap_or_else(|| !0);

                parser.tree[my_idx].data = NodeType::Factor(node_data);
                parse_factor(parser, parent, operators)?;
            }
            Ok(())
        }

        if 1 == self.tree.count_children(my_idx) {
            self.tree.remove_node(my_idx);
        } else {
            self.tree[my_idx].data = NodeType::Factor(node_data);
        }
        parse_factor(self, parent, &operators)?;

        Ok(my_idx)
    }

    fn factor(&mut self, parent: usize) -> ParseResult<usize> {
        let parent = self.tree.add_child(Some(parent));
        let child_idx = match self.scanner.peek().token_type {
            TokenType::Identifier => {
                if TokenType::LParen == self.scanner.peek_at(1).token_type {
                    Some(self.call(parent)?)
                } else {
                    Some(self.variable(parent)?)
                }
            }
            tt @ TokenType::LiteralBoolean
            | tt @ TokenType::LiteralInt
            | tt @ TokenType::LiteralReal
            | tt @ TokenType::LiteralString => {
                let my_idx = self.tree.add_child(Some(parent));
                self.tree[my_idx].data = NodeType::Literal(Some(self.match_token(&[tt])?));
                Some(my_idx)
            }
            TokenType::LParen => {
                let mut recovery_token = None;
                self.match_token(&[TokenType::LParen])?;
                let my_idx = self
                    .process(
                        Parser::expression,
                        parent,
                        &[TokenType::RParen],
                        &mut recovery_token,
                    )?
                    .unwrap_or_else(|| !0);
                self.match_token(&[TokenType::RParen])?;
                Some(my_idx)
            }
            TokenType::OperatorNot => {
                let my_idx = self.tree.add_child(Some(parent));
                self.tree[my_idx].data = NodeType::Not(TokenIdx {
                    token: Some(self.match_token(&[TokenType::OperatorNot])?),
                    idx: self.factor(my_idx)?,
                });
                Some(my_idx)
            }
            _ => None,
        };

        if TokenType::OperatorSize == self.scanner.peek().token_type {
            let my_idx = parent;
            self.tree[my_idx].data = NodeType::ArraySize(TokenIdx {
                token: Some(self.match_token(&[TokenType::OperatorSize])?),
                idx: child_idx.unwrap_or_else(|| !0),
            });
            Ok(my_idx)
        } else {
            self.tree.remove_node(parent);
            Ok(child_idx.unwrap_or_else(|| !0))
        }
    }

    // ---------------------------------------------------------------------
    // Public utility functions
    // ---------------------------------------------------------------------
    pub fn new(
        source_str: &'a str,
        logger: &'b mut Logger<'a>,
        tree: &'b mut LcRsTree<NodeType<'a>>,
    ) -> Self {
        Parser {
            scanner: Scanner::new(source_str),
            tree: tree,
            recursion_depth: 0,
            recovery_tokens: HashMap::new(),
            logger: logger,
        }
    }

    pub fn parse(&mut self, out_file: Option<&'a str>) {
        // Parse the program
        match self.program() {
            Ok(_) => (),
            Err(_) => {
                assert!(false, "Unhandled error at parse.");
                ()
            }
        }

        // If there were any unmatched multiline comment starting tokens, add those as errors.
        for (line, col) in &self.scanner.unmatched_multiline_comment_prefixes {
            self.logger
                .add_error(ErrorType::UnmatchedComment(*line, *col));
        }

        // Serialize the AST to a json if so specified.
        if let Some(filename) = out_file {
            if let Some(json) = self.tree.serialize() {
                let file = fs::File::create(&filename).expect(
                    format!("Could not create a new file with the name {}", &filename).as_str(),
                );

                println!("Writing the AST to the file \"{}\"", &filename);

                let mut writer = BufWriter::new(&file);
                match serde_json::to_writer_pretty(&mut writer, &json) {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("Error writing to file: {}", err);
                    }
                }
            }
        }
    }

    //fn template(&mut self, parent: usize) -> ParseResult<()> {
    //    let my_idx = self.tree.add_child(Some(parent));
    //    let mut node_data = SomethingOrOther {
    //        token: None,
    //        idx: !0,
    //        opt_idx: None,
    //    };

    //    fn parse_<'a, 'b>(
    //        parser: &mut Parser<'a, 'b>,
    //        tt: TokenType,
    //        my_idx: usize,
    //        node_data: &mut SomethingOrOther,
    //    ) -> ParseResult<()> {
    //        let mut recovery_token = None;
    //        let tt = match tt {
    //            TokenType::KeywordNOTAKEYWORD => TokenType::Undefined,
    //            _ => {
    //                assert!(false, "Token {} doesn't belong here.", tt);
    //                TokenType::Undefined
    //            }
    //        };

    //        if TokenType::Undefined != tt {
    //            parse_(parser, tt, my_idx, node_data)?;
    //        }
    //        Ok(())
    //    }

    //    parse_(self, TokenType::KeywordASDASD, my_idx, &mut node_data)?;
    //    self.tree[my_idx].data = NodeType::ASDASD(node_data);

    //    Ok(my_idx)
    //}
}

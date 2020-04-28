use super::data_types::{
    ErrorType, NodeType, SymbolType, TokenData, TokenIdxBool, TokenIdxIdx, TokenIdxOptIdx,
    TokenIdxOptIdxOptIdx, TokenType,
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
    const DUMMY_TOKEN: TokenData<'a> = TokenData {
        column: !0,
        line: !0,
        token_type: TokenType::Undefined,
        value: "DummyDummy",
    };

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
                    let token = parser.process(
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
        let mut recovery_token = None;
        let tt = self.scanner.peek().token_type;
        while TokenType::KeywordFunction == tt || TokenType::KeywordProcedure == tt {
            recovery_token = None;
            self.process(
                Parser::function,
                my_idx,
                &[TokenType::KeywordFunction, TokenType::KeywordProcedure],
                &mut recovery_token,
            )?;
            let tt = self.scanner.peek().token_type;
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
        // TODO: Aren't these identifiers supposed to be saved somewhere?
        let my_idx = self.tree.add_child(Some(parent));
        let tt = self.scanner.peek().token_type;
        while TokenType::KeywordVar == tt || TokenType::Identifier == tt {
            let recovery_token = None;
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
            let tt = self.scanner.peek().token_type;
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

        parse_parameter(self, TokenType::KeywordVar, my_idx, &mut node_data);
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
                        Parser::simple_expr,
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
                TokenType::Identifier,
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
            TokenType::KeywordEnd => return Ok(()),
            _ => {}
        }

        self.statement_list(parent)
    }

    fn statement(&mut self, parent: usize) -> ParseResult<()> {
        match self.scanner.peek().token_type {
            TokenType::KeywordVar => self.declaration(parent)?,
            TokenType::KeywordBegin => self.block(parent)?,
            TokenType::KeywordIf => self.if_statement(parent)?,
            TokenType::KeywordWhile => self.while_statement(parent)?,
            TokenType::KeywordReturn => self.return_statement(parent)?,
            TokenType::KeywordWrite => self.write_statement(parent)?,
            TokenType::KeywordRead => self.read_statement(parent)?,
            TokenType::KeywordAssert => self.assert_statement(parent)?,
            TokenType::Identifier => self.id_statement(parent)?,
            _ => {
                // Unknown start of statement. Match to nothing, yielding a syntax error. Empty
                // statements and lexical errors lead to here.
                self.match_token(&[])?;
            }
        }

        Ok(())
    }

    // ---------------------------------------------------------------------
    // Functions for each of the possible statements.
    // fn statement() leads to one of these
    // ---------------------------------------------------------------------
    fn declaration(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn if_statement(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn while_statement(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn return_statement(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn write_statement(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn read_statement(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn assert_statement(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn id_statement(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn call(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn assignment(&mut self, parent: usize) -> ParseResult<()> {
        Ok(())
    }

    // ---------------------------------------------------------------------
    // Functions for expressions
    // ---------------------------------------------------------------------
    fn expr(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut recovery_token = None;
        let first_expr = self
            .process(
                Parser::simple_expr,
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
                let second_expr = self.simple_expr(my_idx)?;
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

    fn simple_expr(&mut self, parent: usize) -> ParseResult<usize> {
        Ok(!0)
    }

    fn term(&mut self, _parent: usize) -> ParseResult<usize> {
        Ok(!0)
    }

    fn factor(&mut self, _parent: usize) -> ParseResult<usize> {
        Ok(!0)
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
}

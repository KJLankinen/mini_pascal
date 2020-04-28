use super::data_types::{
    ErrorType, NodeType, SymbolType, TokenData, TokenIdx, TokenIdxOptIdx, TokenIdxOptIdxOptSymbol,
    TokenSymbol, TokenType,
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
        let my_id = self.tree.add_child(None);
        let mut node_data = TokenIdxOptIdx {
            token: None,
            idx: !0,
            opt_idx: None,
        };

        fn parse_program<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TokenType,
            my_id: usize,
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
                    if token.is_some() {
                        TokenType::Identifier
                    } else {
                        recovery_token.expect("Recovery token must be some token.")
                    }
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
                    if token.is_some() {
                        node_data.token = token;
                        TokenType::StatementSeparator
                    } else {
                        recovery_token.expect("Recovery token must be some token.")
                    }
                }
                TokenType::StatementSeparator => {
                    if parser
                        .process(
                            Parser::match_token,
                            &[TokenType::StatementSeparator],
                            &[
                                TokenType::KeywordProcedure,
                                TokenType::KeywordFunction,
                                TokenType::KeywordBegin,
                                TokenType::EndOfProgram,
                            ],
                            &mut recovery_token,
                        )?
                        .is_some()
                    {
                        TokenType::KeywordProcedure
                    } else {
                        recovery_token.expect("Recovery token must be some token.")
                    }
                }
                TokenType::KeywordFunction | TokenType::KeywordProcedure => {
                    // Subroutines are voluntary, so check if there are some
                    match parser.scanner.peek().token_type {
                        TokenType::KeywordFunction | TokenType::KeywordProcedure => {
                            let sub_id = parser.tree.add_child(Some(my_id));
                            // TODO: add the data to subtree's own node
                            node_data.opt_idx = Some(sub_id);
                            parser.process(
                                Parser::subroutines,
                                sub_id,
                                &[TokenType::KeywordBegin, TokenType::EndOfProgram],
                                &mut recovery_token,
                            )?;

                            recovery_token.unwrap_or_else(|| TokenType::KeywordBegin)
                        }
                        _ => TokenType::KeywordBegin,
                    }
                }
                TokenType::KeywordBegin => {
                    let block_id = parser.process(
                        Parser::block,
                        my_id,
                        &[TokenType::EndOfProgram],
                        &mut recovery_token,
                    )?;

                    node_data.idx = block_id.unwrap_or_else(|| !0);
                    TokenType::EndOfProgram
                }
                TokenType::EndOfProgram => {
                    parser.match_token(&[TokenType::EndOfProgram])?;
                    TokenType::Undefined
                }
                _ => TokenType::Undefined,
            };

            if TokenType::Undefined != tt {
                parse_program(parser, tt, my_id, node_data)?;
            }
            Ok(())
        }

        parse_program(self, TokenType::KeywordProgram, my_id, &mut node_data)?;
        self.tree[my_id].data = NodeType::Program(node_data);

        Ok(())
    }

    fn subroutines(&mut self, parent: usize) -> ParseResult<()> {
        match self.scanner.peek().token_type {
            TokenType::KeywordFunction | TokenType::KeywordProcedure => {
                let mut recovery_token = None;
                self.process(
                    Parser::function,
                    parent,
                    &[TokenType::KeywordFunction, TokenType::KeywordProcedure],
                    &mut recovery_token,
                )?;
                self.subroutines(parent)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn function(&mut self, _parent: usize) -> ParseResult<()> {
        Ok(())
    }

    /*fn function(&mut self, parent: usize) -> ParseResult<()> {
        let my_id = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdxOptIdxOptSymbol {
            token: None,
            idx: !0,
            opt_idx: None,
            opt_symbol: None,
        };

        let has_returntype = TokenType::KeywordFunction == self.scanner.peek().token_type;

        let mut block_id = !0;
        let mut block_closure = |parser: &mut Self| -> ParseResult<()> {
            let mut recovery_token = None;
            parser.process(
                Parser::block,
                my_id,
                &[TokenType::StatementSeparator],
                &mut recovery_token,
            )?;

            if let Some(lc) = parser.tree[my_id].left_child {
                if let Some(rs) = parser.tree[lc].right_sibling {
                    // Function has parameters, block is the right sibling
                    block_id = rs;
                } else {
                    // Function doesn't have parameters (or something is wrong),
                    // block is the left child
                    block_id = lc;
                }
            }

            parser.match_token(&[TokenType::StatementSeparator])?;
            Ok(())
        };

        let mut sep_closure = |parser: &mut Self| -> ParseResult<()> {
            let mut recovery_token = None;
            parser.process(
                Parser::match_token,
                &[TokenType::StatementSeparator],
                &[TokenType::KeywordBegin],
                &mut recovery_token,
            )?;

            block_closure(parser)
        };

        let mut symbol_type = None;
        let mut type_closure = |parser: &mut Self| -> ParseResult<()> {
            // process ')' first, then possibly type, if function or nothing if procedure
            let mut recovery_token = None;
            let token = parser.process(
                Parser::match_token,
                &[TokenType::RParen],
                &[TokenType::StatementSeparator, TokenType::KeywordBegin],
                &mut recovery_token,
            )?;

            if token.is_some() {
                // Function, not procedure
                if has_returntype {
                    recovery_token = None;
                    let token = parser.process(
                        Parser::match_token,
                        &[TokenType::TypeSeparator],
                        &[
                            TokenType::Type,
                            TokenType::StatementSeparator,
                            TokenType::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    if token.is_some() || TokenType::Type == token.unwrap().token_type {
                        recovery_token = None;
                        symbol_type = parser.process(
                            Parser::var_type,
                            my_id,
                            &[TokenType::StatementSeparator, TokenType::KeywordBegin],
                            &mut recovery_token,
                        )?;

                        if symbol_type.is_some()
                            || TokenType::StatementSeparator == recovery_token.unwrap()
                        {
                            sep_closure(parser)?;
                        } else {
                            block_closure(parser)?;
                        }
                    } else {
                        match recovery_token.expect("Recovery token must be some token.") {
                            TokenType::StatementSeparator => sep_closure(parser)?,
                            _ => block_closure(parser)?,
                        };
                    }
                }
                sep_closure(parser)?;
            } else {
                match recovery_token.expect("Recovery token must be some token.") {
                    TokenType::StatementSeparator => sep_closure(parser)?,
                    _ => block_closure(parser)?,
                };
            }
            Ok(())
        };

        let mut param_id = None;
        let mut param_closure = |parser: &mut Self| -> ParseResult<()> {
            // process '(' first, then parameter list
            let mut recovery_token = None;
            let token = parser.process(
                Parser::match_token,
                &[TokenType::LParen],
                &[
                    TokenType::RParen,
                    TokenType::StatementSeparator,
                    TokenType::KeywordBegin,
                ],
                &mut recovery_token,
            )?;

            if token.is_some() {
                match parser.scanner.peek().token_type {
                    TokenType::KeywordVar | TokenType::Identifier => {
                        let param_id = parser.tree.add_child(Some(my_id));
                        recovery_token = None;
                        parser.process(
                            Parser::parameter_list,
                            param_id,
                            &[
                                TokenType::RParen,
                                TokenType::StatementSeparator,
                                TokenType::KeywordBegin,
                            ],
                            &mut recovery_token,
                        )?;

                        let fp = parser.tree[param_id].left_child.unwrap_or_else(|| !0);
                        parser.tree[param_id].data = NodeType::Parameters {
                            first_parameter: fp,
                        };

                        if let Some(tt) = recovery_token {
                            match tt {
                                TokenType::RParen => type_closure(parser)?,
                                TokenType::StatementSeparator => sep_closure(parser)?,
                                _ => block_closure(parser)?,
                            }
                        } else {
                            type_closure(parser)?;
                        }
                    }
                    _ => type_closure(parser)?,
                }
            } else {
                match recovery_token.expect("Recovery token must be some token.") {
                    TokenType::RParen => type_closure(parser)?,
                    TokenType::StatementSeparator => sep_closure(parser)?,
                    _ => block_closure(parser)?,
                };
            }
            Ok(())
        };

        let mut id_token = None;
        let mut id_closure = |parser: &mut Self| -> ParseResult<()> {
            let mut recovery_token = None;
            id_token = parser.process(
                Parser::match_token,
                &[TokenType::Identifier],
                &[
                    TokenType::LParen,
                    TokenType::RParen,
                    TokenType::StatementSeparator,
                    TokenType::KeywordBegin,
                ],
                &mut recovery_token,
            )?;

            if id_token.is_some() {
                param_closure(parser)?;
            } else {
                match recovery_token.expect("Recovery token must be some token.") {
                    TokenType::LParen => param_closure(parser)?,
                    TokenType::RParen => type_closure(parser)?,
                    TokenType::StatementSeparator => sep_closure(parser)?,
                    _ => block_closure(parser)?,
                };
            }

            Ok(())
        };

        let mut recovery_token = None;
        let token = self.process(
            Parser::match_token,
            &[TokenType::KeywordProcedure, TokenType::KeywordFunction],
            &[
                TokenType::LParen,
                TokenType::RParen,
                TokenType::StatementSeparator,
                TokenType::KeywordBegin,
            ],
            &mut recovery_token,
        )?;

        if token.is_some() {
            id_closure(self)?;
        } else {
            match recovery_token.expect("Recovery token must be some token.") {
                TokenType::LParen => param_closure(self)?,
                TokenType::RParen => type_closure(self)?,
                TokenType::StatementSeparator => sep_closure(self)?,
                _ => block_closure(self)?,
            };
        }

        self.tree[my_id].data = NodeType::Function(node_data);

        Ok(())
    }*/

    fn parameter_list(&mut self, _parent: usize) -> ParseResult<()> {
        Ok(())
    }

    fn block(&mut self, parent: usize) -> ParseResult<usize> {
        let my_id = self.tree.add_child(Some(parent));
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
            self.statement_list(my_id)?;
        }

        self.tree[my_id].data = NodeType::Block(self.tree[my_id].left_child.unwrap_or_else(|| !0));
        self.match_token(&[TokenType::KeywordEnd])?;

        Ok(my_id)
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

    fn statement(&mut self, _parent: usize) -> ParseResult<()> {
        match self.scanner.peek().token_type {
            //TokenType::KeywordVar => self.declaration(parent)?,
            //TokenType::KeywordRead => self.read_statement(parent)?,
            //TokenType::KeywordAssert => self.assert_statement(parent)?,
            //TokenType::Identifier => self.assignment(parent)?,
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
    // fn statement_prefix() leads to one of these
    // ---------------------------------------------------------------------

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

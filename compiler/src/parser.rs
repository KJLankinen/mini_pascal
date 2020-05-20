use super::data_types::NodeType as NT;
use super::data_types::SymbolType as ST;
use super::data_types::TokenType as TT;
use super::data_types::{
    ErrorType, IdxIdx, TokenData, TokenIdx, TokenIdxBool, TokenIdxIdx, TokenIdxIdxOptIdx,
    TokenIdxOptIdx, TokenIdxOptIdxOptIdx, TokenOptIdx, TokenSymbolIdxIdx, TokenSymbolIdxOptIdx,
    TokenSymbolType, VariableData,
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
    token_type: TT,
}

// ---------------------------------------------------------------------
// Type definition for the recursive descent parser
// ---------------------------------------------------------------------
pub struct Parser<'a, 'b> {
    scanner: Scanner<'a>,
    tree: &'b mut LcRsTree<NT<'a>>,
    recursion_depth: usize,
    recovery_tokens: HashMap<TT, HashSet<usize>>,
    logger: &'b mut Logger<'a>,
}

// ---------------------------------------------------------------------
// Method implementations for the parser
// ---------------------------------------------------------------------
impl<'a, 'b> Parser<'a, 'b> {
    const EXPRESSION_FIRST: &'static [TT] = &[
        TT::OperatorPlus,
        TT::OperatorMinus,
        TT::OperatorNot,
        TT::LParen,
        TT::Identifier,
        TT::LiteralInt,
        TT::LiteralBool,
        TT::LiteralReal,
        TT::LiteralString,
    ];
    // ---------------------------------------------------------------------
    // fn match_token() and fn process() are part of the error handling of the parser.
    // Pretty much all the code inside each function is for handling different kind of errors.
    // Much of the handling code calls these functions with a function pointer.
    // ---------------------------------------------------------------------
    fn match_token(&mut self, token_types: &[TT]) -> ParseResult<TokenData<'a>> {
        // If the next token matches any of the given expected tokens, return it
        let token_type = self.scanner.peek().token_type;
        for tt in token_types {
            if token_type == *tt {
                return Ok(self.scanner.next());
            }
        }

        // Token was none of the expected tokens.
        // If the token is a known token, but not undefined, it is a syntax error.
        if TT::Undefined != token_type {
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
            if TT::Identifier == self.scanner.peek().token_type {
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
                if TT::Undefined == tt {
                    self.logger
                        .add_error(ErrorType::LexicalError(*self.scanner.peek()));
                }
            }
            self.scanner.next();

            // There should always be at least one recovery token, and that is the EOF.
            // If there is not, a wild bug has appeared.
            assert!(
                TT::EOF != tt,
                "EOF should be a recovery token. {:#?}",
                self.recovery_tokens
            );
        }
    }

    fn process<T, R>(
        &mut self,
        func: fn(&mut Self, T) -> ParseResult<R>,
        arg1: T,
        recovery_tokens: &[TT],
        recovery_token: &mut Option<TT>,
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
    fn program(&mut self, _: usize) -> ParseResult<()> {
        let my_idx = self.tree.add_child(None);
        let mut node_data = TokenIdxOptIdx {
            token: None,
            idx: !0,
            opt_idx: None,
        };

        fn parse_program<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenIdxOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordProgram => {
                    parser.process(
                        Parser::match_token,
                        &[TT::KeywordProgram],
                        &[
                            TT::StatementSeparator,
                            TT::KeywordProcedure,
                            TT::KeywordFunction,
                            TT::KeywordBegin,
                            TT::EndOfProgram,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| TT::Identifier)
                }
                TT::Identifier => {
                    let token = parser.process(
                        Parser::match_token,
                        &[TT::Identifier],
                        &[
                            TT::StatementSeparator,
                            TT::KeywordProcedure,
                            TT::KeywordFunction,
                            TT::KeywordBegin,
                            TT::EndOfProgram,
                        ],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| {
                        node_data.token = token;
                        TT::StatementSeparator
                    })
                }
                TT::StatementSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TT::StatementSeparator],
                        &[
                            TT::KeywordProcedure,
                            TT::KeywordFunction,
                            TT::KeywordBegin,
                            TT::EndOfProgram,
                        ],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::KeywordFunction)
                }
                TT::KeywordFunction | TT::KeywordProcedure => {
                    node_data.opt_idx = parser
                        .process(
                            Parser::subroutines,
                            my_idx,
                            &[TT::KeywordBegin, TT::EndOfProgram],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| None);
                    recovery_token.unwrap_or_else(|| TT::KeywordBegin)
                }
                TT::KeywordBegin => {
                    node_data.idx = parser
                        .process(
                            Parser::block,
                            my_idx,
                            &[TT::EndOfProgram],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);

                    TT::EndOfProgram
                }
                TT::EndOfProgram => {
                    parser.match_token(&[TT::EndOfProgram])?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_program(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_program(self, TT::KeywordProgram, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::Program(node_data);

        Ok(())
    }

    fn subroutines(&mut self, parent: usize) -> ParseResult<Option<usize>> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut tt = self.scanner.peek().token_type;
        while TT::KeywordFunction == tt || TT::KeywordProcedure == tt {
            let mut recovery_token = None;
            self.process(
                Parser::function,
                my_idx,
                &[TT::KeywordFunction, TT::KeywordProcedure],
                &mut recovery_token,
            )?;
            tt = self.scanner.peek().token_type;
        }

        let idx = if self.tree[my_idx].left_child.is_some() {
            self.tree[my_idx].data = NT::Subroutines(self.tree[my_idx].left_child.unwrap());
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

        fn parse_function<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenIdxOptIdxOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                // Handle "function" and "procedure" both here.
                TT::KeywordFunction => {
                    parser.process(
                        Parser::match_token,
                        &[TT::KeywordProcedure, TT::KeywordFunction],
                        &[
                            TT::LParen,
                            TT::RParen,
                            TT::KeywordVar,
                            TT::StatementSeparator,
                            TT::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| TT::Identifier)
                }
                TT::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TT::Identifier],
                        &[
                            TT::LParen,
                            TT::RParen,
                            TT::KeywordVar,
                            TT::StatementSeparator,
                            TT::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| TT::LParen)
                }
                TT::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TT::LParen],
                        &[
                            TT::RParen,
                            TT::KeywordVar,
                            TT::StatementSeparator,
                            TT::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| match parser.scanner.peek().token_type {
                        TT::KeywordVar | TT::Identifier => TT::KeywordVar,
                        _ => TT::RParen,
                    })
                }
                TT::RParen => {
                    parser.process(
                        Parser::match_token,
                        &[TT::RParen],
                        &[
                            TT::TypeSeparator,
                            TT::Type,
                            TT::StatementSeparator,
                            TT::KeywordBegin,
                        ],
                        &mut recovery_token,
                    )?;

                    recovery_token.unwrap_or_else(|| {
                        if TT::TypeSeparator == parser.scanner.peek().token_type {
                            TT::TypeSeparator
                        } else {
                            TT::StatementSeparator
                        }
                    })
                }
                TT::KeywordVar => {
                    node_data.opt_idx = parser
                        .process(
                            Parser::parameter_list,
                            my_idx,
                            &[TT::RParen, TT::StatementSeparator, TT::KeywordBegin],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| None);
                    recovery_token.unwrap_or_else(|| TT::RParen)
                }
                TT::TypeSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TT::TypeSeparator],
                        &[TT::Type, TT::StatementSeparator, TT::KeywordBegin],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::Type)
                }
                TT::Type => {
                    node_data.opt_idx2 = parser.process(
                        Parser::var_type,
                        my_idx,
                        &[TT::StatementSeparator, TT::KeywordBegin],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::StatementSeparator)
                }
                TT::StatementSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TT::StatementSeparator],
                        &[TT::KeywordBegin],
                        &mut recovery_token,
                    )?;
                    TT::KeywordBegin
                }
                TT::KeywordBegin => {
                    node_data.idx = parser
                        .process(
                            Parser::block,
                            my_idx,
                            &[TT::StatementSeparator],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);

                    parser.match_token(&[TT::StatementSeparator])?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_function(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_function(self, TT::KeywordFunction, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::Function(node_data);

        Ok(())
    }

    fn parameter_list(&mut self, parent: usize) -> ParseResult<Option<usize>> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut tt = self.scanner.peek().token_type;
        while TT::KeywordVar == tt || TT::Identifier == tt {
            let mut recovery_token = None;
            self.process(
                Parser::parameter,
                my_idx,
                &[TT::ListSeparator, TT::KeywordVar, TT::Identifier],
                &mut recovery_token,
            )?;

            match self.scanner.peek().token_type {
                TT::KeywordVar | TT::Identifier => {
                    // Mising list separator
                    self.logger.add_error(ErrorType::SyntaxError(
                        *self.scanner.peek(),
                        vec![TT::ListSeparator],
                    ));
                }
                TT::ListSeparator => {
                    self.match_token(&[TT::ListSeparator])?;
                }
                _ => {}
            }
            tt = self.scanner.peek().token_type;
        }

        let idx = if self.tree[my_idx].left_child.is_some() {
            self.tree[my_idx].data = NT::ParamList(self.tree[my_idx].left_child.unwrap());
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
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenIdxBool<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordVar => {
                    if TT::Identifier == parser.scanner.peek().token_type {
                        TT::Identifier
                    } else {
                        node_data.b = true;
                        parser.process(
                            Parser::match_token,
                            &[TT::KeywordVar],
                            &[TT::TypeSeparator, TT::Type],
                            &mut recovery_token,
                        )?;
                        recovery_token.unwrap_or_else(|| TT::Identifier)
                    }
                }
                TT::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TT::Identifier],
                        &[TT::TypeSeparator, TT::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::TypeSeparator)
                }
                TT::TypeSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TT::TypeSeparator],
                        &[TT::Type],
                        &mut recovery_token,
                    )?;
                    TT::Type
                }
                TT::Type => {
                    node_data.idx = parser.var_type(my_idx)?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_parameter(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_parameter(self, TT::KeywordVar, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::Parameter(node_data);
        Ok(())
    }

    fn var_type(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenSymbolType {
            token: None,
            st: ST::Undefined,
        };
        let mut expr_idx = None;

        fn parse_type<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenSymbolType<'a>,
            expr_idx: &mut Option<usize>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordArray => {
                    if TT::Type == parser.scanner.peek().token_type {
                        TT::Type
                    } else {
                        parser.process(
                            Parser::match_token,
                            &[TT::KeywordArray],
                            &[TT::LBracket, TT::RBracket, TT::KeywordOf, TT::Type],
                            &mut recovery_token,
                        )?;
                        recovery_token.unwrap_or_else(|| TT::LBracket)
                    }
                }
                TT::LBracket => {
                    parser.process(
                        Parser::match_token,
                        &[TT::LBracket],
                        &[TT::RBracket, TT::KeywordOf, TT::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralInt)
                }
                TT::LiteralInt => {
                    // LiteralInt might not actually be the next token. We're only using this as a
                    // label for this arm, which we get to from LBracket.
                    node_data.token = Some(*parser.scanner.peek());
                    *expr_idx = parser.process(
                        Parser::simple_expression,
                        my_idx,
                        &[TT::RBracket, TT::KeywordOf, TT::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::RBracket)
                }
                TT::RBracket => {
                    parser.process(
                        Parser::match_token,
                        &[TT::RBracket],
                        &[TT::KeywordOf, TT::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::KeywordOf)
                }
                TT::KeywordOf => {
                    parser.process(
                        Parser::match_token,
                        &[TT::KeywordOf],
                        &[TT::Type],
                        &mut recovery_token,
                    )?;
                    TT::Type
                }
                TT::Type => {
                    let token = parser.match_token(&[TT::Type])?;
                    if node_data.token.is_none() {
                        node_data.token = Some(token);
                    }

                    node_data.st = match expr_idx {
                        Some(idx) => match token.value {
                            "Boolean" => ST::ArrayBool(*idx),
                            "integer" => ST::ArrayInt(*idx),
                            "real" => ST::ArrayReal(*idx),
                            "string" => ST::ArrayString(*idx),
                            _ => ST::Undefined,
                        },
                        None => match token.value {
                            "Boolean" => ST::Bool,
                            "integer" => ST::Int,
                            "real" => ST::Real,
                            "string" => ST::String,
                            _ => ST::Undefined,
                        },
                    };
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_type(parser, tt, my_idx, node_data, expr_idx)?;
            }
            Ok(())
        }

        parse_type(
            self,
            TT::KeywordArray,
            my_idx,
            &mut node_data,
            &mut expr_idx,
        )?;
        self.tree[my_idx].data = NT::VariableType(node_data);
        Ok(my_idx)
    }

    fn block(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut recovery_token = None;
        self.process(
            Parser::match_token,
            &[TT::KeywordBegin],
            &[
                TT::KeywordVar,
                TT::KeywordBegin,
                TT::KeywordIf,
                TT::KeywordWhile,
                TT::KeywordRead,
                TT::KeywordWrite,
                TT::KeywordReturn,
                TT::KeywordAssert,
                TT::Identifier,
                TT::KeywordEnd,
            ],
            &mut recovery_token,
        )?;

        if recovery_token.is_none() || TT::KeywordEnd != recovery_token.unwrap() {
            self.statement_list(my_idx)?;
        }

        self.tree[my_idx].data = NT::Block(self.tree[my_idx].left_child.unwrap_or_else(|| !0));
        self.match_token(&[TT::KeywordEnd])?;

        Ok(my_idx)
    }

    fn statement_list(&mut self, parent: usize) -> ParseResult<()> {
        let mut recovery_token = None;
        self.process(
            Parser::statement,
            parent,
            &[
                TT::KeywordVar,
                TT::KeywordBegin,
                TT::KeywordIf,
                TT::KeywordWhile,
                TT::KeywordRead,
                TT::KeywordWrite,
                TT::KeywordReturn,
                TT::KeywordAssert,
                TT::StatementSeparator,
            ],
            &mut recovery_token,
        )?;

        match self.scanner.peek().token_type {
            TT::KeywordVar
            | TT::KeywordBegin
            | TT::KeywordIf
            | TT::KeywordWhile
            | TT::KeywordRead
            | TT::KeywordWrite
            | TT::KeywordReturn
            | TT::KeywordAssert
            | TT::Identifier => {
                // Mising statement separator
                self.logger.add_error(ErrorType::SyntaxError(
                    *self.scanner.peek(),
                    vec![TT::StatementSeparator],
                ));
            }
            TT::StatementSeparator => {
                self.match_token(&[TT::StatementSeparator])?;
            }
            _ => {}
        }

        if TT::KeywordEnd != self.scanner.peek().token_type {
            self.statement_list(parent)
        } else {
            Ok(())
        }
    }

    fn statement(&mut self, parent: usize) -> ParseResult<usize> {
        match self.scanner.peek().token_type {
            TT::KeywordVar => self.declaration(parent),
            TT::KeywordBegin => self.block(parent),
            TT::KeywordIf => self.if_statement(parent),
            TT::KeywordWhile => self.while_statement(parent),
            TT::KeywordReturn => self.return_statement(parent),
            TT::KeywordWrite => self.write_statement(parent),
            TT::KeywordRead => self.read_statement(parent),
            TT::KeywordAssert => self.assert_statement(parent),
            TT::Identifier => {
                if TT::LParen == self.scanner.peek_at(1).token_type {
                    self.call(parent)
                } else {
                    self.assignment(parent)
                }
            }
            _ => {
                // Unknown start of statement. Match to nothing, yielding a syntax error. Empty
                // statements and lexical errors lead to here.
                self.match_token(&[])?;
                Ok(!0)
            }
        }
    }

    fn id_list(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut recovery_token = None;
        self.tree[my_idx].data = NT::Variable(VariableData {
            token: self.process(
                Parser::match_token,
                &[TT::Identifier],
                &[TT::ListSeparator],
                &mut recovery_token,
            )?,
            st: ST::Undefined,
            is_ref: false,
            count: !0,
            depth: -1,
            array_idx: None,
            string_idx: None,
        });

        let tt = self.scanner.peek().token_type;
        if TT::ListSeparator == tt {
            self.match_token(&[TT::ListSeparator])?;
        } else if TT::Identifier == tt {
            // Mising list separator
            self.logger.add_error(ErrorType::SyntaxError(
                *self.scanner.peek(),
                vec![TT::ListSeparator],
            ));
        }

        if TT::Identifier == self.scanner.peek().token_type {
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
                &[TT::ListSeparator],
                &mut recovery_token,
            )?
            .unwrap_or_else(|| !0);

        if self.expression_follows() {
            // Mising list separator
            self.logger.add_error(ErrorType::SyntaxError(
                *self.scanner.peek(),
                vec![TT::ListSeparator],
            ));
        } else if TT::ListSeparator == self.scanner.peek().token_type {
            self.match_token(&[TT::ListSeparator])?;
        } else {
            return Ok(expr_idx);
        }

        self.argument_list(parent)?;
        Ok(expr_idx)
    }

    fn variable(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = VariableData {
            token: None,
            st: ST::Undefined,
            is_ref: false,
            count: !0,
            depth: -1,
            array_idx: None,
            string_idx: None,
        };

        fn parse_variable<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut VariableData<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TT::Identifier],
                        &[TT::LBracket, TT::RBracket],
                        &mut recovery_token,
                    )?;
                    if TT::LBracket == parser.scanner.peek().token_type {
                        TT::LBracket
                    } else {
                        TT::Undefined
                    }
                }
                TT::LBracket => {
                    parser.process(
                        Parser::match_token,
                        &[TT::LBracket],
                        &[TT::RBracket],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralInt)
                }
                TT::LiteralInt => {
                    // Label for array expression
                    node_data.array_idx = Some(
                        parser
                            .process(
                                Parser::expression,
                                my_idx,
                                &[TT::RBracket],
                                &mut recovery_token,
                            )?
                            .unwrap_or_else(|| !0),
                    );
                    TT::RBracket
                }
                TT::RBracket => {
                    parser.match_token(&[TT::RBracket])?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_variable(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_variable(self, TT::Identifier, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::Variable(node_data);
        Ok(my_idx)
    }

    fn variable_list(&mut self, parent: usize) -> ParseResult<usize> {
        let mut recovery_token = None;
        let variable_idx = self
            .process(
                Parser::variable,
                parent,
                &[TT::ListSeparator],
                &mut recovery_token,
            )?
            .unwrap_or_else(|| !0);

        match self.scanner.peek().token_type {
            TT::ListSeparator => {
                self.match_token(&[TT::ListSeparator])?;
            }
            TT::Identifier => {
                // Mising list separator
                self.logger.add_error(ErrorType::SyntaxError(
                    *self.scanner.peek(),
                    vec![TT::ListSeparator],
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
            tt: TT,
            my_idx: usize,
            node_data: &mut IdxIdx,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordVar => {
                    parser.process(
                        Parser::match_token,
                        &[TT::KeywordVar],
                        &[TT::TypeSeparator, TT::Type],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::Identifier)
                }
                TT::Identifier => {
                    node_data.idx = parser
                        .process(
                            Parser::id_list,
                            my_idx,
                            &[TT::TypeSeparator, TT::Type],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    recovery_token.unwrap_or_else(|| TT::TypeSeparator)
                }
                TT::TypeSeparator => {
                    parser.process(
                        Parser::match_token,
                        &[TT::TypeSeparator],
                        &[TT::Type],
                        &mut recovery_token,
                    )?;
                    TT::Type
                }
                TT::Type => {
                    node_data.idx2 = parser.var_type(my_idx)?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_declaration(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_declaration(self, TT::KeywordVar, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::Declaration(node_data);

        Ok(my_idx)
    }

    fn if_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdxIdxOptIdx {
            token: None,
            idx: !0,
            idx2: !0,
            opt_idx: None,
        };

        fn parse_if<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenIdxIdxOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordIf => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TT::KeywordIf],
                        &[TT::KeywordThen, TT::KeywordElse],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralInt)
                }
                TT::KeywordThen => {
                    parser.process(
                        Parser::match_token,
                        &[TT::KeywordThen],
                        &[TT::KeywordElse],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralString)
                }
                TT::KeywordElse => {
                    parser.match_token(&[TT::KeywordElse])?;
                    TT::LiteralBool
                }
                TT::LiteralInt => {
                    // Label for expression
                    node_data.idx = parser
                        .process(
                            Parser::expression,
                            my_idx,
                            &[TT::KeywordThen, TT::KeywordElse],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    recovery_token.unwrap_or_else(|| TT::KeywordThen)
                }
                TT::LiteralString => {
                    // Label for first statement
                    node_data.idx2 = parser
                        .process(
                            Parser::statement,
                            my_idx,
                            &[TT::KeywordElse],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);

                    if TT::KeywordElse == parser.scanner.peek().token_type {
                        TT::KeywordElse
                    } else {
                        TT::Undefined
                    }
                }
                TT::LiteralBool => {
                    // Label for second statement
                    node_data.opt_idx = Some(parser.statement(my_idx)?);
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_if(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_if(self, TT::KeywordIf, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::If(node_data);

        Ok(my_idx)
    }

    fn while_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdxIdx {
            token: None,
            idx: !0,
            idx2: !0,
        };

        fn parse_while<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenIdxIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordWhile => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TT::KeywordWhile],
                        &[TT::KeywordDo],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralInt)
                }
                TT::KeywordDo => {
                    parser.match_token(&[TT::KeywordDo])?;
                    TT::LiteralString
                }
                TT::LiteralInt => {
                    // Label for expression
                    node_data.idx = parser
                        .process(
                            Parser::expression,
                            my_idx,
                            &[TT::KeywordDo],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TT::KeywordDo
                }
                TT::LiteralString => {
                    // Label for statement
                    node_data.idx2 = parser.statement(my_idx)?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_while(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_while(self, TT::KeywordWhile, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::While(node_data);

        Ok(my_idx)
    }

    fn return_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenOptIdx {
            token: Some(self.match_token(&[TT::KeywordReturn])?),
            opt_idx: None,
        };

        node_data.opt_idx = if self.expression_follows() {
            Some(self.expression(my_idx)?)
        } else {
            None
        };

        self.tree[my_idx].data = NT::Return(node_data);
        Ok(my_idx)
    }

    fn write_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdx {
            token: None,
            idx: !0,
        };

        fn parse_write<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordWrite => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TT::KeywordWrite],
                        &[TT::LParen, TT::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LParen)
                }
                TT::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TT::LParen],
                        &[TT::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralInt)
                }
                TT::LiteralInt => {
                    // Label for argument list
                    node_data.idx = parser
                        .process(
                            Parser::argument_list,
                            my_idx,
                            &[TT::RParen],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TT::RParen
                }
                TT::RParen => {
                    parser.match_token(&[TT::RParen])?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_write(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_write(self, TT::KeywordWrite, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::Write(node_data);

        Ok(my_idx)
    }

    fn read_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut arg = !0;

        fn parse_read<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            arg: &mut usize,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordRead => {
                    parser.process(
                        Parser::match_token,
                        &[TT::KeywordRead],
                        &[TT::LParen, TT::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LParen)
                }
                TT::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TT::LParen],
                        &[TT::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralInt)
                }
                TT::LiteralInt => {
                    // Label for argument list
                    *arg = parser
                        .process(
                            Parser::variable_list,
                            my_idx,
                            &[TT::RParen],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TT::RParen
                }
                TT::RParen => {
                    parser.match_token(&[TT::RParen])?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_read(parser, tt, my_idx, arg)?;
            }
            Ok(())
        }

        parse_read(self, TT::KeywordRead, my_idx, &mut arg)?;
        self.tree[my_idx].data = NT::Read(arg);

        Ok(my_idx)
    }

    fn assert_statement(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenIdxOptIdx {
            token: None,
            idx: !0,
            opt_idx: None,
        };

        fn parse_assert<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenIdxOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::KeywordAssert => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TT::KeywordAssert],
                        &[TT::LParen, TT::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LParen)
                }
                TT::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TT::LParen],
                        &[TT::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralInt)
                }
                TT::LiteralInt => {
                    // Label for argument list
                    node_data.idx = parser
                        .process(
                            Parser::expression,
                            my_idx,
                            &[TT::RParen],
                            &mut recovery_token,
                        )?
                        .unwrap_or_else(|| !0);
                    TT::RParen
                }
                TT::RParen => {
                    parser.match_token(&[TT::RParen])?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_assert(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_assert(self, TT::KeywordAssert, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::Assert(node_data);

        Ok(my_idx)
    }

    fn call(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenOptIdx {
            token: None,
            opt_idx: None,
        };

        fn parse_call<'a, 'b>(
            parser: &mut Parser<'a, 'b>,
            tt: TT,
            my_idx: usize,
            node_data: &mut TokenOptIdx<'a>,
        ) -> ParseResult<()> {
            let mut recovery_token = None;
            let tt = match tt {
                TT::Identifier => {
                    node_data.token = parser.process(
                        Parser::match_token,
                        &[TT::Identifier],
                        &[TT::LParen, TT::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LParen)
                }
                TT::LParen => {
                    parser.process(
                        Parser::match_token,
                        &[TT::LParen],
                        &[TT::RParen],
                        &mut recovery_token,
                    )?;
                    recovery_token.unwrap_or_else(|| TT::LiteralInt)
                }
                TT::LiteralInt => {
                    // Label for argument list
                    if parser.expression_follows() {
                        node_data.opt_idx = parser.process(
                            Parser::argument_list,
                            my_idx,
                            &[TT::RParen],
                            &mut recovery_token,
                        )?;
                    }
                    TT::RParen
                }
                TT::RParen => {
                    parser.match_token(&[TT::RParen])?;
                    TT::Undefined
                }
                _ => {
                    assert!(false, "Token {} doesn't belong here.", tt);
                    TT::Undefined
                }
            };

            if TT::Undefined != tt {
                parse_call(parser, tt, my_idx, node_data)?;
            }
            Ok(())
        }

        parse_call(self, TT::Identifier, my_idx, &mut node_data)?;
        self.tree[my_idx].data = NT::Call(node_data);

        Ok(my_idx)
    }

    fn assignment(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = IdxIdx { idx: !0, idx2: !0 };

        let mut recovery_token = None;
        node_data.idx = self
            .process(
                Parser::variable,
                my_idx,
                &[TT::Assignment],
                &mut recovery_token,
            )?
            .unwrap_or_else(|| !0);
        self.match_token(&[TT::Assignment])?;
        node_data.idx2 = self.expression(my_idx)?;

        self.tree[my_idx].data = NT::Assignment(node_data);
        Ok(my_idx)
    }

    // ---------------------------------------------------------------------
    // Functions for expressions
    // ---------------------------------------------------------------------
    fn expression(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenSymbolIdxIdx {
            token: None,
            st: ST::Undefined,
            idx: !0,
            idx2: !0,
        };

        let operators = [
            TT::OperatorEqual,
            TT::OperatorNotEqual,
            TT::OperatorGreaterEqual,
            TT::OperatorGreater,
            TT::OperatorLessEqual,
            TT::OperatorLess,
        ];
        let mut recovery_token = None;
        node_data.idx = self
            .process(
                Parser::simple_expression,
                my_idx,
                &operators,
                &mut recovery_token,
            )?
            .unwrap_or_else(|| !0);

        if let Some(_) = operators
            .iter()
            .find(|&&op| op == self.scanner.peek().token_type)
        {
            recovery_token = None;
            node_data.token = self.process(
                Parser::match_token,
                &operators,
                Parser::EXPRESSION_FIRST,
                &mut recovery_token,
            )?;

            node_data.idx2 = self.simple_expression(my_idx)?;
            self.tree[my_idx].data = NT::RelOp(node_data);
            Ok(my_idx)
        } else {
            self.tree.remove_node(my_idx);
            Ok(node_data.idx)
        }
    }

    fn simple_expression(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenSymbolIdxOptIdx {
            token: None,
            st: ST::Undefined,
            idx: !0,
            opt_idx: None,
        };

        let mut recovery_token = None;
        let operators = [TT::OperatorPlus, TT::OperatorMinus, TT::OperatorOr];

        let return_idx = match self.scanner.peek().token_type {
            // First term may have a sign in front
            tt @ TT::OperatorPlus | tt @ TT::OperatorMinus => {
                node_data.token = self.process(
                    Parser::match_token,
                    &[tt],
                    Parser::EXPRESSION_FIRST,
                    &mut recovery_token,
                )?;
                node_data.idx = self.simple_expression(my_idx)?;
                my_idx
            }
            _ => {
                node_data.idx = self
                    .process(Parser::term, my_idx, &operators, &mut recovery_token)?
                    .unwrap_or_else(|| !0);

                if let Some(_) = operators
                    .iter()
                    .find(|&&op| op == self.scanner.peek().token_type)
                {
                    recovery_token = None;
                    node_data.token = self.process(
                        Parser::match_token,
                        &operators,
                        Parser::EXPRESSION_FIRST,
                        &mut recovery_token,
                    )?;

                    node_data.opt_idx = Some(self.simple_expression(my_idx)?);
                    my_idx
                } else {
                    node_data.idx
                }
            }
        };

        if return_idx == my_idx {
            self.tree[my_idx].data = NT::AddOp(node_data);
        } else {
            self.tree.remove_node(my_idx);
        }

        Ok(return_idx)
    }

    fn term(&mut self, parent: usize) -> ParseResult<usize> {
        let my_idx = self.tree.add_child(Some(parent));
        let mut node_data = TokenSymbolIdxIdx {
            token: None,
            st: ST::Undefined,
            idx: !0,
            idx2: !0,
        };

        let mut recovery_token = None;
        let operators = [
            TT::OperatorMultiply,
            TT::OperatorDivide,
            TT::OperatorModulo,
            TT::OperatorAnd,
        ];

        node_data.idx = self
            .process(Parser::factor, my_idx, &operators, &mut recovery_token)?
            .unwrap_or_else(|| !0);

        let return_idx = if let Some(_) = operators
            .iter()
            .find(|&&op| op == self.scanner.peek().token_type)
        {
            recovery_token = None;
            node_data.token = self.process(
                Parser::match_token,
                &operators,
                Parser::EXPRESSION_FIRST,
                &mut recovery_token,
            )?;

            node_data.idx2 = self.term(my_idx)?;
            my_idx
        } else {
            node_data.idx
        };

        if return_idx == my_idx {
            self.tree[my_idx].data = NT::MulOp(node_data);
        } else {
            self.tree.remove_node(my_idx);
        }

        Ok(return_idx)
    }

    fn factor(&mut self, parent: usize) -> ParseResult<usize> {
        // Add a possible parent node for the .size operator,
        // which may follow another factor
        let size_idx = self.tree.add_child(Some(parent));

        let my_idx = self.tree.add_child(Some(size_idx));
        let mut node_data = TokenIdx {
            token: None,
            idx: !0,
        };

        let mut recovery_token = None;
        node_data.idx = match self.scanner.peek().token_type {
            TT::Identifier => {
                let func = if TT::LParen == self.scanner.peek_at(1).token_type {
                    Parser::call
                } else {
                    Parser::variable
                };
                self.process(func, my_idx, &[TT::OperatorSize], &mut recovery_token)?
                    .unwrap_or_else(|| !0)
            }
            tt @ TT::LiteralBool
            | tt @ TT::LiteralInt
            | tt @ TT::LiteralReal
            | tt @ TT::LiteralString => {
                let lit_idx = self.tree.add_child(Some(my_idx));
                let token = self.process(
                    Parser::match_token,
                    &[tt],
                    &[TT::OperatorSize],
                    &mut recovery_token,
                )?;
                self.tree[lit_idx].data = NT::Literal(TokenOptIdx {
                    token: token,
                    opt_idx: None,
                });
                lit_idx
            }
            TT::LParen => {
                self.process(
                    Parser::match_token,
                    &[TT::LParen],
                    Parser::EXPRESSION_FIRST,
                    &mut recovery_token,
                )?;

                recovery_token = None;
                let expr_idx = self
                    .process(
                        Parser::expression,
                        my_idx,
                        &[TT::RParen],
                        &mut recovery_token,
                    )?
                    .unwrap_or_else(|| !0);

                recovery_token = None;
                self.process(
                    Parser::match_token,
                    &[TT::RParen],
                    &[TT::OperatorSize],
                    &mut recovery_token,
                )?;

                expr_idx
            }
            TT::OperatorNot => {
                node_data.token = self.process(
                    Parser::match_token,
                    &[TT::OperatorNot],
                    Parser::EXPRESSION_FIRST,
                    &mut recovery_token,
                )?;

                recovery_token = None;
                self.process(
                    Parser::factor,
                    my_idx,
                    &[TT::OperatorSize],
                    &mut recovery_token,
                )?
                .unwrap_or_else(|| !0)
            }
            _ => {
                self.match_token(&[])?;
                !0
            }
        };

        // This factor is a useless node, if it has no token
        let return_idx = if node_data.token.is_none() {
            self.tree.remove_node(my_idx);
            node_data.idx
        } else {
            self.tree[my_idx].data = NT::Not(node_data);
            my_idx
        };

        // Remove the "parent" factor node that was created at start, if there is no size operator
        let return_idx = if TT::OperatorSize == self.scanner.peek().token_type {
            self.tree[size_idx].data = NT::ArraySize(TokenIdx {
                token: Some(self.match_token(&[TT::OperatorSize])?),
                idx: return_idx,
            });
            size_idx
        } else {
            self.tree.remove_node(size_idx);
            return_idx
        };

        Ok(return_idx)
    }

    // ---------------------------------------------------------------------
    // Other functions
    // ---------------------------------------------------------------------
    fn expression_follows(&mut self) -> bool {
        Parser::EXPRESSION_FIRST
            .iter()
            .find(|&&op| op == self.scanner.peek().token_type)
            .is_some()
    }

    // ---------------------------------------------------------------------
    // Public functions
    // ---------------------------------------------------------------------
    pub fn new(
        source_str: &'a str,
        logger: &'b mut Logger<'a>,
        tree: &'b mut LcRsTree<NT<'a>>,
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
        let mut recovery_token = None;
        match self.process(Parser::program, 0, &[TT::EOF], &mut recovery_token) {
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

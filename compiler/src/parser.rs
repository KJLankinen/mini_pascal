use super::data_types::{ErrorType, NodeType, SymbolType, TokenData, TokenType};
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

        // Token was none of the expected tokens. If the token is undefined, we have a lexical
        // error, which will be handled below (for each consecutive undefined token that is found
        // before the next recovery token). If the token is a known token, it is still unexpected,
        // so mark it as a syntax error.
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
        self.tree[my_id].data = NodeType::Program;

        let mut recovery_token = None;
        self.process(
            Parser::statement_list,
            my_id,
            &[TokenType::EndOfProgram],
            &mut recovery_token,
        )?;
        self.process(
            Parser::end_of_program,
            0,
            &[TokenType::EndOfProgram],
            &mut recovery_token,
        )?;

        Ok(())
    }

    fn end_of_program(&mut self, _parent: usize) -> ParseResult<()> {
        self.match_token(&[TokenType::EndOfProgram])?;
        Ok(())
    }

    fn statement_list(&mut self, parent: usize) -> ParseResult<()> {
        // Stop recursion if the next token is end of program
        // or 'end' keyword that ends the for loop.
        match self.scanner.peek().token_type {
            TokenType::EndOfProgram | TokenType::KeywordEnd => {}
            _ => {
                // If the statement encounters an error it can't be handle at any earlier level,
                // the parsing can at least continue from the start of the next statement.
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

    fn statement(&mut self, parent: usize) -> ParseResult<()> {
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
                // recorded as a syntax error by the match_token() function, as it usually is.
                // Thus, record it here.
                self.logger.add_error(ErrorType::SyntaxError(
                    *self.scanner.peek(),
                    vec![TokenType::EndOfStatement],
                ));
            }
        }
        Ok(())
    }

    fn statement_prefix(&mut self, parent: usize) -> ParseResult<()> {
        match self.scanner.peek().token_type {
            TokenType::KeywordVar => self.declaration(parent)?,
            TokenType::KeywordFor => self.for_loop(parent)?,
            TokenType::KeywordRead => self.read_statement(parent)?,
            TokenType::KeywordPrint => self.print_statement(parent)?,
            TokenType::KeywordAssert => self.assert_statement(parent)?,
            TokenType::Identifier => self.assignment(parent)?,
            _ => {
                // Unknown start of statement. Match to nothing, yielding a syntax error. Empty
                // statements and lexical errors lead to here.
                self.match_token(&[])?;
            }
        }

        Ok(())
    }

    fn expression(&mut self, parent: usize) -> ParseResult<()> {
        let my_id = self.tree.add_child(Some(parent));

        // First case: Unary operator and operand
        if TokenType::OperatorNot == self.scanner.peek().token_type {
            // We just peeked this token, so the match is a certainty.
            let token = self.match_token(&[TokenType::OperatorNot])?;
            self.tree[my_id].data = NodeType::Expression {
                operator: Some(token),
                symbol_type: SymbolType::Undefined,
            };
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
                self.tree[my_id].data = NodeType::Expression {
                    operator: Some(token),
                    symbol_type: SymbolType::Undefined,
                };

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

    fn operand(&mut self, parent: usize) -> ParseResult<()> {
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
            let my_id = self.tree.add_child(Some(parent));
            let symbol_type = match token.token_type {
                TokenType::LiteralInt => SymbolType::Int,
                TokenType::LiteralString => SymbolType::String,
                TokenType::Identifier => SymbolType::Undefined,
                _ => SymbolType::Undefined,
            };
            self.tree[my_id].data = NodeType::Operand {
                token: Some(token),
                symbol_type: symbol_type,
            };
        }
        Ok(())
    }

    // ---------------------------------------------------------------------
    // Functions for each of the possible statements.
    // fn statement_prefix() leads to one of these
    // ---------------------------------------------------------------------
    // Some of these functions use closures. The closures are used as recovery points, i.e. points
    // at which the parsing of the code can be resumed. Each recovery token is associated with a
    // closure. If an error is found and a recovery token is matched, the associated closure is
    // called to continue parsing. The earlier closures can call later closures. This means that
    // the closures have to be declared in reverse order relative to the parsed code. E.g. in the for loop
    // function, the "do" closure is declared before "expression", "range" or "in" closures.
    fn declaration(&mut self, parent: usize) -> ParseResult<()> {
        // Declaration with an optional assignment
        let my_id = self.tree.add_child(Some(parent));
        self.match_token(&[TokenType::KeywordVar])?;

        // Revovery point for expression
        let expr_closure = |parser: &mut Self| -> ParseResult<()> { parser.expression(my_id) };

        // Recovery point for ':=' token
        let assignment_closure = |parser: &mut Self| -> ParseResult<()> {
            // The statement can contain an assignment. If it does, use the value the user
            // provided as the right sibling. Otherwise use some later specified default.
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
            }

            Ok(())
        };

        // This will be set inside the type_closure.
        let mut symbol_type = SymbolType::Undefined;
        // Recovery point for 'int', 'string' and 'bool' tokens
        let mut type_closure = |parser: &mut Self| -> ParseResult<()> {
            let mut recovery_token = None;
            let token = parser.process(
                Parser::match_token,
                &[TokenType::Type],
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

            // Specify the symbol type of the identifier in this declaration.
            // It will be used later by semantic analysis and interpretation.
            symbol_type = match token.unwrap_or_else(|| TokenData::default()).value {
                "int" => SymbolType::Int,
                "string" => SymbolType::String,
                "bool" => SymbolType::Bool,
                _ => SymbolType::Undefined,
            };

            if token.is_some()
                || TokenType::Assignment
                    == recovery_token.expect("Recovery token must be some token.")
            {
                assignment_closure(parser)?;
            } else {
                expr_closure(parser)?;
            }

            Ok(())
        };

        // Recovery point for ':' token
        let mut separator_closure = |parser: &mut Self| -> ParseResult<()> {
            let mut recovery_token = None;
            let result = parser.process(
                Parser::match_token,
                &[TokenType::TypeSeparator],
                &[
                    TokenType::Type,
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
                type_closure(parser)?;
            } else {
                match recovery_token.expect("Recovery token must be some token.") {
                    TokenType::Type => type_closure(parser)?,
                    TokenType::Assignment => assignment_closure(parser)?,
                    _ => expr_closure(parser)?,
                };
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
                TokenType::Type,
                TokenType::Assignment,
                TokenType::OperatorNot,
                TokenType::LParen,
                TokenType::LiteralInt,
                TokenType::LiteralString,
                TokenType::Identifier,
            ],
            &mut recovery_token,
        )?;

        if token.is_some() {
            separator_closure(self)?;
        } else {
            match recovery_token.expect("Recovery token must be some token.") {
                TokenType::TypeSeparator => separator_closure(self)?,
                TokenType::Type => type_closure(self)?,
                TokenType::Assignment => assignment_closure(self)?,
                _ => expr_closure(self)?,
            };
        }

        self.tree[my_id].data = NodeType::Declaration {
            identifier: token,
            symbol_type: symbol_type,
            expression: self.tree[my_id].left_child,
        };

        Ok(())
    }

    fn for_loop(&mut self, parent: usize) -> ParseResult<()> {
        // For loop
        let my_id = self.tree.add_child(Some(parent));
        self.match_token(&[TokenType::KeywordFor])?;

        // Recovery point for the last 'for' token
        let for_closure = |parser: &mut Self| -> ParseResult<()> {
            parser.match_token(&[TokenType::KeywordFor])?;
            Ok(())
        };

        // Recovery point for the 'end' token
        let end_closure = |parser: &mut Self| -> ParseResult<()> {
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
        let do_closure = |parser: &mut Self| -> ParseResult<()> {
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
        let expr_closure = |parser: &mut Self| -> ParseResult<()> {
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
        let range_closure = |parser: &mut Self| -> ParseResult<()> {
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
                match recovery_token.expect("Recovery token must be some token.") {
                    TokenType::KeywordDo => do_closure(parser)?,
                    _ => expr_closure(parser)?,
                };

                Ok(())
            }
        };

        // Recovery point for the first expression
        let first_expr_closure = |parser: &mut Self| -> ParseResult<()> {
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
                match recovery_token.expect("Recovery token must be some token.") {
                    TokenType::KeywordDo => do_closure(parser)?,
                    TokenType::Range => range_closure(parser)?,
                    _ => expr_closure(parser)?,
                };

                Ok(())
            }
        };

        // Recovery point for the 'in' token
        let in_closure = |parser: &mut Self| -> ParseResult<()> {
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
                match recovery_token.expect("Recovery token must be some token.") {
                    TokenType::Range => range_closure(parser)?,
                    _ => do_closure(parser)?,
                };

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

        if token.is_some() {
            in_closure(self)?
        } else {
            match recovery_token.expect("Recovery token must be some token.") {
                TokenType::KeywordIn => in_closure(self)?,
                TokenType::KeywordDo => do_closure(self)?,
                _ => range_closure(self)?,
            };
        }

        let second_expr;
        let first_statement;
        let first_expr = match self.tree[my_id].left_child {
            Some(v) => {
                second_expr = match self.tree[v].right_sibling {
                    Some(v) => {
                        first_statement = self.tree[v].right_sibling;
                        v
                    }
                    None => {
                        first_statement = None;
                        !0
                    }
                };
                v
            }
            None => {
                second_expr = !0;
                first_statement = None;
                !0
            }
        };

        self.tree[my_id].data = NodeType::For {
            identifier: token,
            start_expression: first_expr,
            end_expression: second_expr,
            first_statement: first_statement,
        };

        Ok(())
    }

    fn read_statement(&mut self, parent: usize) -> ParseResult<()> {
        // Read statement
        self.match_token(&[TokenType::KeywordRead])?;

        let token = self.match_token(&[TokenType::Identifier])?;
        let my_id = self.tree.add_child(Some(parent));
        self.tree[my_id].data = NodeType::Read {
            identifier: Some(token),
        };

        Ok(())
    }

    fn print_statement(&mut self, parent: usize) -> ParseResult<()> {
        // Print statement
        let my_id = self.tree.add_child(Some(parent));
        let token = self.match_token(&[TokenType::KeywordPrint])?;

        // No point in recovery tokens, since the expression is the last part of this
        // statement.
        self.expression(my_id)?;

        // Store expression idx for later use
        let expression = self.tree[my_id].left_child.unwrap_or_else(|| !0);
        self.tree[my_id].data = NodeType::Print {
            token: Some(token),
            expression: expression,
        };

        Ok(())
    }

    fn assert_statement(&mut self, parent: usize) -> ParseResult<()> {
        // Assertion statement
        let my_id = self.tree.add_child(Some(parent));
        let token = self.match_token(&[TokenType::KeywordAssert])?;

        // Recovery point for the ')' token
        let paren_closure = |parser: &mut Self| -> ParseResult<()> {
            parser.match_token(&[TokenType::RParen])?;
            Ok(())
        };

        // Recovery point for the expression
        let expr_closure = |parser: &mut Self| -> ParseResult<()> {
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

        if self
            .process(
                Parser::match_token,
                &[TokenType::LParen],
                &recovery_tokens,
                &mut recovery_token,
            )?
            .is_some()
        {
            expr_closure(self)?;
        } else {
            match recovery_token.expect("Recovery token must be some token.") {
                TokenType::RParen => paren_closure(self)?,
                _ => expr_closure(self)?,
            };
        }

        // Store expression idx for later use.
        let expression = self.tree[my_id].left_child.unwrap_or_else(|| !0);
        self.tree[my_id].data = NodeType::Assert {
            token: Some(token),
            expression: expression,
        };

        Ok(())
    }

    fn assignment(&mut self, parent: usize) -> ParseResult<()> {
        // Assignment to a variable
        let token = self.match_token(&[TokenType::Identifier])?;
        let my_id = self.tree.add_child(Some(parent));

        // We can recover at the start of an expression, if ':=' is not found.
        let mut recovery_token = None;
        self.process(
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

        self.expression(my_id)?;

        // Store expression idx for later use
        let expression = self.tree[my_id].left_child.unwrap_or_else(|| !0);
        self.tree[my_id].data = NodeType::Assignment {
            identifier: Some(token),
            expression: expression,
        };

        Ok(())
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

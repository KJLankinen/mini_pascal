use super::lcrs_tree::{LcRsTree, Update};
use super::scanner::{Scanner, TokenData, TokenType};
use serde::Serialize;

// ---------------------------------------------------------------------
// Type definition for the recursive descent parser
// ---------------------------------------------------------------------
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    tree: LcRsTree<NodeData<'a>>,
    recursion_depth: usize,
}

// ---------------------------------------------------------------------
// Method implementations for the parser
// ---------------------------------------------------------------------
impl<'a> Parser<'a> {
    fn match_token(&mut self, token_types: &[TokenType]) -> Result<TokenData<'a>, TokenData<'a>> {
        let token = self.scanner.next()?;

        for tt in token_types {
            if token.token_type == *tt {
                return Ok(token);
            }
        }

        Err(token)
    }

    fn process<O, E>(
        &mut self,
        func: fn(&mut Self, Option<usize>) -> Result<O, E>,
        arg1: Option<usize>,
    ) -> Result<O, E> {
        self.recursion_depth += 1;
        let res = func(self, arg1);
        self.recursion_depth -= 1;

        return res;
    }

    // ---------------------------------------------------------------------
    // Recursive processing functions
    // ---------------------------------------------------------------------

    fn program(&mut self, _parent: Option<usize>) -> Result<(), TokenData<'a>> {
        let my_id = self.tree.add_child(None);
        self.tree.update_data(
            my_id,
            NodeData {
                node_type: Some(NodeType::Program),
                token: None,
            },
        );
        self.process(Parser::statement_list, my_id)?;
        self.process(Parser::end_of_program, None)?;
        Ok(())
    }

    fn end_of_program(&mut self, _parent: Option<usize>) -> Result<(), TokenData<'a>> {
        if self.scanner.unmatched_multiline_comment_prefixes.is_empty() {
            self.match_token(&[TokenType::EndOfProgram])?;
        } else {
            for (line, col) in &self.scanner.unmatched_multiline_comment_prefixes {
                println!("Runaway multi line comment:");
                self.scanner.print_line(*line as usize, *col as usize);
            }
        }
        Ok(())
    }

    fn statement_list(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        // Stop recursion if the next token is end of program
        // or 'end' keyword that ends the for loop.
        match self.scanner.peek().token_type {
            TokenType::EndOfProgram | TokenType::KeywordEnd => {}
            _ => {
                self.process(Parser::statement, parent)?;
                self.process(Parser::statement_list, parent)?;
            }
        }
        Ok(())
    }

    fn statement(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        self.process(Parser::statement_prefix, parent)?;
        self.match_token(&[TokenType::EndOfStatement])?;
        Ok(())
    }

    fn statement_prefix(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        match self.scanner.peek().token_type {
            TokenType::KeywordVar => {
                // Declaration with an optional assignment
                let my_id = self.tree.add_child(parent);
                self.match_token(&[TokenType::KeywordVar])?;

                // Add node for the identifier
                // Literals and identifiers use the same node type, Operand.
                // Which the node actually represents can be disambiguated by the type of the token.
                let id = self.tree.add_child(my_id);
                let token = self.match_token(&[TokenType::Identifier])?;
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
                    self.match_token(&[TokenType::Assignment])?;
                    self.process(Parser::expression, my_id)?;
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
                self.process(Parser::expression, range_id)?;
                let token = self.match_token(&[TokenType::Range])?;
                self.tree.update_data(
                    range_id,
                    NodeData {
                        node_type: Some(NodeType::Range),
                        token: Some(token),
                    },
                );
                self.process(Parser::expression, range_id)?;

                self.match_token(&[TokenType::KeywordDo])?;
                // Process however many statements there are inside the for block
                self.process(Parser::statement_list, my_id)?;
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

                let id = self.tree.add_child(my_id);
                let token = self.match_token(&[TokenType::Identifier])?;
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

                self.process(Parser::expression, my_id)?;
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

                self.match_token(&[TokenType::LParen])?;
                self.process(Parser::expression, my_id)?;
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

                let token = self.match_token(&[TokenType::Assignment])?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Assignment),
                        token: Some(token),
                    },
                );

                self.process(Parser::expression, my_id)?;
            }
            _ => {
                println!("Invalid start of statement!");
            }
        }
        Ok(())
    }

    fn expression(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        let my_id = self.tree.add_child(parent);

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

            return Ok(());
        }

        // Second and third case start with an operand
        self.process(Parser::operand, my_id)?;

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
                self.process(Parser::operand, my_id)?;
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

    fn operand(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        if TokenType::LParen == self.scanner.peek().token_type {
            self.match_token(&[TokenType::LParen])?;
            self.process(Parser::expression, parent)?;
            self.match_token(&[TokenType::RParen])?;
        } else {
            let my_id = self.tree.add_child(parent);
            let token = self.match_token(&[
                TokenType::LiteralInt,
                TokenType::LiteralString,
                TokenType::Identifier,
            ])?;
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
        }
    }

    pub fn parse(&mut self) {
        match self.process(Parser::program, None) {
            Ok(_) => {}
            Err(_) => assert!(false, "Unhandled error at parse."),
        }
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

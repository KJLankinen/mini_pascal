use super::lcrs_tree::{LcRsTree, Update};
use super::scanner::{Scanner, TokenData, TokenType};
use serde::Serialize;

// ---------------------------------------------------------------------
// Type definition for the recursive descent parser
// ---------------------------------------------------------------------
pub struct Parser<'a> {
    scanner: Scanner<'a>,
    tree: LcRsTree<NodeData<'a>>,
}

// ---------------------------------------------------------------------
// Method implementations for the parser
// ---------------------------------------------------------------------
impl<'a> Parser<'a> {
    fn match_token(&mut self, token_type: TokenType) -> Result<TokenData<'a>, TokenData<'a>> {
        let token = self.scanner.next()?;

        if token.token_type == token_type {
            Ok(token)
        } else {
            Err(token)
        }
    }

    // ---------------------------------------------------------------------
    // Recursive processing functions
    // ---------------------------------------------------------------------

    fn process_program(&mut self) -> Result<(), TokenData<'a>> {
        let my_id = self.tree.add_child(None);
        self.tree.update_data(
            my_id,
            NodeData {
                node_type: Some(NodeType::Program),
                token: None,
            },
        );
        self.process_statement_list(my_id)?;
        self.process_end_of_program()?;
        Ok(())
    }

    fn process_end_of_program(&mut self) -> Result<(), TokenData<'a>> {
        if self.scanner.unmatched_multiline_comment_prefixes.is_empty() {
            self.match_token(TokenType::EndOfProgram)?;
        } else {
            for (line, col) in &self.scanner.unmatched_multiline_comment_prefixes {
                println!("Runaway multi line comment:");
                self.scanner.print_line(*line as usize, *col as usize);
            }
        }
        Ok(())
    }

    fn process_statement_list(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        // Stop recursion if the next token is end of program
        // or 'end' keyword that ends the for loop.
        match self.scanner.peek().token_type {
            TokenType::EndOfProgram | TokenType::KeywordEnd => {}
            _ => {
                self.process_statement(parent)?;
                self.process_statement_list(parent)?;
            }
        }
        Ok(())
    }

    fn process_statement(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        self.process_statement_prefix(parent)?;
        self.match_token(TokenType::EndOfStatement)?;
        Ok(())
    }

    fn process_statement_prefix(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        match self.scanner.peek().token_type {
            TokenType::KeywordVar => {
                // Declaration with an optional assignment
                let my_id = self.tree.add_child(parent);
                self.match_token(TokenType::KeywordVar)?;

                // Add node for the identifier
                // Literals and identifiers use the same node type, Operand.
                // Which the node actually represents can be disambiguated by the type of the token.
                let id = self.tree.add_child(my_id);
                let token = self.match_token(TokenType::Identifier)?;
                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );

                self.match_token(TokenType::TypeSeparator)?;

                // Store the type as the token of the declaration node
                let token: Option<TokenData>;
                match self.scanner.peek().token_type {
                    token_type @ TokenType::TypeInt
                    | token_type @ TokenType::TypeString
                    | token_type @ TokenType::TypeBool => {
                        token = Some(self.match_token(token_type)?);
                    }
                    _ => {
                        // A syntax error, let's pass something that we know it's not
                        // so the given token is a syntax error
                        token = Some(self.match_token(TokenType::TypeInt)?);
                    }
                }

                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Declaration),
                        token: token,
                    },
                );

                // The statement can contain an assignment. If it does, use the value the user
                // provided as the right sibling. Otherwise use some default.
                if TokenType::Assignment == self.scanner.peek().token_type {
                    self.match_token(TokenType::Assignment)?;
                    self.process_expression(my_id)?;
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
                let token = self.match_token(TokenType::KeywordFor)?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::For),
                        token: Some(token),
                    },
                );

                let id = self.tree.add_child(my_id);
                let token = self.match_token(TokenType::Identifier)?;
                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );

                self.match_token(TokenType::KeywordIn)?;

                // The range token '..' is the parent of the two expressions
                let range_id = self.tree.add_child(my_id);
                self.process_expression(range_id)?;
                let token = self.match_token(TokenType::Range)?;
                self.tree.update_data(
                    range_id,
                    NodeData {
                        node_type: Some(NodeType::Range),
                        token: Some(token),
                    },
                );
                self.process_expression(range_id)?;

                self.match_token(TokenType::KeywordDo)?;
                // Process however many statements there are inside the for block
                self.process_statement_list(my_id)?;
                self.match_token(TokenType::KeywordEnd)?;
                self.match_token(TokenType::KeywordFor)?;
            }
            TokenType::KeywordRead => {
                // Read statement
                let my_id = self.tree.add_child(parent);
                let token = self.match_token(TokenType::KeywordRead)?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Read),
                        token: Some(token),
                    },
                );

                let id = self.tree.add_child(my_id);
                let token = self.match_token(TokenType::Identifier)?;
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
                let token = self.match_token(TokenType::KeywordPrint)?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Print),
                        token: Some(token),
                    },
                );

                self.process_expression(my_id)?;
            }
            TokenType::KeywordAssert => {
                // Assertion statement
                let my_id = self.tree.add_child(parent);
                let token = self.match_token(TokenType::KeywordAssert)?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Assert),
                        token: Some(token),
                    },
                );

                self.match_token(TokenType::LParen)?;
                self.process_expression(my_id)?;
                self.match_token(TokenType::RParen)?;
            }
            TokenType::Identifier => {
                // Assignment to a variable
                let my_id = self.tree.add_child(parent);
                let id = self.tree.add_child(my_id);
                let token = self.match_token(TokenType::Identifier)?;

                self.tree.update_data(
                    id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );

                let token = self.match_token(TokenType::Assignment)?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Assignment),
                        token: Some(token),
                    },
                );

                self.process_expression(my_id)?;
            }
            _ => {
                println!("Invalid start of statement!");
            }
        }
        Ok(())
    }

    fn process_expression(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        let my_id = self.tree.add_child(parent);

        // First case: Unary operator and operand
        if TokenType::OperatorNot == self.scanner.peek().token_type {
            let token = self.match_token(TokenType::OperatorNot)?;
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
        self.process_operand(my_id)?;

        match self.scanner.peek().token_type {
            token_type @ TokenType::OperatorPlus
            | token_type @ TokenType::OperatorMinus
            | token_type @ TokenType::OperatorMultiply
            | token_type @ TokenType::OperatorDivide
            | token_type @ TokenType::OperatorLessThan
            | token_type @ TokenType::OperatorEqual
            | token_type @ TokenType::OperatorAnd => {
                // Second case: operand, binary operator, operand
                let token = self.match_token(token_type)?;
                self.process_operand(my_id)?;
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

    fn process_operand(&mut self, parent: Option<usize>) -> Result<(), TokenData<'a>> {
        match self.scanner.peek().token_type {
            token_type @ TokenType::LParen => {
                self.match_token(token_type)?;
                self.process_expression(parent)?;
                self.match_token(TokenType::RParen)?;
            }
            token_type @ TokenType::LiteralInt
            | token_type @ TokenType::LiteralString
            | token_type @ TokenType::Identifier => {
                let my_id = self.tree.add_child(parent);
                let token = self.match_token(token_type)?;
                self.tree.update_data(
                    my_id,
                    NodeData {
                        node_type: Some(NodeType::Operand),
                        token: Some(token),
                    },
                );
            }
            _ => {
                // This is unexpected and an error situation
                assert!(false, "Missing operand!");
            }
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
        }
    }

    pub fn parse(&mut self) {
        match self.process_program() {
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
    Unexpected,
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

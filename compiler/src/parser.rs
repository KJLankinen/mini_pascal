use super::scanner::Scanner;
use super::scanner::TokenData;
use super::scanner::TokenType;
use serde::Serialize;

#[derive(Serialize, PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum NodeType {
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

#[derive(Serialize, Debug, Clone)]
pub struct Node<'a> {
    pub parent: usize,
    pub left_child: usize,
    pub right_sibling: usize,
    pub node_type: NodeType,
    pub token: TokenData<'a>,
}

impl<'a> Node<'a> {
    fn new(
        parent: usize,
        left_child: usize,
        right_sibling: usize,
        node_type: NodeType,
        token: TokenData<'a>,
    ) -> Self {
        Node {
            parent: parent,
            left_child: left_child,
            right_sibling: right_sibling,
            node_type: node_type,
            token: token,
        }
    }
}

impl<'a> Default for Node<'a> {
    fn default() -> Node<'a> {
        Node::new(!0, !0, !0, NodeType::Unexpected, TokenData::default())
    }
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    tree: Vec<Node<'a>>,
}

// A recursive descent parser
impl<'a> Parser<'a> {
    pub fn new(source_str: &'a str) -> Self {
        Parser {
            scanner: Scanner::new(source_str),
            tree: vec![],
        }
    }

    pub fn parse(&mut self) {
        self.process_program();
        super::write_tree_to_json(&self.tree);
    }

    fn add_node(&mut self, parent: usize, previous: usize) -> usize {
        let id = self.tree.len();
        self.tree.push(Node::default());
        self.tree[id].parent = parent;

        if 0 == id {
            // This is the first node, don't bother modifying the non-existent previous entry
        } else if previous == parent {
            // This node is the first child of the parent node
            self.tree[previous].left_child = id;
        } else {
            self.tree[previous].right_sibling = id;
        }

        id
    }

    fn match_token(&mut self, token_type: TokenType) -> TokenData<'a> {
        let token = self.scanner.next().unwrap();
        if token_type == token.token_type {
            println!("Dandy! {:?}", token);
        } else {
            println!("Unexpected token {:?}", token);
        }

        token
    }

    fn process_program(&mut self) {
        let my_id = self.add_node(!0, !0);
        self.tree[my_id].node_type = NodeType::Program;
        self.process_statement_list(my_id, my_id);
        self.process_end_of_program();
    }

    fn process_end_of_program(&mut self) {
        if self.scanner.unmatched_multiline_comment_prefixes.is_empty() {
            self.match_token(TokenType::EndOfProgram);
        } else {
            for (line, col) in &self.scanner.unmatched_multiline_comment_prefixes {
                println!("Runaway multi line comment:");
                self.scanner.print_line(*line as usize, *col as usize);
            }
        }
    }

    fn process_statement_list(&mut self, parent: usize, previous: usize) {
        // Stop recursion if the next token is end of program
        // or 'end' keyword that ends the for loop.
        match self.scanner.peek().unwrap().token_type {
            TokenType::EndOfProgram | TokenType::KeywordEnd => {}
            _ => {
                let previous = self.process_statement(parent, previous);
                self.process_statement_list(parent, previous);
            }
        }
    }

    fn process_statement(&mut self, parent: usize, previous: usize) -> usize {
        let my_id = self.process_statement_prefix(parent, previous);
        self.match_token(TokenType::EndOfStatement);

        my_id
    }

    fn process_statement_prefix(&mut self, parent: usize, previous: usize) -> usize {
        match self.scanner.peek().unwrap().token_type {
            TokenType::KeywordVar => {
                // Declaration with an optional assignment
                let my_id = self.add_node(parent, previous);
                self.tree[my_id].node_type = NodeType::Declaration;
                self.match_token(TokenType::KeywordVar);

                // Add node for the identifier
                // Literals and identifiers use the same node type, Operand.
                // Which the node actually represents can be disambiguated by the type of the token.
                let previous = self.add_node(my_id, my_id);
                self.tree[previous].token = self.match_token(TokenType::Identifier);
                self.tree[previous].node_type = NodeType::Operand;

                self.match_token(TokenType::TypeSeparator);

                // Store the type as the token of the declaration node
                match self.scanner.peek().unwrap().token_type {
                    token_type @ TokenType::TypeInt
                    | token_type @ TokenType::TypeString
                    | token_type @ TokenType::TypeBool => {
                        self.tree[my_id].token = self.match_token(token_type);
                    }
                    _ => {
                        // A syntax error, let's pass something that we know it's not
                        self.tree[my_id].token = self.match_token(TokenType::TypeInt);
                    }
                }

                // The statement can contain an assignment. If it does, use the value the user
                // provided as the right sibling. Otherwise use some default.
                if TokenType::Assignment == self.scanner.peek().unwrap().token_type {
                    self.match_token(TokenType::Assignment);
                    self.process_expression(my_id, previous);
                } else {
                    let previous = self.add_node(my_id, previous);
                    self.tree[previous].node_type = NodeType::Operand;
                }

                return my_id;
            }
            TokenType::KeywordFor => {
                // For loop
                let my_id = self.add_node(parent, previous);
                self.tree[my_id].node_type = NodeType::For;
                self.tree[my_id].token = self.match_token(TokenType::KeywordFor);

                // Identifier is the first child of for loop
                let previous = self.add_node(my_id, my_id);
                self.tree[previous].node_type = NodeType::Operand;
                self.tree[previous].token = self.match_token(TokenType::Identifier);

                self.match_token(TokenType::KeywordIn);

                // Range is the second child of for, i.e. the sibling of identifier
                let range_id = self.add_node(my_id, previous);
                self.tree[range_id].node_type = NodeType::Range;

                // First child expression of range
                let previous = self.process_expression(range_id, range_id);

                self.tree[range_id].token = self.match_token(TokenType::Range);

                // Second child expression of range, i.e. sibling of first expression
                self.process_expression(range_id, previous);
                self.match_token(TokenType::KeywordDo);
                // Process however many statements there are inside the for block
                self.process_statement_list(my_id, range_id);
                self.match_token(TokenType::KeywordEnd);
                self.match_token(TokenType::KeywordFor);

                return my_id;
            }
            TokenType::KeywordRead => {
                // Read statement
                let my_id = self.add_node(parent, previous);
                self.tree[my_id].node_type = NodeType::Read;
                self.tree[my_id].token = self.match_token(TokenType::KeywordRead);

                let previous = self.add_node(my_id, my_id);
                self.tree[previous].node_type = NodeType::Operand;
                self.tree[previous].token = self.match_token(TokenType::Identifier);

                return my_id;
            }
            TokenType::KeywordPrint => {
                // Print statement
                let my_id = self.add_node(parent, previous);
                self.tree[my_id].node_type = NodeType::Print;
                self.tree[my_id].token = self.match_token(TokenType::KeywordPrint);

                self.process_expression(my_id, my_id);

                return my_id;
            }
            TokenType::KeywordAssert => {
                // Assertion statement
                let my_id = self.add_node(parent, previous);
                self.tree[my_id].node_type = NodeType::Assert;
                self.tree[my_id].token = self.match_token(TokenType::KeywordAssert);

                self.match_token(TokenType::LParen);
                self.process_expression(my_id, my_id);
                self.match_token(TokenType::RParen);

                return my_id;
            }
            TokenType::Identifier => {
                // Assignment to a variable
                let my_id = self.add_node(parent, previous);
                self.tree[my_id].node_type = NodeType::Assignment;

                let previous = self.add_node(my_id, my_id);
                self.tree[previous].node_type = NodeType::Operand;
                self.tree[previous].token = self.match_token(TokenType::Identifier);

                self.tree[my_id].token = self.match_token(TokenType::Assignment);

                self.process_expression(my_id, previous);

                return my_id;
            }
            _ => {
                println!("Invalid start of statement!");

                return !0;
            }
        }
    }

    fn process_expression(&mut self, parent: usize, previous: usize) -> usize {
        let my_id = self.add_node(parent, previous);
        self.tree[my_id].node_type = NodeType::Expression;

        if TokenType::OperatorNot == self.scanner.peek().unwrap().token_type {
            self.tree[my_id].token = self.match_token(TokenType::OperatorNot);
        }

        let previous = self.process_operand(my_id, my_id);

        match self.scanner.peek().unwrap().token_type {
            token_type @ TokenType::OperatorPlus
            | token_type @ TokenType::OperatorMinus
            | token_type @ TokenType::OperatorMultiply
            | token_type @ TokenType::OperatorDivide
            | token_type @ TokenType::OperatorLessThan
            | token_type @ TokenType::OperatorEqual
            | token_type @ TokenType::OperatorAnd => {
                self.tree[my_id].token = self.match_token(token_type);
                self.process_operand(my_id, previous);
            }
            _ => {}
        }

        my_id
    }

    fn process_operand(&mut self, parent: usize, previous: usize) -> usize {
        match self.scanner.peek().unwrap().token_type {
            token_type @ TokenType::LParen => {
                self.match_token(token_type);
                let my_id = self.process_expression(parent, previous);
                self.match_token(TokenType::RParen);

                return my_id;
            }
            token_type @ TokenType::LiteralInt
            | token_type @ TokenType::LiteralString
            | token_type @ TokenType::Identifier => {
                let my_id = self.add_node(parent, previous);
                self.tree[my_id].node_type = NodeType::Operand;
                self.tree[my_id].token = self.match_token(token_type);

                return my_id;
            }
            _ => {
                println!("Missing operand!");
                return !0;
            }
        }
    }
}

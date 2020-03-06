use super::lcrs_tree::LcRsTree;
use super::parser::{NodeData, NodeType};
use super::scanner::TokenType;
use std::collections::HashMap;
use std::fmt;

#[derive(Copy, Clone, Debug)]
enum SemanticError {
    MismatchedTypes,
    UndeclaredIdentifier,
    IllegalOperation,
    Undefined,
}

pub struct Analyzer<'a> {
    tree: LcRsTree<NodeData<'a>>,
    symbols: HashMap<&'a str, SymbolType>,
    errors: Vec<SemanticError>,
}

impl<'a> Analyzer<'a> {
    pub fn new(tree: LcRsTree<NodeData<'a>>) -> Self {
        Analyzer {
            tree: tree,
            symbols: HashMap::new(),
            errors: vec![],
        }
    }

    pub fn analyze(&mut self) -> bool {
        // Start walking down the tree
        assert!(self.tree.len() > 0);
        let mut node = self.tree[0].left_child;
        while let Some(idx) = node {
            self.handle(idx);
            node = self.tree[idx].right_sibling;
        }

        self.print_errors()
    }

    fn print_errors(&self) -> bool {
        for err in self.errors.iter() {
            match err {
                SemanticError::MismatchedTypes => println!("Mismatched types."),
                SemanticError::UndeclaredIdentifier => println!("Undeclared identifier."),
                SemanticError::IllegalOperation => println!("Illegal operation."),
                SemanticError::Undefined => println!("Undefined semantic error."),
            }
        }
        self.errors.is_empty()
    }

    fn handle(&mut self, idx: usize) {
        match self.tree[idx]
            .data
            .node_type
            .expect("AST should not contain Nodes with type None.")
        {
            NodeType::Declaration => self.handle_declaration(idx),
            NodeType::Assignment => self.handle_assignment(idx),
            NodeType::For => self.handle_for(idx),
            NodeType::Read => self.handle_read(idx),
            NodeType::Print => self.handle_print(idx),
            NodeType::Assert => self.handle_assert(idx),
            _ => {
                assert!(false, "Unhandled node type.");
                ()
            }
        };
    }

    fn get_operand_type(&mut self, idx: usize) -> SymbolType {
        let token = &self.tree[idx]
            .data
            .token
            .expect("Operand is missing a token.");
        match token.token_type {
            TokenType::LiteralInt => SymbolType::Int,
            TokenType::LiteralString => SymbolType::String,
            TokenType::Identifier => {
                if let Some(symbol) = self.symbols.get(token.value) {
                    *symbol
                } else {
                    self.errors.push(SemanticError::UndeclaredIdentifier);
                    SymbolType::Undefined
                }
            }
            _ => {
                assert!(false, "Operand has an illegal token type.");
                SymbolType::Undefined
            }
        }
    }

    fn get_expression_type(&mut self, idx: usize) -> SymbolType {
        if NodeType::Operand
            == self.tree[idx]
                .data
                .node_type
                .expect("No node type on expression.")
        {
            self.get_operand_type(idx)
        } else {
            let lc = self.tree[idx]
                .left_child
                .expect("Expression is missing a child node.");
            let expr_type = self.get_expression_type(lc);
            match self.tree[idx]
                .data
                .token
                .expect("Expression is missing a token.")
                .token_type
            {
                tt @ TokenType::OperatorNot => {
                    if SymbolType::Bool == expr_type {
                        SymbolType::Bool
                    } else {
                        self.errors.push(SemanticError::MismatchedTypes);
                        SymbolType::Undefined
                    }
                }
                tt @ TokenType::OperatorAnd
                | tt @ TokenType::OperatorPlus
                | tt @ TokenType::OperatorMinus
                | tt @ TokenType::OperatorMultiply
                | tt @ TokenType::OperatorDivide
                | tt @ TokenType::OperatorLessThan
                | tt @ TokenType::OperatorEqual => {
                    let second_expr_type = self.get_expression_type(
                        self.tree[lc]
                            .right_sibling
                            .expect("Expression is missing its second child."),
                    );

                    if expr_type == second_expr_type {
                        match tt {
                            tt @ TokenType::OperatorAnd => {
                                if SymbolType::Bool == expr_type {
                                    expr_type
                                } else {
                                    self.errors.push(SemanticError::MismatchedTypes);
                                    SymbolType::Undefined
                                }
                            }
                            tt @ TokenType::OperatorPlus => {
                                if SymbolType::String == expr_type || SymbolType::Int == expr_type {
                                    expr_type
                                } else {
                                    self.errors.push(SemanticError::MismatchedTypes);
                                    SymbolType::Undefined
                                }
                            }
                            tt @ TokenType::OperatorMinus => {
                                if SymbolType::Int == expr_type {
                                    expr_type
                                } else {
                                    self.errors.push(SemanticError::MismatchedTypes);
                                    SymbolType::Undefined
                                }
                            }
                            tt @ TokenType::OperatorMultiply => {
                                if SymbolType::Int == expr_type {
                                    expr_type
                                } else {
                                    self.errors.push(SemanticError::MismatchedTypes);
                                    SymbolType::Undefined
                                }
                            }
                            tt @ TokenType::OperatorDivide => {
                                if SymbolType::Int == expr_type {
                                    expr_type
                                } else {
                                    self.errors.push(SemanticError::MismatchedTypes);
                                    SymbolType::Undefined
                                }
                            }
                            TokenType::OperatorLessThan => expr_type,
                            TokenType::OperatorEqual => expr_type,
                            _ => {
                                assert!(false, "Expression token has an illegal type.");
                                SymbolType::Undefined
                            }
                        }
                    } else {
                        self.errors.push(SemanticError::MismatchedTypes);
                        SymbolType::Undefined
                    }
                }
                _ => {
                    assert!(false, "Expression token has an illegal type.");
                    SymbolType::Undefined
                }
            }
        }
    }

    fn handle_declaration(&mut self, idx: usize) {
        println!("Declaration {:#?}", self.tree[idx].data);
    }

    fn handle_assignment(&mut self, idx: usize) {
        let node = &self.tree[idx];
        let identifier = &self.tree[node
            .left_child
            .expect("Assignment is missing an identifier in AST.")];
        let id_token = identifier
            .data
            .token
            .expect("Identifier is missing a token.");
        if let Some(symbol) = self.symbols.get(id_token.value) {
            let symbol = *symbol;
            let rs = identifier
                .right_sibling
                .expect("Assignment should contain an expression.");
            let et = self.get_expression_type(rs);
            if et != symbol {
                self.errors.push(SemanticError::MismatchedTypes);
            }
        } else {
            self.errors.push(SemanticError::UndeclaredIdentifier);
        }
    }

    fn handle_for(&mut self, idx: usize) {
        println!("For {:#?}", self.tree[idx].data);
    }

    fn handle_read(&mut self, idx: usize) {
        println!("Read {:#?}", self.tree[idx].data);
    }

    fn handle_print(&mut self, idx: usize) {
        println!("Print {:#?}", self.tree[idx].data);
    }

    fn handle_assert(&mut self, idx: usize) {
        println!("Assertion {:#?}", self.tree[idx].data);
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum SymbolType {
    Int,
    String,
    Bool,
    Undefined,
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolType::Int => write!(f, "int"),
            SymbolType::String => write!(f, "string"),
            SymbolType::Bool => write!(f, "bool"),
            SymbolType::Undefined => write!(f, "undefined"),
        }
    }
}
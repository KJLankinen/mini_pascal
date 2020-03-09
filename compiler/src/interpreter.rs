use super::data_types::{ErrorType, NodeData, NodeType, SymbolType};
use super::lcrs_tree::LcRsTree;
use super::logger::Logger;
use std::collections::HashMap;

pub struct Interpreter<'a, 'b> {
    logger: &'b mut Logger<'a>,
    tree: &'b LcRsTree<NodeData<'a>>,
    symbols: HashMap<&'a str, (SymbolType, &'a str)>,
}

impl<'a, 'b> Interpreter<'a, 'b> {
    // ---------------------------------------------------------------------
    // Called to start the interpretation
    // ---------------------------------------------------------------------
    pub fn interpret(&mut self) {
        // Start walking down the tree
        let mut node = self.tree[0].left_child;

        while let Some(idx) = node {
            self.interpret_statement(idx);
            node = self.tree[idx].right_sibling;
        }
    }

    // ---------------------------------------------------------------------
    // Functions that interpret the program
    // ---------------------------------------------------------------------
    fn interpret_statement(&mut self, idx: usize) {
        match self.tree[idx]
            .data
            .node_type
            .expect("AST should not contain Nodes with type None.")
        {
            NodeType::Declaration => self.interpret_declaration(idx),
            NodeType::Assignment => self.interpret_assignment(idx),
            NodeType::For => self.interpret_for(idx),
            NodeType::Read => self.interpret_read(idx),
            NodeType::Print => self.interpret_print(idx),
            NodeType::Assert => self.interpret_assert(idx),
            _ => {
                assert!(false, "Unhandled node type.");
                ()
            }
        };
    }

    fn evaluate_expression<T>(&mut self, idx: usize) -> T
    where
        T: Default,
    {
        T::default()
    }

    fn interpret_declaration(&mut self, idx: usize) {}

    fn interpret_assignment(&mut self, idx: usize) {}

    fn interpret_for(&mut self, idx: usize) {}

    fn interpret_read(&mut self, idx: usize) {}

    fn interpret_print(&mut self, idx: usize) {}

    fn interpret_assert(&mut self, idx: usize) {}

    pub fn new(tree: &'b LcRsTree<NodeData<'a>>, logger: &'b mut Logger<'a>) -> Self {
        Interpreter {
            logger: logger,
            tree: tree,
            symbols: HashMap::new(),
        }
    }
}

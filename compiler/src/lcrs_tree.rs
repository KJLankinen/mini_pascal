use serde::Serialize;
use serde_json::json;

pub trait Update {
    fn update(&mut self, data: Self);
}

#[derive(Debug, Clone)]
struct Node<T>
where
    T: Default,
    T: Update,
    T: Serialize,
{
    parent: Option<usize>,
    left_child: Option<usize>,
    right_sibling: Option<usize>,
    data: T,
}

impl<T> Default for Node<T>
where
    T: Default,
    T: Update,
    T: Serialize,
{
    fn default() -> Self {
        Node {
            parent: None,
            left_child: None,
            right_sibling: None,
            data: T::default(),
        }
    }
}

#[derive(Serialize)]
struct JsonNode<'a, T> {
    data: &'a T,
    children: Option<serde_json::Value>,
}

pub struct LcRsTree<T>
where
    T: Default,
    T: Update,
    T: Serialize,
{
    nodes: Vec<Node<T>>,
}

impl<T> LcRsTree<T>
where
    T: Default,
    T: Update,
    T: Serialize,
{
    pub fn new() -> Self {
        LcRsTree { nodes: vec![] }
    }

    pub fn add_child(&mut self, parent: Option<usize>) -> Option<usize> {
        let my_id = self.new_node();
        self.nodes[my_id].parent = parent;

        match parent {
            Some(id) => {
                // Parent exists, this is the normal case
                match self.nodes[id].left_child {
                    Some(id) => {
                        // Parent already has at least one child
                        let mut prev = id;
                        while let Some(id) = self.nodes[prev].right_sibling {
                            prev = id;
                        }
                        // prev is my youngest older sibling
                        self.nodes[prev].right_sibling = Some(my_id);
                    }
                    None => {
                        // Parent is childless, I am first child, id is parent's id
                        self.nodes[id].left_child = Some(my_id);
                    }
                }
            }
            None => {
                // Parent is None, which should only be the case when the first node is added.
                assert!(0 == my_id, "Parent of a new node cannot be None.");
            }
        }

        Some(my_id)
    }

    pub fn update_data(&mut self, id: Option<usize>, data: T) {
        if let Some(id) = id {
            self.nodes[id].data = data;
        }
    }

    fn new_node(&mut self) -> usize {
        let id = self.nodes.len();
        self.nodes.push(Node::default());

        id
    }

    pub fn serialize<'a>(&'a mut self) -> Option<serde_json::Value> {
        if self.nodes.is_empty() {
            return None;
        } else {
            return self.recursive_serialize(Some(0));
        }
    }

    fn recursive_serialize(&self, id: Option<usize>) -> Option<serde_json::Value> {
        if let Some(my_id) = id {
            let mut my_children = vec![];
            if let Some(id) = self.nodes[my_id].left_child {
                // Add first child
                my_children.push(self.recursive_serialize(Some(id)));

                // Add the rest of the children
                let mut next = id;
                while let Some(id) = self.nodes[next].right_sibling {
                    my_children.push(self.recursive_serialize(Some(id)));
                    next = id;
                }
            }

            if 1 < my_children.len() {
                return Some(json!(JsonNode {
                    data: &self.nodes[my_id].data,
                    children: Some(json!(my_children))
                }));
            } else if 1 == my_children.len() {
                return Some(json!(JsonNode {
                    data: &self.nodes[my_id].data,
                    children: Some(json!(my_children[0]))
                }));
            } else {
                return Some(json!(&self.nodes[my_id].data));
            }
        }

        return None;
    }
}

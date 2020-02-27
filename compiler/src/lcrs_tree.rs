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
    unused_ids: Vec<usize>,
}

impl<T> LcRsTree<T>
where
    T: Default,
    T: Update,
    T: Serialize,
{
    pub fn new() -> Self {
        LcRsTree {
            nodes: vec![],
            unused_ids: vec![],
        }
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
        if let Some(id) = self.unused_ids.pop() {
            id
        } else {
            let id = self.nodes.len();
            self.nodes.push(Node::default());

            id
        }
    }

    pub fn count_children(&self, id: Option<usize>) -> usize {
        let mut num_children = 0;
        if let Some(id) = id {
            if id < self.nodes.len() {
                if let Some(id) = self.nodes[id].left_child {
                    num_children += 1;

                    let mut next = id;
                    while let Some(id) = self.nodes[next].right_sibling {
                        num_children += 1;
                        next = id;
                    }
                }
            }
        }

        return num_children;
    }

    pub fn remove_me(&mut self, id: Option<usize>) {
        let promote_children = |my_id: usize, tree: &mut Self| {
            // This closure changes the children. Parent of children is set to be the parent
            // of the given id, and the siblings of the given id are set to be the younger siblings of
            // the children.
            let my_child = tree.nodes[my_id].left_child.unwrap();
            tree.nodes[my_child].parent = tree.nodes[my_id].parent;

            let mut next = my_child;
            while let Some(my_child) = tree.nodes[next].right_sibling {
                tree.nodes[my_child].parent = tree.nodes[my_id].parent;
                next = my_child;
            }

            tree.nodes[next].right_sibling = tree.nodes[my_id].right_sibling;
        };

        if let Some(id) = id {
            if id < self.nodes.len() {
                if let Some(parent) = self.nodes[id].parent {
                    if let Some(next_child) = self.nodes[parent].left_child {
                        if next_child == id {
                            // I am first child
                            if self.nodes[id].left_child.is_some() {
                                // I have children
                                self.nodes[parent].left_child = self.nodes[id].left_child;
                                promote_children(id, self);
                            } else {
                                // I have no children, so make my next sibling be first child
                                self.nodes[parent].left_child = self.nodes[id].right_sibling;
                            }
                        } else {
                            // I am not first child
                            let mut prev = next_child;
                            while let Some(next_child) = self.nodes[prev].right_sibling {
                                // If I am the next sibling of prev, stop.
                                if id == next_child {
                                    break;
                                }
                                prev = next_child;
                            }

                            if self.nodes[id].left_child.is_some() {
                                // I have children, make my first child be the next sibling of my
                                // youngest older sibling and promote my children.
                                self.nodes[prev].right_sibling = self.nodes[id].left_child;
                                promote_children(id, self);
                            } else {
                                // I have no children, so make my older sibling point to my younger
                                // sibling
                                self.nodes[prev].right_sibling = self.nodes[id].right_sibling;
                            }
                        }

                        // I am done
                        self.nodes[id] = Node::default();
                        self.unused_ids.push(id);
                    }
                }
            }
        }
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

            let mut obj = json!(&self.nodes[my_id].data);
            match my_children.len() {
                0 => {}
                1 => {
                    let child = my_children[0].as_ref().unwrap();
                    obj.as_object_mut()
                        .unwrap()
                        .insert("child".to_string(), json!(child));
                }
                _ => {
                    obj.as_object_mut()
                        .unwrap()
                        .insert("children".to_string(), json!(my_children));
                }
            }

            return Some(json!(obj));
        }

        return None;
    }
}

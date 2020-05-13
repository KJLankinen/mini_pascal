use super::data_types::SymbolType;
use std::collections::HashMap;

pub struct SymbolTable<'a> {
    depth: usize,
    symbols_in_scope: HashMap<usize, HashMap<&'a str, SymbolType>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        SymbolTable {
            depth: 0,
            symbols_in_scope: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: &'a str, st: SymbolType) {
        self.symbols_in_scope
            .entry(self.depth)
            .or_insert(HashMap::new())
            .insert(key, st);
    }

    pub fn get(&self, key: &'a str) -> Option<&SymbolType> {
        self.symbols_in_scope
            .get(&self.depth)
            .and_then(|m| m.get(key))
    }

    pub fn find(&self, key: &'a str) -> Option<&SymbolType> {
        for i in (0..=self.depth).rev() {
            if let Some(st) = self.symbols_in_scope.get(&i).and_then(|m| m.get(key)) {
                return Some(st);
            }
        }
        None
    }

    pub fn step_in(&mut self) {
        self.depth += 1;
    }

    pub fn step_out(&mut self) {
        if let Some(hm) = self.symbols_in_scope.get_mut(&self.depth) {
            hm.clear();
        }
        assert!(
            self.depth > 0,
            "SymbolTable depth should never be 0 before stepping out."
        );
        self.depth -= 1;
    }
}

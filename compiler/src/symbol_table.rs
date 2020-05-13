use super::data_types::SymbolType;
use std::collections::HashMap;

pub struct SymbolTable<'a> {
    depth: usize,
    pub symbols: HashMap<usize, HashMap<&'a str, SymbolType>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        SymbolTable {
            depth: 0,
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: &'a str, st: SymbolType) {
        self.symbols
            .get_mut(&self.depth)
            .expect("SymbolTable should have a map at current depth.")
            .insert(key, st);
    }

    pub fn get(&self, key: &'a str) -> Option<&SymbolType> {
        self.symbols
            .get(&self.depth)
            .expect("SymbolTable should have a map at current depth.")
            .get(key)
    }

    pub fn find(&self, key: &'a str) -> Option<&SymbolType> {
        for i in (0..=self.depth).rev() {
            if let Some(hm) = self.symbols.get(&i) {
                if let Some(st) = hm.get(key) {
                    return Some(st);
                }
            }
        }
        None
    }

    pub fn step_in(&mut self) {
        self.depth += 1;
        if self.symbols.get(&self.depth).is_none() {
            self.symbols.insert(self.depth, HashMap::new());
        }
    }

    pub fn step_out(&mut self) {
        if let Some(hm) = self.symbols.get_mut(&self.depth) {
            hm.clear();
        }
        assert!(
            self.depth > 0,
            "SymbolTable depth should never be 0 before stepping out."
        );
        self.depth -= 1;
    }
}

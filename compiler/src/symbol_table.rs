use super::data_types::SymbolType;
use std::collections::HashMap;

pub struct SymbolTable<'a> {
    depth: i32,
    symbols_in_scope: HashMap<i32, HashMap<String, SymbolType>>,
    variable_counts: HashMap<&'a str, HashMap<SymbolType, Vec<usize>>>,
    maximum_counts: HashMap<SymbolType, Vec<usize>>,
    current_fname: Option<&'a str>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        SymbolTable {
            depth: -1,
            symbols_in_scope: HashMap::new(),
            variable_counts: HashMap::new(),
            maximum_counts: HashMap::new(),
            current_fname: None,
        }
    }

    pub fn insert(&mut self, key: &'a str, st: SymbolType) {
        assert!(
            -1 < self.depth,
            "Scope depth must be positive, use step_in() before insert."
        );
        assert!(
            self.current_fname.is_some(),
            "Current function name must be set before inserting values."
        );

        self.symbols_in_scope
            .entry(self.depth)
            .or_insert(HashMap::new())
            .insert(key.to_lowercase(), st);

        // Increment the number of this SymbolType encountered
        // inside the current function at the current depth
        self.variable_counts
            .entry(self.current_fname.unwrap())
            .or_insert(HashMap::new())
            .entry(st)
            .or_insert(vec![0; self.depth as usize + 1])[self.depth as usize] += 1;
    }

    pub fn get(&self, key: &'a str) -> Option<&SymbolType> {
        self.symbols_in_scope
            .get(&self.depth)
            .and_then(|m| m.get(&key.to_lowercase()))
    }

    pub fn find(&self, key: &'a str) -> Option<&SymbolType> {
        for i in (0..=self.depth).rev() {
            if let Some(st) = self
                .symbols_in_scope
                .get(&i)
                .and_then(|m| m.get(&key.to_lowercase()))
            {
                return Some(st);
            }
        }
        None
    }

    pub fn step_in(&mut self, new_fname: Option<&'a str>) {
        self.depth += 1;

        if new_fname.is_some() {
            self.current_fname = new_fname;
        }

        // For every SymbolType that has been encountered inside the current function,
        // add a new count of zero at this new depth.
        self.variable_counts
            .get_mut(self.current_fname.unwrap_or_else(|| ""))
            .map(|m| m.values_mut().for_each(|v| v.push(0)));
    }

    pub fn step_out(&mut self) {
        assert!(
            -1 < self.depth,
            "SymbolTable depth should never be < 0 before stepping out."
        );

        if let Some(hm) = self.symbols_in_scope.get_mut(&self.depth) {
            hm.clear();
        }

        // Update the maximum counts for each symbol type encountered inside the scope we're
        // stepping out of.
        if let Some(fname) = self.current_fname {
            if let Some(map) = self.variable_counts.get_mut(fname) {
                for (st, vec) in map.iter_mut() {
                    if 0 < vec.len() {
                        let max_vec = self.maximum_counts.entry(*st).or_insert(vec![0; vec.len()]);
                        while max_vec.len() < vec.len() {
                            max_vec.push(vec[max_vec.len()]);
                        }
                        assert!(
                            max_vec.len() >= vec.len(),
                            "max vec should be same length or longer as current vec."
                        );

                        let idx = vec.len() - 1;
                        max_vec[idx] = *[max_vec[idx], vec.pop().unwrap()].iter().max().unwrap();
                    }
                }
            }
        }

        self.depth -= 1;
        if -1 == self.depth {
            // We're stepping completely out of a function.
            // Save the maximum counts of the function
            if let Some(fname) = self.current_fname {
                if let Some(map) = self.variable_counts.get_mut(fname) {
                    for (st, vec) in map.iter_mut() {
                        *vec = self
                            .maximum_counts
                            .get(st)
                            .expect(
                                "maximum_count and variable_count should contain the same keys.",
                            )
                            .to_vec();
                    }
                }
            }
            self.maximum_counts.clear();
            self.current_fname = None;
        }
    }
}

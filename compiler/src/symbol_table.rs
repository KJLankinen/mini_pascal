use super::data_types::{FunctionSignature, SymbolType, EMPTY_TOKEN};
use std::collections::HashMap;

pub struct SymbolTable<'a> {
    depth: i32,
    symbols_in_scope: HashMap<i32, HashMap<String, (SymbolType, usize)>>,
    variable_counts: HashMap<SymbolType, Vec<usize>>,
    maximum_counts: HashMap<SymbolType, Vec<usize>>,
    function_data: HashMap<&'a str, FunctionData<'a>>,
    current_fname: Option<&'a str>,
}

#[derive(Debug)]
pub struct FunctionData<'a> {
    pub variable_indices: HashMap<(SymbolType, usize), usize>,
    pub variable_counts: HashMap<SymbolType, usize>,
    pub signature: FunctionSignature<'a>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        SymbolTable {
            depth: -1,
            symbols_in_scope: HashMap::new(),
            variable_counts: HashMap::new(),
            maximum_counts: HashMap::new(),
            function_data: HashMap::new(),
            current_fname: None,
        }
    }

    pub fn insert_function_signature(&mut self, key: &'a str, fs: FunctionSignature<'a>) {
        self.function_data
            .entry(key)
            .or_insert(FunctionData {
                variable_indices: HashMap::new(),
                variable_counts: HashMap::new(),
                signature: FunctionSignature {
                    parameters: vec![],
                    return_type: None,
                    token: EMPTY_TOKEN,
                },
            })
            .signature = fs;
    }

    pub fn get_function_signature(&self, key: &'a str) -> Option<&FunctionSignature<'a>> {
        self.function_data.get(key).map(|fd| &fd.signature)
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

        let count = self
            .variable_counts
            .entry(st)
            .or_insert(vec![0; self.depth as usize + 1])
            .get_mut(self.depth as usize)
            .unwrap();
        *count += 1;

        self.symbols_in_scope
            .entry(self.depth)
            .or_insert(HashMap::new())
            .insert(key.to_lowercase(), (st, *count));
    }

    pub fn get(&self, key: &'a str) -> Option<&(SymbolType, usize)> {
        self.symbols_in_scope
            .get(&self.depth)
            .and_then(|m| m.get(&key.to_lowercase()))
    }

    pub fn find(&self, key: &'a str) -> Option<&(SymbolType, usize)> {
        for i in (0..=self.depth).rev() {
            if let Some(tuple) = self
                .symbols_in_scope
                .get(&i)
                .and_then(|m| m.get(&key.to_lowercase()))
            {
                return Some(tuple);
            }
        }
        None
    }

    pub fn step_in(&mut self, new_fname: Option<&'a str>) {
        self.depth += 1;

        if new_fname.is_some() {
            self.current_fname = new_fname;
            // Add parameters of this function to scope
            for param in self
                .function_data
                .get(new_fname.unwrap())
                .expect("Every function should have a signature at this point.")
                .signature
                .parameters
                .to_vec()
            {
                self.insert(param.id, param.symbol_type);
            }
        }

        self.variable_counts.values_mut().for_each(|v| v.push(0));
    }

    pub fn step_out(&mut self) {
        assert!(
            -1 < self.depth,
            "SymbolTable depth should never be < 0 before stepping out."
        );

        self.symbols_in_scope
            .get_mut(&self.depth)
            .map(|m| m.clear());

        // Update the maximum counts for each symbol type encountered inside the scope we're
        // stepping out of.
        for (st, vec) in self.variable_counts.iter_mut() {
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

        self.depth -= 1;
        if -1 == self.depth {
            // We're stepping completely out of a function.
            // Calculate the indices of variables for the function to a map.
            if let Some(fname) = self.current_fname {
                /*
                 * Example:
                 * let vec =        [0,         0,          1,              0,          3,              2];
                 * Iterators below will then calculate
                 * first(scan)      [0,         0,          0,              1,          1,              4],
                 * then(zip & map)  [None,      None,       Some(0),        None,       Some(1),        Some(4)],
                 * finally(enum)    [(0, None), (1, None),  (2, Some(0)),   (3, None),  (4, Some(1)),   (5, Some(4))]
                 *
                 * In the final vector, the first value of the tuple is the depth,
                 * the second is the index if there was any variables
                 * of the given symbol type at this scope depth.
                 */
                let mut total = 0;
                let fd = self
                    .function_data
                    .get_mut(fname)
                    .expect("Every function should have its data set already.");
                for (st, vec) in self.maximum_counts.iter() {
                    for (i, v) in vec
                        .iter()
                        .scan(0, |state, &x| {
                            let t = *state;
                            *state = *state + x;
                            Some(t)
                        })
                        .zip(vec)
                        .map(|(cumsum, n)| if *n != 0 { Some(cumsum) } else { None })
                        .enumerate()
                    {
                        if let Some(starting_idx) = v {
                            fd.variable_indices.insert((*st, i), starting_idx + total);
                        }
                    }
                    let sum = vec.iter().sum::<usize>();
                    fd.variable_counts.insert(*st, sum);
                    total += sum;
                }
            }

            self.variable_counts.clear();
            self.maximum_counts.clear();
            self.current_fname = None;
        }
    }

    pub fn _get_variable_index(
        &self,
        fname: &'a str,
        st: SymbolType,
        depth: usize,
    ) -> Option<&usize> {
        self.function_data
            .get(fname)
            .and_then(|fd| fd.variable_indices.get(&(st, depth)))
    }
}
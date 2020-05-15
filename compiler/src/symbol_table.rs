use super::data_types::{FunctionSignature, SymbolType, EMPTY_TOKEN};
use std::collections::HashMap;

pub struct SymbolTable<'a> {
    depth: i32,
    symbols_in_scope: HashMap<i32, HashMap<String, (SymbolType, usize, i32)>>,
    counts: Counts,
    function_data: HashMap<&'a str, FunctionData<'a>>,
    current_fname: Option<&'a str>,
    string_literals: String,
    string_literal_bytes: Vec<(usize, usize)>,
}

impl<'a> SymbolTable<'a> {
    pub fn insert_function_signature(&mut self, key: &'a str, fs: FunctionSignature<'a>) {
        self.function_data
            .entry(key)
            .or_insert(FunctionData {
                i32_idx: HashMap::new(),
                f32_idx: HashMap::new(),
                it: 0,
                ft: 0,
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

        // Reals are represented as f32 in webassembly, everything else as i32
        let count = match st {
            SymbolType::Real => self.counts.get_fc(self.depth as usize),
            SymbolType::Bool
            | SymbolType::Int
            | SymbolType::String
            | SymbolType::ArrayBool(_)
            | SymbolType::ArrayInt(_)
            | SymbolType::ArrayReal(_)
            | SymbolType::ArrayString(_) => self.counts.get_ic(self.depth as usize),
            SymbolType::Undefined => {
                assert!(false, "Trying to insert a variable of undefined type.");
                self.counts.get_ic(self.depth as usize)
            }
        };

        self.symbols_in_scope
            .entry(self.depth)
            .or_insert(HashMap::new())
            .insert(key.to_lowercase(), (st, *count, self.depth));

        *count += 1;
    }

    pub fn get(&self, key: &'a str) -> Option<&(SymbolType, usize, i32)> {
        self.symbols_in_scope
            .get(&self.depth)
            .and_then(|m| m.get(&key.to_lowercase()))
    }

    pub fn find(&self, key: &'a str) -> Option<&(SymbolType, usize, i32)> {
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

        self.counts.push();
    }

    pub fn step_out(&mut self) {
        assert!(
            -1 < self.depth,
            "SymbolTable depth should never be < 0 before stepping out."
        );

        self.symbols_in_scope
            .get_mut(&self.depth)
            .map(|m| m.clear());

        self.counts.pop();

        self.depth -= 1;
        if -1 == self.depth {
            // We're stepping completely out of a function.
            // Updata function data with the count of local variables
            if let Some(fname) = self.current_fname {
                let fd = self
                    .function_data
                    .get_mut(fname)
                    .expect("Every function should have its data set already.");

                let update_function_data =
                    |map: &mut HashMap<i32, usize>,
                     total: &mut usize,
                     cs: &Vec<(i32, Option<usize>)>,
                     t: usize,
                     offset: usize| {
                        for (i, v) in cs {
                            if let Some(idx) = v {
                                map.insert(*i, idx + offset);
                            }
                        }
                        *total = t;
                    };

                let ((ics, it), (fcs, ft)) = self.counts.calculate_indices();
                update_function_data(&mut fd.i32_idx, &mut fd.it, &ics, it, 0);
                update_function_data(&mut fd.f32_idx, &mut fd.ft, &fcs, ft, it);
            }

            self.current_fname = None;
        }
    }

    pub fn get_variable_index(&self, fname: &'a str, st: SymbolType, depth: i32) -> Option<&usize> {
        self.function_data.get(fname).and_then(|fd| match st {
            SymbolType::Real => fd.f32_idx.get(&depth),
            SymbolType::Bool
            | SymbolType::Int
            | SymbolType::String
            | SymbolType::ArrayBool(_)
            | SymbolType::ArrayInt(_)
            | SymbolType::ArrayReal(_)
            | SymbolType::ArrayString(_) => fd.i32_idx.get(&depth),
            SymbolType::Undefined => {
                assert!(false, "Trying to get a variable of undefined type.");
                fd.i32_idx.get(&depth)
            }
        })
    }

    pub fn add_string_literal(&mut self, literal: &'a str) -> usize {
        // String literals are concatenated into a one big blob.
        // "start" and "length" are in bytes and relative to the start of the string blob.
        // The data will be stored in linear memory and can then be accesses with
        // a helper function that takes in the idx returned by this function.
        let start = self.string_literals.as_bytes().len();
        self.string_literals.push_str(literal);
        let length = self.string_literals.as_bytes().len() - start;
        self.string_literal_bytes.push((start, length));
        self.string_literal_bytes.len() - 1
    }

    pub fn _print_string_literals(&self) {
        // Debug function
        println!("{}, {:#?}", self.string_literals, self.string_literal_bytes);
    }

    pub fn _print_string_literal(&mut self, idx: usize) {
        // Debug function
        assert!(idx < self.string_literal_bytes.len());
        let (start, len) = self.string_literal_bytes[idx];
        let mut literal = self.string_literals.split_off(start);
        let end = literal.split_off(len);
        println!("{}, {}, {}", self.string_literals, literal, end);
        self.string_literals.push_str(literal.as_str());
        self.string_literals.push_str(end.as_str());
    }

    pub fn new() -> Self {
        SymbolTable {
            depth: -1,
            symbols_in_scope: HashMap::new(),
            counts: Counts::new(),
            function_data: HashMap::new(),
            current_fname: None,
            string_literals: String::new(),
            string_literal_bytes: vec![],
        }
    }
}

// ------------------------------------------------------------------------------------------------
// "Counts" is used to keep track of how many i32 representable and how many f32 representable
// variables are encountered as a function (or the main block) is traversed.
// Example:
// The maximum number of reals encountered (so far) in all the scopes of depth 3 is fm[3].
// ------------------------------------------------------------------------------------------------
struct Counts {
    ic: Vec<usize>, // current i32 counts
    im: Vec<usize>, // max i32 counts
    fc: Vec<usize>, // current f32 counts
    fm: Vec<usize>, // max f32 counts
}

impl Counts {
    pub fn new() -> Self {
        Counts {
            ic: vec![],
            im: vec![],
            fc: vec![],
            fm: vec![],
        }
    }

    pub fn get_fc(&mut self, idx: usize) -> &mut usize {
        self.fc.get_mut(idx).unwrap()
    }

    pub fn get_ic(&mut self, idx: usize) -> &mut usize {
        self.ic.get_mut(idx).unwrap()
    }

    pub fn push(&mut self) {
        self.ic.push(0);
        self.fc.push(0);
    }

    pub fn pop(&mut self) {
        let update_maximums = |counts: &mut Vec<usize>, maxs: &mut Vec<usize>| {
            assert!(
                !counts.is_empty(),
                "Count vector should always have at least one element."
            );
            while maxs.len() < counts.len() {
                maxs.push(counts[maxs.len()]);
            }
            assert!(
                maxs.len() >= counts.len(),
                "max vec should be same length or longer as count vec."
            );

            let idx = counts.len() - 1;
            maxs[idx] = *[maxs[idx], counts.pop().unwrap()].iter().max().unwrap();
        };

        update_maximums(&mut self.ic, &mut self.im);
        update_maximums(&mut self.fc, &mut self.fm);
    }

    fn calculate_cumsum(&self, vec: &Vec<usize>) -> Vec<(i32, Option<usize>)> {
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
        vec.iter()
            .scan(0, |state, &x| {
                let t = *state;
                *state = *state + x;
                Some(t)
            })
            .zip(vec)
            .map(|(cumsum, n)| if *n != 0 { Some(cumsum) } else { None })
            .enumerate()
            .map(|(i, v)| (i as i32, v))
            .collect()
    }

    pub fn calculate_indices(
        &mut self,
    ) -> (
        (Vec<(i32, Option<usize>)>, usize),
        (Vec<(i32, Option<usize>)>, usize),
    ) {
        (
            (self.calculate_cumsum(&self.im), self.im.iter().sum()),
            (self.calculate_cumsum(&self.fm), self.fm.iter().sum()),
        )
    }
}

// ------------------------------------------------------------------------------------------------
// "FunctionData" contains data related to a function.
// The idx maps contain the starting indices for local variables at different scope depths. The scope
// depth is the key and the value is the starting index. All other variables are represented as i32
// but reals are represented as f32. The local variables are recycled per depth, i.e. different
// scopes with the same depth use the same local variables.
// Total number of each kind of local variable is saved as well, and the function signature is
// saved in the struct of the same name.
// ------------------------------------------------------------------------------------------------
#[derive(Debug)]
pub struct FunctionData<'a> {
    pub i32_idx: HashMap<i32, usize>,
    pub f32_idx: HashMap<i32, usize>,
    pub it: usize,
    pub ft: usize,
    pub signature: FunctionSignature<'a>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counts_cumsum() {
        let counts = Counts::new();
        assert_eq!(
            vec![
                (0, None),
                (1, None),
                (2, Some(0)),
                (3, None),
                (4, Some(1)),
                (5, Some(4))
            ],
            counts.calculate_cumsum(&vec![0, 0, 1, 0, 3, 2])
        );
    }

    #[test]
    fn counts_push() {
        let mut counts = Counts::new();
        counts.push();
        assert_eq!(counts.ic.pop(), Some(0));
        assert_eq!(counts.fc.pop(), Some(0));
    }

    #[test]
    fn counts_pop() {
        let mut counts = Counts::new();
        counts.ic = vec![0, 5, 4, 8];
        counts.fc = vec![11, 2, 0, 40];
        counts.pop();
        counts.pop();
        counts.pop();
        counts.pop();

        assert_eq!(counts.im, vec![0, 5, 4, 8]);
        assert_eq!(counts.fm, vec![11, 2, 0, 40]);

        assert_eq!(counts.ic.pop(), None);
        assert_eq!(counts.fc.pop(), None);

        counts.ic = vec![6, 1, 18, 0];
        counts.fc = vec![3, 5, 8, 14];

        counts.pop();
        counts.pop();
        counts.pop();
        counts.pop();

        assert_eq!(counts.im, vec![6, 5, 18, 8]);
        assert_eq!(counts.fm, vec![11, 5, 8, 40]);
    }
}

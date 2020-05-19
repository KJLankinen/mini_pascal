use super::data_types::{FunctionSignature, SymbolType, EMPTY_TOKEN};
use std::collections::{HashMap, HashSet};

/*
 * "SymbolTable" contains data related to variables encountered during semantic analysis and used
 * by the stacker.
 */

pub struct SymbolTable<'a> {
    pub depth: i32,
    symbols_in_scope: HashMap<i32, HashMap<String, (SymbolType, usize, i32, bool)>>,
    counts: Counts,
    function_data: HashMap<&'a str, FunctionData<'a>>,
    current_fname: Option<&'a str>,
    string_literals: String,
    string_literal_bytes: Vec<(usize, usize)>,
    write_arguments: HashMap<(u32, u32), Vec<SymbolType>>,
    pub library_functions: HashSet<&'a str>,
}

impl<'a> SymbolTable<'a> {
    pub fn insert(&mut self, key: &'a str, st: SymbolType) {
        assert!(
            -1 < self.depth,
            "Scope depth must be positive, use step_in() before insert."
        );
        assert!(
            self.current_fname.is_some(),
            "Current function name must be set before inserting values."
        );
        assert!(
            SymbolType::Undefined != st,
            "Trying to insert a variable of undefined type."
        );

        let count = self.counts.get_count(self.depth as usize, st);
        self.symbols_in_scope
            .entry(self.depth)
            .or_insert(HashMap::new())
            .insert(key.to_lowercase(), (st, *count, self.depth, false));

        *count += 1;
    }

    pub fn get(&self, key: &'a str) -> Option<&(SymbolType, usize, i32, bool)> {
        self.symbols_in_scope
            .get(&self.depth)
            .and_then(|m| m.get(&key.to_lowercase()))
    }

    pub fn find(&self, key: &'a str) -> Option<&(SymbolType, usize, i32, bool)> {
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

        self.counts.push();

        if new_fname.is_some() {
            self.current_fname = new_fname;
            // Add parameters of this function to scope
            for (i, param) in self
                .function_data
                .get(new_fname.unwrap())
                .expect("Every function should have a signature at this point.")
                .signature
                .parameters
                .iter()
                .enumerate()
            {
                self.symbols_in_scope
                    .entry(self.depth)
                    .or_insert(HashMap::new())
                    .insert(
                        param.id.to_lowercase(),
                        (param.symbol_type, i, self.depth, param.is_ref),
                    );
            }
        }
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
                let num_parameters = fd.signature.parameters.len();
                update_function_data(&mut fd.i32_idx, &mut fd.it, &ics, it, num_parameters);
                update_function_data(&mut fd.f32_idx, &mut fd.ft, &fcs, ft, it + num_parameters);

                self.counts.clear();
                self.current_fname = None;
            }
        }
    }

    pub fn get_variable_index(&self, fname: &'a str, st: SymbolType, depth: i32) -> usize {
        // Depth 0 is reserved for parameters. Since parameters can be in arbitrary order but local
        // variables are ordered by type (all i32 represented variables first, then f32), handling
        // differs for the two cases.
        if let Some(fd) = self.function_data.get(fname) {
            if 0 == depth {
                0
            } else {
                assert!(
                    SymbolType::Undefined != st,
                    "Trying to get a variable of undefined type."
                );
                let v = if SymbolType::Real == st {
                    fd.f32_idx.get(&depth)
                } else {
                    fd.i32_idx.get(&depth)
                }
                .expect("No variable found with given symbol type and depth.");
                *v
            }
        } else {
            assert!(false, "No function data with given function name.");
            0
        }
    }

    pub fn add_string_literal<'b>(&mut self, literal: &'b str) -> usize {
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

    pub fn borrow_string_literals(&self) -> &String {
        &self.string_literals
    }

    pub fn borrow_string_literal_bytes(&self) -> &Vec<(usize, usize)> {
        &self.string_literal_bytes
    }

    pub fn add_write_arguments(&mut self, loc: (u32, u32), args: Vec<SymbolType>) {
        self.write_arguments.insert(loc, args);
    }

    pub fn get_write_arguments(&mut self, loc: (u32, u32)) -> Vec<SymbolType> {
        self.write_arguments
            .remove(&loc)
            .expect("Write arguments should be saved.")
    }

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
                num_refs: 0,
            })
            .signature = fs;
    }

    pub fn get_function_signature(&self, key: &'a str) -> Option<&FunctionSignature<'a>> {
        self.function_data.get(key).map(|fd| &fd.signature)
    }

    pub fn increment_ref_count(&mut self, num: usize) {
        self.current_fname.and_then(|fname| {
            self.function_data.get_mut(fname).and_then(|fd| {
                if fd.num_refs < num {
                    fd.num_refs = num;
                    Some(num)
                } else {
                    None
                }
            })
        });
    }

    pub fn get_function_ref_count(&self, key: &'a str) -> Option<usize> {
        self.function_data.get(key).and_then(|fd| {
            if fd.num_refs > 0 {
                Some(fd.num_refs)
            } else {
                None
            }
        })
    }

    pub fn get_local_variable_totals(&self, key: &'a str) -> Option<(usize, usize)> {
        self.function_data.get(key).map(|fd| (fd.it, fd.ft))
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
        println!("{}", literal);
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
            write_arguments: HashMap::new(),
            library_functions: HashSet::new(),
        }
    }
}

/*
 * "Counts" is used to keep track of how many i32 representable and how many f32 representable
 * variables are encountered as a function (or the main block) is traversed.
 * Example:
 * The maximum number of reals encountered (so far) in all the scopes of depth 3 is fm[3].
*/

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

    pub fn get_count(&mut self, idx: usize, st: SymbolType) -> &mut usize {
        // Reals are represented as f32 in webassembly, everything else as i32
        if SymbolType::Real == st {
            self.fc
                .get_mut(idx)
                .expect("f32 counts does not contain a value at the given idx.")
        } else {
            self.ic
                .get_mut(idx)
                .expect("i32 counts does not contain a value at the given idx.")
        }
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

    pub fn clear(&mut self) {
        self.ic.clear();
        self.im.clear();
        self.fc.clear();
        self.fm.clear();
    }
}

/*
* "FunctionData" contains data related to a function.
* The idx maps contain the starting indices for local variables at different scope depths. The scope
* depth is the key and the value is the starting index. All other variables are represented as i32
* but reals are represented as f32. The local variables are recycled per depth, i.e. different
* scopes with the same depth use the same local variables.
* Total number of each kind of local variable is saved as well, and the function signature is
* saved in the struct of the same name.
*/

#[derive(Debug)]
pub struct FunctionData<'a> {
    i32_idx: HashMap<i32, usize>,
    f32_idx: HashMap<i32, usize>,
    it: usize,
    ft: usize,
    signature: FunctionSignature<'a>,
    num_refs: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data_types::Parameter;

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

    #[test]
    fn variable_indices() {
        let var_types = vec![
            SymbolType::Bool,
            SymbolType::Real,
            SymbolType::Int,
            SymbolType::Int,
            SymbolType::Bool,
            SymbolType::Int, // first local
            SymbolType::Int,
            SymbolType::Real,
            SymbolType::ArrayReal(0),
            SymbolType::String,
            SymbolType::String,
            SymbolType::Real,
            SymbolType::Int,
            SymbolType::String,
            SymbolType::Real,
            SymbolType::Int,
            SymbolType::Int,
            SymbolType::Int,
            SymbolType::ArrayReal(0),
            SymbolType::Real,
        ];
        let vars: Vec<(String, SymbolType)> = (0..var_types.len())
            .map(|i| (i.to_string(), var_types[i]))
            .collect();
        let num_params = 5;
        let mut locals: Vec<(&str, SymbolType, usize)> = (num_params..var_types.len())
            .map(|i| (vars[i].0.as_str(), vars[i].1, 0))
            .collect();
        let parameters = (0..num_params)
            .map(|i| Parameter {
                _is_ref: false,
                symbol_type: vars[i].1,
                id: &vars[i].0,
            })
            .collect();

        let mut symbol_table = SymbolTable::new();
        let fs = FunctionSignature {
            parameters: parameters,
            return_type: None,
            token: EMPTY_TOKEN,
        };

        symbol_table.insert_function_signature(fs.token.value, fs);
        symbol_table.step_in(Some(EMPTY_TOKEN.value));
        // depth 0
        {
            symbol_table.step_in(None);
            // depth 1
            {
                symbol_table.insert(locals[0].0, locals[0].1); // i32 0 + 5 = 5
                symbol_table.insert(locals[1].0, locals[1].1); // i32 1 + 5 = 6
                symbol_table.insert(locals[2].0, locals[2].1); // f32 0 + 13 = 13

                locals[0].2 = symbol_table.get(locals[0].0).unwrap().1;
                locals[1].2 = symbol_table.get(locals[1].0).unwrap().1;
                locals[2].2 = symbol_table.get(locals[2].0).unwrap().1;

                symbol_table.step_in(None);
                // depth 2
                {
                    symbol_table.insert(locals[3].0, locals[3].1); // i32 0 + 8
                    symbol_table.insert(locals[4].0, locals[4].1); // i32 1 + 8
                    symbol_table.insert(locals[5].0, locals[5].1); // i32 2 + 8

                    locals[3].2 = symbol_table.get(locals[3].0).unwrap().1;
                    locals[4].2 = symbol_table.get(locals[4].0).unwrap().1;
                    locals[5].2 = symbol_table.get(locals[5].0).unwrap().1;
                }
                symbol_table.step_out();

                // depth 1
                symbol_table.insert(locals[6].0, locals[6].1); // f32 1 + 13 = 14
                locals[6].2 = symbol_table.get(locals[6].0).unwrap().1;

                symbol_table.step_in(None);
                // depth 2
                {
                    symbol_table.insert(locals[7].0, locals[7].1); // i32 0 + 8 = 8
                    symbol_table.insert(locals[8].0, locals[8].1); // i32 1 + 8 = 9
                    symbol_table.insert(locals[9].0, locals[9].1); // f32 0 + 16 = 16

                    locals[7].2 = symbol_table.get(locals[7].0).unwrap().1;
                    locals[8].2 = symbol_table.get(locals[8].0).unwrap().1;
                    locals[9].2 = symbol_table.get(locals[9].0).unwrap().1;

                    symbol_table.step_in(None);
                    // depth 3
                    {
                        symbol_table.insert(locals[10].0, locals[10].1); // i32 0 + 11 = 11
                        symbol_table.insert(locals[11].0, locals[11].1); // i32 1 + 11 = 12

                        locals[10].2 = symbol_table.get(locals[10].0).unwrap().1;
                        locals[11].2 = symbol_table.get(locals[11].0).unwrap().1;
                    }
                    symbol_table.step_out();
                }
                symbol_table.step_out();

                // depth 1
                symbol_table.insert(locals[12].0, locals[12].1); // i32 2 + 5 = 7
                locals[12].2 = symbol_table.get(locals[12].0).unwrap().1;

                symbol_table.step_in(None);
                // depth 2
                {
                    symbol_table.insert(locals[13].0, locals[13].1); // i32 0 + 8 = 8
                    locals[13].2 = symbol_table.get(locals[13].0).unwrap().1;
                }
                symbol_table.step_out();

                // depth 1
                symbol_table.insert(locals[14].0, locals[14].1); // f32 2 + 13 = 15
                locals[14].2 = symbol_table.get(locals[14].0).unwrap().1;
            }
            symbol_table.step_out();
        }
        symbol_table.step_out();

        let depths = vec![0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2, 3, 3, 1, 2, 1];
        let counts: Vec<usize> = (0..num_params).chain(locals.iter().map(|l| l.2)).collect();
        let indices = vec![
            (0, 0),
            (1, 0),
            (2, 0),
            (3, 0),
            (4, 0),
            (0, 5),
            (1, 5),
            (0, 13),
            (0, 8),
            (1, 8),
            (2, 8),
            (1, 13),
            (0, 8),
            (1, 8),
            (0, 16),
            (0, 11),
            (1, 11),
            (2, 5),
            (0, 8),
            (2, 13),
        ];

        for i in 0..var_types.len() {
            assert_eq!(indices[i].0, counts[i]);
            assert_eq!(
                indices[i].1,
                symbol_table.get_variable_index(EMPTY_TOKEN.value, vars[i].1, depths[i])
            );
        }
    }
}

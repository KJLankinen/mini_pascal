use super::data_types::{ErrorType, SymbolType};

pub struct Logger<'a> {
    errors: Vec<ErrorType<'a>>,
    lines: Vec<&'a str>,
}

impl<'a> Logger<'a> {
    pub fn new(source_str: &'a str) -> Self {
        Logger {
            errors: vec![],
            lines: source_str.lines().collect(),
        }
    }

    pub fn add_error(&mut self, error: ErrorType<'a>) {
        self.errors.push(error);
    }

    pub fn print_errors(&self) {
        for error in self.errors.iter() {
            eprintln!("error: {}", error);
            let column;
            let line;
            match error {
                ErrorType::LexicalError(token) => {
                    eprintln!("Encountered an undefined token \"{}\".", token.value);
                    line = token.line;
                    column = token.column;
                }
                ErrorType::SyntaxError(token, expected) => {
                    if 1 < expected.len() {
                        eprint!("Expected one of ");
                        for e in expected.iter() {
                            eprint!("\"{}\", ", e);
                        }
                    } else if 0 < expected.len() {
                        eprint!("Expected \"{}\", ", expected[0]);
                    } else {
                        eprint!("Unexpectedly ");
                    }
                    eprintln!("found \"{}\".", token.value);
                    line = token.line;
                    column = token.column;
                }
                ErrorType::MismatchedTypes(token, type1, type2) => {
                    eprint!("Types used with operator \"{}\" don't match. ", token.value);
                    eprintln!(
                        "Left side is of type \"{}\", right side of type \"{}\".",
                        type1, type2
                    );
                    line = token.line;
                    column = token.column;
                }
                ErrorType::UndeclaredIdentifier(token) => {
                    eprintln!("Identifier \"{}\" is used before declaration.", token.value);
                    line = token.line;
                    column = token.column;
                }
                ErrorType::IllegalOperation(token, symbol) => {
                    eprintln!(
                        "Operator \"{}\" can't be used in an expression with type \"{}\".",
                        token.value, symbol
                    );
                    line = token.line;
                    column = token.column;
                }
                ErrorType::UnmatchedComment(l, c) => {
                    eprintln!(
                        "Multi-line comment starts at {}:{} but is never terminated.",
                        l, c
                    );
                    line = *l;
                    column = *c;
                }
                ErrorType::Redeclaration(token) => {
                    eprintln!(
                        "A redeclaration of an already declared variable \"{}\".",
                        token.value
                    );
                    line = token.line;
                    column = token.column;
                }
                ErrorType::AssignMismatchedType(token, identifier_type, expression_type) => {
                    eprint!("Mismatched types in assignment. Identifier is of type ");
                    eprintln!(
                        "\"{}\', expression of type \"{}\".",
                        identifier_type, expression_type
                    );
                    line = token.line;
                    column = token.column;
                }
                ErrorType::IOMismatchedType(token, expression_type) => {
                    eprint!(
                        "Read and print can only be used with types \"{}\" and \"{}\", ",
                        SymbolType::Int,
                        SymbolType::String
                    );
                    eprintln!("not with the given type of \"{}\".", expression_type);
                    line = token.line;
                    column = token.column;
                }
                ErrorType::AssertMismatchedType(token, expression_type) => {
                    eprint!(
                        "Assert can only be used with type \"{}\", ",
                        SymbolType::Bool
                    );
                    eprintln!("given expression was of type \"{}\".", expression_type);
                    line = token.line;
                    column = token.column;
                }
                ErrorType::IndexTypeMismatch(token, expression_type) => {
                    eprint!(
                        "Indexing expression must be of type \"{}\", ",
                        SymbolType::Int
                    );
                    eprintln!("expression was of type \"{}\".", expression_type);
                    line = token.line;
                    column = token.column;
                }
                ErrorType::IllegalIndexing(token, symbol_type) => {
                    eprint!(
                        "Indexing can only be used with types \"{}\", \"{}\", \"{}\", \"{}\", ",
                        SymbolType::ArrayBool(0),
                        SymbolType::ArrayInt(0),
                        SymbolType::ArrayReal(0),
                        SymbolType::ArrayString(0),
                    );
                    eprintln!("while identifier was of type \"{}\".", symbol_type);
                    line = token.line;
                    column = token.column;
                }
                ErrorType::TooFewArguments(
                    function_token,
                    parameter_types,
                    call_token,
                    argument_types,
                )
                | ErrorType::TooManyArguments(
                    function_token,
                    parameter_types,
                    call_token,
                    argument_types,
                )
                | ErrorType::MismatchedArgumentTypes(
                    function_token,
                    parameter_types,
                    call_token,
                    argument_types,
                ) => {
                    eprint!(
                        "Function takes {} arguments with types ",
                        parameter_types.len()
                    );
                    for pt in parameter_types {
                        eprint!("{}, ", pt);
                    }
                    eprint!("\n");
                    self.print_error(function_token.line as usize, function_token.column as usize);

                    eprint!(
                        "but caller supplied {} arguments with types ",
                        argument_types.len()
                    );
                    for at in argument_types {
                        eprint!("{}, ", at);
                    }
                    eprint!("\n");
                    line = call_token.line;
                    column = call_token.column;
                }
            }
            self.print_error(line as usize, column as usize);
        }
    }

    fn print_line(&self, line: usize) {
        assert!(line > 0);
        // Lines start from '1' in editors but vector indexing starts from '0'
        let line = line - 1;
        assert!(line < self.lines.len(), "Line number is too large.");
        eprint!("{}", self.lines[line]);
    }

    fn print_error(&self, line: usize, column: usize) {
        // A debug functions for printing a specific line of source code.
        assert!(line > 0);
        // Lines start from '1' in editors but vector indexing starts from '0'
        let line = line - 1;
        assert!(line < self.lines.len(), "Line number is too large.");

        let expl_string = format!(
            "{}^------",
            vec![' '; column - 1].into_iter().collect::<String>(),
        );

        //eprintln!("error @ {}:{}", line + 1, column);
        if 0 < line {
            eprintln!("  {}\t|\t{}", line, self.lines[line - 1]);
        }
        eprintln!("  {}\t|\t{}", line + 1, self.lines[line]);
        eprintln!("\t|\t{}", expl_string);
    }

    pub fn get_line(&self, line: usize) -> &'a str {
        assert!(line <= self.lines.len());
        self.lines[line - 1]
    }

    pub fn errors_encountered(&self) -> bool {
        !self.errors.is_empty()
    }
}

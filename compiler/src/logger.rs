use super::data_types::ErrorType as ET;
use super::data_types::SymbolType as ST;

pub struct Logger<'a> {
    errors: Vec<ET<'a>>,
    lines: Vec<&'a str>,
    pub file_name: &'a str,
}

impl<'a> Logger<'a> {
    pub fn new(source_str: &'a str, file_name: &'a str) -> Self {
        Logger {
            errors: vec![],
            lines: source_str.lines().collect(),
            file_name: file_name,
        }
    }

    pub fn add_error(&mut self, error: ET<'a>) {
        self.errors.push(error);
    }

    pub fn print_errors(&self) {
        for error in self.errors.iter() {
            eprintln!("error: {}", error);
            let column;
            let line;
            match error {
                ET::LexicalError(token) => {
                    eprintln!("Encountered an undefined token \"{}\".", token.value);
                    line = token.line;
                    column = token.column;
                }
                ET::SyntaxError(token, expected) => {
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
                ET::UndeclaredIdentifier(token) => {
                    eprintln!("Identifier \"{}\" is used before declaration.", token.value);
                    line = token.line;
                    column = token.column;
                }
                ET::IllegalOperation(token, symbols) => {
                    eprint!(
                        "Operator \"{}\" can't be used in an expression with type(s) ",
                        token.value
                    );
                    for st in symbols {
                        eprint!("\"{}\", ", st);
                    }
                    eprintln!("");
                    line = token.line;
                    column = token.column;
                }
                ET::UnmatchedComment(l, c) => {
                    eprintln!(
                        "Multi-line comment starts at {}:{} but is never terminated.",
                        l, c
                    );
                    line = *l;
                    column = *c;
                }
                ET::Redeclaration(token) => {
                    eprintln!(
                        "A redeclaration of an already declared variable \"{}\".",
                        token.value
                    );
                    line = token.line;
                    column = token.column;
                }
                ET::AssignMismatchedType(token, identifier_type, expression_type) => {
                    eprint!("Mismatched types in assignment. Identifier is of type ");
                    eprintln!(
                        "\"{}\", expression of type \"{}\".",
                        identifier_type, expression_type
                    );
                    line = token.line;
                    column = token.column;
                }
                ET::AssertMismatchedType(token, expression_type) => {
                    eprint!("Assert can only be used with type \"{}\", ", ST::Bool);
                    eprintln!("given expression was of type \"{}\".", expression_type);
                    line = token.line;
                    column = token.column;
                }
                ET::IndexTypeMismatch(token, expression_type) => {
                    eprint!("Indexing expression must be of type \"{}\", ", ST::Int);
                    eprintln!("expression was of type \"{}\".", expression_type);
                    line = token.line;
                    column = token.column;
                }
                ET::IllegalIndexing(token, symbol_type) => {
                    eprint!(
                        "Indexing can only be used with types \"{}\", \"{}\", \"{}\", \"{}\", ",
                        ST::ArrayBool(0),
                        ST::ArrayInt(0),
                        ST::ArrayReal(0),
                        ST::ArrayString(0),
                    );
                    eprintln!("while identifier was of type \"{}\".", symbol_type);
                    line = token.line;
                    column = token.column;
                }
                ET::TooFewArguments(
                    function_token,
                    parameter_types,
                    call_token,
                    argument_types,
                )
                | ET::TooManyArguments(
                    function_token,
                    parameter_types,
                    call_token,
                    argument_types,
                )
                | ET::MismatchedArgumentTypes(
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
                ET::MismatchedReturnType(token, return_type, expected_return_type) => {
                    eprintln!("Mismatched return type. Return expression is of type \"{}\", while expected type is \"{}\".",
                        return_type.map(|t| format!("{}", t)).unwrap_or_else(|| "void".to_string()),
                        expected_return_type.map(|t| format!("{}", t)).unwrap_or_else(|| "void".to_string())
                    );
                    line = token.line;
                    column = token.column;
                }
                ET::ReadMismatchedType(token, variable_type) => {
                    eprintln!("Read is not defined for type \"{}\".", variable_type);
                    line = token.line;
                    column = token.column;
                }
                ET::ExprTypeMismatch(token, expected_type, given_type) => {
                    eprintln!(
                        "Expression type is expected to be \"{}\", but instead it is \"{}\".",
                        expected_type, given_type
                    );
                    line = token.line;
                    column = token.column;
                }
                ET::MissingReturnStatements(token) => {
                    eprintln!("Function does not return from every possible branch.");
                    line = token.line;
                    column = token.column;
                }
                ET::ArraySizeTypeMismatch(token, factor_type) => {
                    eprintln!(
                        ".size operator is not defined for type \"{}\".",
                        factor_type
                    );
                    line = token.line;
                    column = token.column;
                }
            }
            self.print_error(line as usize, column as usize);
        }
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

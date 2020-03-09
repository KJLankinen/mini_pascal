use super::data_types::ErrorType;

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
                    if type2.is_some() {
                        eprint!("Types used with token \"{}\" don't match. ", token.value);
                        eprintln!(
                            "Left side is of type \"{}\", right side of type \"{}\".",
                            type1,
                            type2.unwrap()
                        );
                    } else {
                        eprintln!(
                            "Expression of type \"{}\" is illegal in this context.",
                            type1
                        );
                    }
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
                ErrorType::AssignmentToBlockedVariable(token) => {
                    eprintln!(
                        "Assignment to the variable \"{}\" is not allowed.",
                        token.value
                    );
                    line = token.line;
                    column = token.column;
                }
            }
            self.print_line(line as usize, column as usize);
        }
    }

    fn print_line(&self, line: usize, column: usize) {
        // A debug functions for printing a specific line of source code.
        assert!(line > 0);
        // Lines start from '1' in editors but vector indexing starts from '0'
        let line = line - 1;
        assert!(line < self.lines.len(), "Line number is too large.");

        let expl_string = format!(
            "{}^------",
            vec![' '; column - 1].into_iter().collect::<String>(),
        );

        println!("error @ {}:{}", line + 1, column);
        if 0 < line {
            println!("  {}\t|\t{}", line, self.lines[line - 1]);
        }
        println!("  {}\t|\t{}", line + 1, self.lines[line]);
        println!("\t|\t{}", expl_string);
    }

    pub fn errors_encountered(&self) -> bool {
        !self.errors.is_empty()
    }
}
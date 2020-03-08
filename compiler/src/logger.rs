use super::data_types::ErrorType;

pub struct Logger<'a> {
    errors: Vec<ErrorType<'a>>,
    lines: Vec<&'a str>,
    source_str: &'a str,
}

impl<'a> Logger<'a> {
    pub fn new(source_str: &'a str) -> Self {
        Logger {
            errors: vec![],
            lines: source_str.lines().collect(),
            source_str: source_str,
        }
    }

    pub fn add_error(&mut self, error: ErrorType<'a>) {
        self.errors.push(error);
    }

    pub fn print_errors(&self) {
        for error in self.errors.iter() {
            eprint!("{}: ", error);
            let column;
            let line;
            match error {
                ErrorType::SyntaxError(token, expected) => {
                    if 1 < expected.len() {
                        eprint!("expected one of ");
                        for e in expected.iter() {
                            eprint!("\"{}\", ", e);
                        }
                    } else if 0 < expected.len() {
                        eprint!("expected \"{}\", ", expected[0]);
                    } else {
                        eprint!("Unexpectedly ");
                    }
                    eprintln!("found \"{}\".", token.value);
                    line = token.line as usize;
                    column = token.column as usize;
                }
                ErrorType::MismatchedTypes => {
                    line = 0;
                    column = 0
                }
                ErrorType::UndeclaredIdentifier => {
                    line = 0;
                    column = 0
                }
                ErrorType::IllegalOperation => {
                    line = 0;
                    column = 0
                }
                ErrorType::UnmatchedComment(l, c) => {
                    eprintln!(
                        "Multi-line comment starts at {}:{} but is never terminated.",
                        l, c
                    );
                    line = *l as usize;
                    column = *c as usize;
                }
                ErrorType::Undefined => {
                    line = 0;
                    column = 0
                }
            }
            self.print_line(line, column);
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

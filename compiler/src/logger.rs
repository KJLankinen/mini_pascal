use super::data_types::ErrorType;

pub struct Logger {
    errors: Vec<ErrorType>,
}

impl Logger {
    pub fn new() -> Self {
        Logger { errors: vec![] }
    }

    pub fn print_errors(&self) {
        for error in self.errors.iter() {
            println!("{}", error);
        }
    }
}

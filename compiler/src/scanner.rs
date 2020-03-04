use serde::Serialize;
use std::collections::HashMap;
use std::fmt;

// ---------------------------------------------------------------------
// Type definition for the scanner that goes through the source string
// to identify and categorize tokens.
// ---------------------------------------------------------------------
#[derive(Debug)]
pub struct Scanner<'a> {
    column: u32,
    line: u32,
    skip_until_newline: bool,
    previous_newline: usize,
    pub unmatched_multiline_comment_prefixes: Vec<(u32, u32)>,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    lines: Vec<&'a str>,
    token_map: HashMap<&'static str, TokenType>,
    source_str: &'a str,
    next_token: TokenData<'a>,
}

// ---------------------------------------------------------------------
// Methods for the scanner
// ---------------------------------------------------------------------
impl<'a> Scanner<'a> {
    // ---------------------------------------------------------------------
    // next() and peek() are the main two out facing functions of the scanner.
    // ---------------------------------------------------------------------
    pub fn next(&mut self) -> TokenData<'a> {
        // The parser receives new tokens by calling this function
        let token = self.next_token;
        self.next_token = self.get_token();
        token
    }

    pub fn peek(&mut self) -> &TokenData<'a> {
        &self.next_token
    }

    // ---------------------------------------------------------------------
    // These functions are related to the inner workings of the scanner.
    // ---------------------------------------------------------------------
    fn get_char(&mut self) -> Option<(usize, char)> {
        // This function makes sure line and column are always counted correctly. Every possible
        // character that goes through the scanner comes from this function. Token length and
        // possible internal state changes are also done, e.g. when a newline character is found.
        // Also each line is stored individually for error messaging purposes.
        match self.chars.peek() {
            Some((pos, ch)) => {
                self.column += 1;
                if &'\n' == ch {
                    self.line += 1;
                    self.column = 0;
                    self.skip_until_newline = false;
                    self.lines
                        .push(&self.source_str[self.previous_newline..*pos]);
                    self.previous_newline = *pos + 1;
                }
                self.chars.next()
            }
            None => None,
        }
    }

    fn skipped_comment(&mut self) -> bool {
        // Enter this function with a single '/'. Peek ahead, and if a comment is starting,
        // skip it. If not, return false and let the scanner continue.
        match self.chars.peek() {
            Some((_, ch)) => {
                match ch {
                    &'/' => {
                        // Singe line comment
                        self.skip_until_newline = true;
                        while self.skip_until_newline {
                            // get_char() will change skip_until_newline to false once it finds '\n'
                            self.get_char();
                        }
                    }
                    &'*' => {
                        // Multi line comment
                        let mut depth: i32 = 1;
                        // Store the position of the start of the multiline comment for error
                        // messaging: if this vector is not empty when end of program is found,
                        // these can be used to specify the correct location of the rogue multiline
                        // comment start.
                        self.unmatched_multiline_comment_prefixes
                            .push((self.line, self.column));
                        self.get_char();
                        while let Some((_, ch)) = self.get_char() {
                            match ch {
                                '*' => {
                                    // This comment might be ending
                                    if let Some((_, ch)) = self.chars.peek() {
                                        if &'/' == ch {
                                            self.get_char();
                                            depth -= 1;
                                            self.unmatched_multiline_comment_prefixes.pop();
                                        }
                                    }
                                }
                                '/' => {
                                    // A new multiline comment might be starting
                                    if let Some((_, ch)) = self.chars.peek() {
                                        if &'*' == ch {
                                            self.unmatched_multiline_comment_prefixes
                                                .push((self.line, self.column));
                                            self.get_char();
                                            depth += 1;
                                        }
                                    }
                                }
                                _ => {}
                            }

                            if 0 == depth {
                                break;
                            }

                            // Something weird is going on, if depth is negative. Note that if the
                            // depth is strictly greater than 0, we don't care about that here,
                            // although it is a syntax error. That is handled later.
                            assert!(depth > 0);
                        }
                    }
                    _ => return false, // The next character after the first '/' was neither '/' nor '*'.
                }
                true // Some characters were skipped over
            }
            None => false, // There are no more characters left, so couldn't possibly skip over them
        }
    }

    fn get_token(&mut self) -> TokenData<'a> {
        // This function is the main work horse of the scanner. This function will always return
        // a valid (but maybe undefined) token, even after the source has ended.
        // If there's no more characters left, the EndOfProgram token is returned repeatedly.
        let mut token = TokenData {
            column: self.column,
            line: self.line,
            value: "",
            token_type: TokenType::Undefined,
        };

        // Loop eternally, until an end condition is met
        loop {
            match self.get_char() {
                Some((pos, ch)) => {
                    if ch.is_whitespace() {
                        continue;
                    }

                    token.column = self.column;
                    token.line = self.line;

                    // Most tokens get their type from the map access at the bottom.
                    // This match disambiguates between some situations like comment vs divide op
                    // when '/' is encountered. Also longer tokens are handled by this match.
                    // These rules/patterns in the match statements are only for the starting
                    // characters. Once an arm is entered, it's not left until the correct
                    // token is finished.
                    match ch {
                        '.' => {
                            // See if there's a second dot following the first
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'.' == ch {
                                    self.get_char();
                                }
                            }
                        }
                        '/' => {
                            if self.skipped_comment() {
                                continue;
                            }
                        }
                        ':' => {
                            // Disambiguate between separator and assignment
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'=' == ch {
                                    self.get_char();
                                }
                            }
                        }
                        '"' => {
                            token.token_type = TokenType::LiteralString;
                            let mut escape_next = false;

                            // Run until the next '"' without a preceding '\' is found, or until
                            // the source string runs out, in which case there's obviously a syntax
                            // error. We don't care about that here, the parser will take care of it.
                            while let Some((_, ch)) = self.chars.peek() {
                                let is_finished = &'"' == ch && false == escape_next;
                                escape_next = &'\\' == ch;
                                self.get_char();

                                if is_finished {
                                    break;
                                }
                            }
                        }
                        'A'..='z' => {
                            token.token_type = TokenType::Identifier;

                            while let Some((_, ch)) = self.chars.peek() {
                                if ch.is_alphanumeric() || &'_' == ch {
                                    self.get_char();
                                    continue;
                                }
                                break;
                            }
                        }
                        '0'..='9' => {
                            token.token_type = TokenType::LiteralInt;

                            while let Some((_, ch)) = self.chars.peek() {
                                if ch.is_numeric() {
                                    self.get_char();
                                    continue;
                                }
                                break;
                            }
                        }
                        _ => {
                            // The token is something else. If it's a legal token, the map will
                            // give its type. If it's illegal, the token type will be undefined
                            // (with which it was initialized at the start of this function) and
                            // the resulting lexical error will be handled by the parser.
                        }
                    }

                    let token_end = match self.chars.peek() {
                        Some((pos, _)) => *pos,
                        None => self.source_str.len(),
                    };
                    token.value = &self.source_str[pos..token_end];

                    // Get your type here, if you're in the map
                    if let Some(tt) = self.token_map.get(token.value) {
                        token.token_type = *tt;
                    }
                }
                None => {
                    // Out of characters
                    token.value = "\0";
                    token.token_type = TokenType::EndOfProgram;
                }
            }

            // Always return Some(token), even when it's illegal or we've encountered the
            // EndOfProgram. In the first case, the token is undefined, in the second we'll just
            // repeatedly return the EndOfProgram token until even the most stone headed parser
            // will understand to stop parsing nothingness.
            return token;
        }
    }

    // ---------------------------------------------------------------------
    // Public auxiliary functions
    // ---------------------------------------------------------------------
    pub fn new(source_str: &'a str) -> Scanner<'a> {
        let mut scanner = Scanner {
            column: 0,
            line: 1,
            skip_until_newline: false,
            previous_newline: 0,
            unmatched_multiline_comment_prefixes: vec![],
            chars: source_str.char_indices().peekable(),
            lines: vec![],
            source_str: source_str,
            next_token: TokenData::default(),
            token_map: [
                ("var", TokenType::KeywordVar),
                ("for", TokenType::KeywordFor),
                ("end", TokenType::KeywordEnd),
                ("in", TokenType::KeywordIn),
                ("do", TokenType::KeywordDo),
                ("read", TokenType::KeywordRead),
                ("print", TokenType::KeywordPrint),
                ("assert", TokenType::KeywordAssert),
                ("int", TokenType::TypeInt),
                ("string", TokenType::TypeString),
                ("bool", TokenType::TypeBool),
                ("(", TokenType::LParen),
                (")", TokenType::RParen),
                (";", TokenType::EndOfStatement),
                ("+", TokenType::OperatorPlus),
                ("-", TokenType::OperatorMinus),
                ("*", TokenType::OperatorMultiply),
                ("/", TokenType::OperatorDivide),
                ("<", TokenType::OperatorLessThan),
                ("=", TokenType::OperatorEqual),
                ("&", TokenType::OperatorAnd),
                ("!", TokenType::OperatorNot),
                ("..", TokenType::Range),
                (":", TokenType::TypeSeparator),
                (":=", TokenType::Assignment),
            ]
            .iter()
            .cloned()
            .collect(),
        };
        // Initialize next_token
        scanner.next_token = scanner.get_token();
        return scanner;
    }

    pub fn print_line(&self, line: usize, column: usize) {
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
}

// ---------------------------------------------------------------------
// Type definitions for the token types and data
// ---------------------------------------------------------------------
#[derive(Serialize, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenType {
    Identifier,
    KeywordVar,
    KeywordFor,
    KeywordEnd,
    KeywordIn,
    KeywordDo,
    KeywordRead,
    KeywordPrint,
    KeywordAssert,
    TypeInt,
    TypeString,
    TypeBool,
    LiteralInt,
    LiteralString,
    OperatorPlus,
    OperatorMinus,
    OperatorMultiply,
    OperatorDivide,
    OperatorLessThan,
    OperatorEqual,
    OperatorAnd,
    OperatorNot,
    Range,
    EndOfStatement,
    TypeSeparator,
    Assignment,
    LParen,
    RParen,
    EndOfProgram,
    Undefined,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Identifier => write!(f, "an identifier"),
            TokenType::KeywordVar => write!(f, "var"),
            TokenType::KeywordFor => write!(f, "for"),
            TokenType::KeywordEnd => write!(f, "end"),
            TokenType::KeywordIn => write!(f, "in"),
            TokenType::KeywordDo => write!(f, "do"),
            TokenType::KeywordRead => write!(f, "read"),
            TokenType::KeywordPrint => write!(f, "print"),
            TokenType::KeywordAssert => write!(f, "assert"),
            TokenType::TypeInt => write!(f, "int"),
            TokenType::TypeString => write!(f, "string"),
            TokenType::TypeBool => write!(f, "bool"),
            TokenType::LiteralInt => write!(f, "a literal integer"),
            TokenType::LiteralString => write!(f, "a literal string"),
            TokenType::OperatorPlus => write!(f, "+"),
            TokenType::OperatorMinus => write!(f, "-"),
            TokenType::OperatorMultiply => write!(f, "*"),
            TokenType::OperatorDivide => write!(f, "/"),
            TokenType::OperatorLessThan => write!(f, "<"),
            TokenType::OperatorEqual => write!(f, "="),
            TokenType::OperatorAnd => write!(f, "&"),
            TokenType::OperatorNot => write!(f, "!"),
            TokenType::Range => write!(f, ".."),
            TokenType::EndOfStatement => write!(f, ";"),
            TokenType::TypeSeparator => write!(f, ":"),
            TokenType::Assignment => write!(f, ":="),
            TokenType::LParen => write!(f, "("),
            TokenType::RParen => write!(f, ")"),
            TokenType::EndOfProgram => write!(f, "\0"),
            TokenType::Undefined => write!(f, "an undefined token"),
        }
    }
}

#[derive(Serialize, Debug, Clone, Copy)]
pub struct TokenData<'a> {
    pub column: u32,
    pub line: u32,
    pub token_type: TokenType,
    pub value: &'a str,
}

impl<'a> Default for TokenData<'a> {
    fn default() -> TokenData<'a> {
        TokenData {
            column: 0,
            line: 0,
            token_type: TokenType::Undefined,
            value: "",
        }
    }
}

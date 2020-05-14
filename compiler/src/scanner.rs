use super::data_types::{TokenData, TokenType};
use std::collections::{HashMap, VecDeque};

// ---------------------------------------------------------------------
// Type definition for the scanner that goes through the source string
// to identify and categorize tokens.
// ---------------------------------------------------------------------
pub struct Scanner<'a> {
    column: u32,
    line: u32,
    skip_until_newline: bool,
    pub unmatched_multiline_comment_prefixes: Vec<(u32, u32)>,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    token_queue: VecDeque<TokenData<'a>>,
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
        self.next_token = if self.token_queue.is_empty() {
            self.get_token()
        } else {
            self.token_queue
                .pop_front()
                .expect("Token queue should have actual tokens, if it's not empty.")
        };
        token
    }

    pub fn peek_at(&mut self, distance: usize) -> &TokenData<'a> {
        assert!(distance < 10, "Sanity check.");

        if 0 == distance {
            &self.next_token
        } else {
            while self.token_queue.len() < distance {
                let token = self.get_token();
                self.token_queue.push_back(token);
            }
            self.token_queue
                .get(distance - 1)
                .expect("Token queue should contain a token at distance - 1")
        }
    }

    pub fn peek(&mut self) -> &TokenData<'a> {
        &self.next_token
    }

    // ---------------------------------------------------------------------
    // These functions are related to the inner workings of the scanner.
    // ---------------------------------------------------------------------
    fn get_char(&mut self) -> Option<(usize, char)> {
        // This function makes sure line and column are always counted correctly. Every possible
        // character that goes through the scanner comes from this function.
        // Possible internal state changes are also done, e.g. when a newline character is found.
        match self.chars.peek() {
            Some((_, ch)) => {
                self.column += 1;
                if &'\n' == ch {
                    self.line += 1;
                    self.column = 0;
                    self.skip_until_newline = false;
                }
                self.chars.next()
            }
            None => None,
        }
    }

    fn handle_comment(&mut self) {
        match self.chars.peek() {
            Some((_, ch)) => {
                match ch {
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
                                        if &'}' == ch {
                                            self.get_char();
                                            depth -= 1;
                                            self.unmatched_multiline_comment_prefixes.pop();
                                        }
                                    }
                                }
                                '{' => {
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
                    _ => {}
                }
            }
            None => {}
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
                Some((mut pos, ch)) => {
                    if ch.is_whitespace() {
                        continue;
                    }

                    token.column = self.column;
                    token.line = self.line;

                    // Most tokens get their type from the map access at the bottom.
                    // This match disambiguates between some situations
                    // Also longer tokens are handled by this match.
                    // These rules/patterns in the match statements are only for the starting
                    // characters. Once an arm is entered, it's not left until the correct
                    // token is finished.
                    match ch {
                        '{' => {
                            self.handle_comment();
                            continue;
                        }
                        '.' => {
                            // End of program or .size. According to the cfg, real literals always
                            // start with at least one digit, so a single dot does not start a real
                            // literal.
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'s' == ch {
                                    // .size
                                    self.get_char();
                                    if let Some((_, ch)) = self.chars.peek() {
                                        if &'i' == ch {
                                            self.get_char();
                                            if let Some((_, ch)) = self.chars.peek() {
                                                if &'z' == ch {
                                                    self.get_char();
                                                    if let Some((_, ch)) = self.chars.peek() {
                                                        if &'e' == ch {
                                                            self.get_char();
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        '<' => {
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'=' == ch || &'>' == ch {
                                    self.get_char();
                                }
                            }
                        }
                        '>' => {
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'=' == ch {
                                    self.get_char();
                                }
                            }
                        }
                        ':' => {
                            if let Some((_, ch)) = self.chars.peek() {
                                if &'=' == ch {
                                    self.get_char();
                                }
                            }
                        }
                        '"' => {
                            token.token_type = TokenType::LiteralString;
                            let mut escape_next = false;

                            // Don't include the starting "-character in the literal.
                            if let Some((p, _)) = self.chars.peek() {
                                pos = *p;
                            }

                            // Run until the next '"' without a preceding '\' is found, or until
                            // the source string runs out, in which case there's obviously a syntax
                            // error. We don't care about that here, the parser will take care of it.
                            while let Some((_, ch)) = self.chars.peek() {
                                let is_finished = &'"' == ch && false == escape_next;
                                escape_next = &'\\' == ch && false == escape_next;
                                self.get_char();

                                if is_finished {
                                    break;
                                }
                            }
                        }
                        'A'..='Z' | 'a'..='z' => {
                            // boolean literals, i.e. "true" and "false" get their type from the
                            // map below and overwrite this type
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
                            let mut scientific = false;
                            let mut signed = false;

                            while let Some((_, ch)) = self.chars.peek() {
                                match ch {
                                    '0'..='9' => {
                                        self.get_char();
                                    }
                                    '.' => {
                                        if TokenType::LiteralInt == token.token_type {
                                            token.token_type = TokenType::LiteralReal;
                                            self.get_char();
                                        }
                                    }
                                    'e' => {
                                        if TokenType::LiteralReal == token.token_type
                                            && false == scientific
                                        {
                                            scientific = true;
                                            self.get_char();

                                            if let Some((_, ch)) = self.chars.peek() {
                                                if false == ch.is_numeric()
                                                    && &'-' != ch
                                                    && &'+' != ch
                                                {
                                                    // Malformed scientific notation
                                                    token.token_type = TokenType::Undefined;
                                                    break;
                                                }
                                            }
                                        } else {
                                            break;
                                        }
                                    }
                                    '-' | '+' => {
                                        if TokenType::LiteralReal == token.token_type
                                            && scientific
                                            && false == signed
                                        {
                                            signed = true;
                                            self.get_char();

                                            if let Some((_, ch)) = self.chars.peek() {
                                                if false == ch.is_numeric() {
                                                    // Malformed scientific notation
                                                    token.token_type = TokenType::Undefined;
                                                    break;
                                                }
                                            }
                                        } else {
                                            break;
                                        }
                                    }
                                    _ => {
                                        break;
                                    }
                                }
                            }
                        }
                        _ => {
                            // The token is something else. If it's a legal token, the map will
                            // give its type. If it's illegal, the token type will be undefined
                            // (with which it was initialized at the start of this function) and
                            // the resulting lexical error will be handled by the parser.
                        }
                    }

                    let mut token_end = match self.chars.peek() {
                        Some((pos, _)) => *pos,
                        None => self.source_str.len(),
                    };

                    // Don't include the ending " with literal strings.
                    if TokenType::LiteralString == token.token_type {
                        token_end = token_end - 1;
                    }
                    token.value = &self.source_str[pos..token_end];

                    // Get your type here, if you're in the map.
                    if let Some(tt) = self.token_map.get(&token.value) {
                        token.token_type = *tt;
                    }
                }
                None => {
                    // Out of characters, repeat the last token
                    token.token_type = TokenType::EOF;
                    token.value = "EOF";
                }
            }

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
            unmatched_multiline_comment_prefixes: vec![],
            chars: source_str.char_indices().peekable(),
            source_str: source_str,
            next_token: TokenData::default(),
            token_queue: VecDeque::new(),
            token_map: [
                ("+", TokenType::OperatorPlus),
                ("-", TokenType::OperatorMinus),
                ("*", TokenType::OperatorMultiply),
                ("/", TokenType::OperatorDivide),
                ("%", TokenType::OperatorModulo),
                ("=", TokenType::OperatorEqual),
                ("<>", TokenType::OperatorNotEqual),
                (">", TokenType::OperatorGreater),
                ("<", TokenType::OperatorLess),
                (">=", TokenType::OperatorGreaterEqual),
                ("<=", TokenType::OperatorLessEqual),
                ("and", TokenType::OperatorAnd),
                ("not", TokenType::OperatorNot),
                ("or", TokenType::OperatorOr),
                (".size", TokenType::OperatorSize),
                ("(", TokenType::LParen),
                (")", TokenType::RParen),
                ("[", TokenType::LBracket),
                ("]", TokenType::RBracket),
                (":=", TokenType::Assignment),
                (",", TokenType::ListSeparator),
                (":", TokenType::TypeSeparator),
                (";", TokenType::StatementSeparator),
                ("if", TokenType::KeywordIf),
                ("then", TokenType::KeywordThen),
                ("else", TokenType::KeywordElse),
                ("of", TokenType::KeywordOf),
                ("while", TokenType::KeywordWhile),
                ("do", TokenType::KeywordDo),
                ("begin", TokenType::KeywordBegin),
                ("end", TokenType::KeywordEnd),
                ("var", TokenType::KeywordVar),
                ("array", TokenType::KeywordArray),
                ("procedure", TokenType::KeywordProcedure),
                ("function", TokenType::KeywordFunction),
                ("program", TokenType::KeywordProgram),
                ("assert", TokenType::KeywordAssert),
                ("return", TokenType::KeywordReturn),
                ("read", TokenType::KeywordRead),
                ("writeln", TokenType::KeywordWrite),
                ("true", TokenType::LiteralBoolean),
                ("false", TokenType::LiteralBoolean),
                ("Boolean", TokenType::Type),
                ("integer", TokenType::Type),
                ("real", TokenType::Type),
                ("string", TokenType::Type),
                (".", TokenType::EndOfProgram),
            ]
            .iter()
            .cloned()
            .collect(),
        };
        // Initialize next_token
        scanner.next_token = scanner.get_token();
        return scanner;
    }
}

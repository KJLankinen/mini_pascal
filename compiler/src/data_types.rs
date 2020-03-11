use serde::Serialize;
use std::fmt;

// ---------------------------------------------------------------------
// Type definitions for enums and auxiliary data types
// ---------------------------------------------------------------------

// ---------------------------------------------------------------------
// Symbols
// ---------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone, Copy, Hash, Serialize, Eq)]
pub enum SymbolType {
    Int,
    String,
    Bool,
    Undefined,
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolType::Int => write!(f, "int"),
            SymbolType::String => write!(f, "string"),
            SymbolType::Bool => write!(f, "bool"),
            SymbolType::Undefined => write!(f, "undefined"),
        }
    }
}

// ---------------------------------------------------------------------
// Errors
// ---------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType<'a> {
    LexicalError(TokenData<'a>),
    SyntaxError(TokenData<'a>, Vec<TokenType>),
    MismatchedTypes(TokenData<'a>, SymbolType, SymbolType),
    UndeclaredIdentifier(TokenData<'a>),
    IllegalOperation(TokenData<'a>, SymbolType),
    UnmatchedComment(u32, u32),
    Redeclaration(TokenData<'a>),
    AssignmentToBlockedVariable(TokenData<'a>),
    ForMismatchedType(
        TokenData<'a>,
        Option<SymbolType>,
        Option<SymbolType>,
        Option<SymbolType>,
    ),
    AssignMismatchedType(TokenData<'a>, SymbolType, SymbolType),
    IOMismatchedType(TokenData<'a>, SymbolType),
    AssertMismatchedType(TokenData<'a>, SymbolType),
}

impl<'a> fmt::Display for ErrorType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorType::LexicalError(_) => write!(f, "lexical error"),
            ErrorType::SyntaxError(_, _) => write!(f, "syntax error"),
            ErrorType::MismatchedTypes(_, _, _) => write!(f, "mismatched types"),
            ErrorType::UndeclaredIdentifier(_) => write!(f, "undeclared identifier"),
            ErrorType::IllegalOperation(_, _) => write!(f, "illegal operation"),
            ErrorType::UnmatchedComment(_, _) => write!(f, "unmatched comment"),
            ErrorType::Redeclaration(_) => write!(f, "redeclaration"),
            ErrorType::AssignmentToBlockedVariable(_) => {
                write!(f, "assignment to a blocked variable")
            }
            ErrorType::ForMismatchedType(_, _, _, _) => write!(f, "mismatched type"),
            ErrorType::AssignMismatchedType(_, _, _) => write!(f, "mismatched type"),
            ErrorType::IOMismatchedType(_, _) => write!(f, "mismatched type"),
            ErrorType::AssertMismatchedType(_, _) => write!(f, "mismatched type"),
        }
    }
}

// ---------------------------------------------------------------------
// Tokens
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
    Type,
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
            TokenType::Type => write!(f, "int, string or bool"),
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

#[derive(Serialize, Debug, Clone, Copy, PartialEq)]
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

// ---------------------------------------------------------------------
// Nodes
// ---------------------------------------------------------------------
#[derive(Serialize, Debug, Clone, Copy, PartialEq)]
pub enum NodeType<'a> {
    Program,
    Operand {
        token: Option<TokenData<'a>>,
        symbol_type: SymbolType,
    },
    Expression {
        operator: Option<TokenData<'a>>,
        symbol_type: SymbolType,
    },
    Declaration {
        identifier: Option<TokenData<'a>>,
        symbol_type: SymbolType,
        expression: Option<usize>,
    },
    Assignment {
        identifier: Option<TokenData<'a>>,
        expression: usize,
    },
    For {
        identifier: Option<TokenData<'a>>,
        start_expression: usize,
        end_expression: usize,
        first_statement: Option<usize>,
    },
    Read {
        identifier: Option<TokenData<'a>>,
    },
    Print {
        token: Option<TokenData<'a>>,
        expression: usize,
    },
    Assert {
        token: Option<TokenData<'a>>,
        expression: usize,
    },
    Undefined,
}

impl<'a> Default for NodeType<'a> {
    fn default() -> Self {
        NodeType::Undefined
    }
}

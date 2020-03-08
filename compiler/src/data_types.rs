use super::lcrs_tree::Update;
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
    SyntaxError(TokenData<'a>, Vec<TokenType>),
    MismatchedTypes(TokenData<'a>, SymbolType, Option<SymbolType>),
    UndeclaredIdentifier(TokenData<'a>),
    IllegalOperation(TokenData<'a>, SymbolType),
    UnmatchedComment(u32, u32),
    Redeclaration(TokenData<'a>),
    Undefined,
}

impl<'a> fmt::Display for ErrorType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorType::SyntaxError(_, _) => write!(f, "syntax error"),
            ErrorType::MismatchedTypes(_, _, _) => write!(f, "mismatched types"),
            ErrorType::UndeclaredIdentifier(_) => write!(f, "undeclared identifier"),
            ErrorType::IllegalOperation(_, _) => write!(f, "illegal operation"),
            ErrorType::UnmatchedComment(_, _) => write!(f, "unmatched comment"),
            ErrorType::Redeclaration(_) => write!(f, "redeclaration"),
            ErrorType::Undefined => write!(f, "undefined"),
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
    Type(SymbolType),
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
            TokenType::Type(s) => s.fmt(f),
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
pub enum NodeType {
    Program,
    Operand,
    Expression,
    Declaration,
    Assignment,
    For,
    Read,
    Print,
    Assert,
}

#[derive(Serialize, Copy, Clone, Debug)]
pub struct NodeData<'a> {
    pub node_type: Option<NodeType>,
    #[serde(flatten)]
    pub token: Option<TokenData<'a>>,
}

impl<'a> Default for NodeData<'a> {
    fn default() -> Self {
        NodeData {
            node_type: None,
            token: None,
        }
    }
}

impl<'a> Update for NodeData<'a> {
    fn update(&mut self, data: Self) {
        if data.node_type.is_some() {
            self.node_type = data.node_type;
        }

        if data.token.is_some() {
            self.token = data.token;
        }
    }
}

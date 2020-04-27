use serde::ser::{SerializeStructVariant, Serializer};
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
    Real,
    ArrayInt,
    ArrayString,
    ArrayBool,
    ArrayReal,
    Undefined,
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolType::Int => write!(f, "int"),
            SymbolType::String => write!(f, "string"),
            SymbolType::Bool => write!(f, "Boolean"),
            SymbolType::Real => write!(f, "real"),
            SymbolType::ArrayInt => write!(f, "array of int"),
            SymbolType::ArrayString => write!(f, "array of string"),
            SymbolType::ArrayBool => write!(f, "array of Boolean"),
            SymbolType::ArrayReal => write!(f, "array of real"),
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
    OperatorPlus,
    OperatorMinus,
    OperatorMultiply,
    OperatorDivide,
    OperatorModulo,
    OperatorEqual,
    OperatorNotEqual,
    OperatorGreater,
    OperatorLess,
    OperatorGreaterEqual,
    OperatorLessEqual,
    OperatorAnd,
    OperatorNot,
    OperatorOr,
    OperatorSize,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Assignment,
    ListSeparator,
    TypeSeparator,
    StatementSeparator,
    Identifier,
    KeywordIf,
    KeywordThen,
    KeywordElse,
    KeywordOf,
    KeywordWhile,
    KeywordDo,
    KeywordBegin,
    KeywordEnd,
    KeywordVar,
    KeywordArray,
    KeywordProcedure,
    KeywordFunction,
    KeywordProgram,
    KeywordAssert,
    KeywordReturn,
    KeywordRead,
    KeywordWrite,
    Type,
    LiteralBoolean,
    LiteralInt,
    LiteralString,
    LiteralReal,
    EndOfProgram,
    Undefined,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::OperatorPlus => write!(f, "+"),
            TokenType::OperatorMinus => write!(f, "-"),
            TokenType::OperatorMultiply => write!(f, "*"),
            TokenType::OperatorDivide => write!(f, "/"),
            TokenType::OperatorModulo => write!(f, "%"),
            TokenType::OperatorEqual => write!(f, "="),
            TokenType::OperatorNotEqual => write!(f, "<>"),
            TokenType::OperatorGreater => write!(f, ">"),
            TokenType::OperatorLess => write!(f, "<"),
            TokenType::OperatorGreaterEqual => write!(f, ">="),
            TokenType::OperatorLessEqual => write!(f, "<="),
            TokenType::OperatorAnd => write!(f, "and"),
            TokenType::OperatorNot => write!(f, "not"),
            TokenType::OperatorOr => write!(f, "or"),
            TokenType::OperatorSize => write!(f, ".size"),
            TokenType::LParen => write!(f, "("),
            TokenType::RParen => write!(f, ")"),
            TokenType::LBracket => write!(f, "["),
            TokenType::RBracket => write!(f, "]"),
            TokenType::Assignment => write!(f, ":="),
            TokenType::ListSeparator => write!(f, ","),
            TokenType::TypeSeparator => write!(f, ":"),
            TokenType::StatementSeparator => write!(f, ";"),
            TokenType::Identifier => write!(f, "an identifier"),
            TokenType::KeywordIf => write!(f, "if"),
            TokenType::KeywordThen => write!(f, "then"),
            TokenType::KeywordElse => write!(f, "else"),
            TokenType::KeywordOf => write!(f, "of"),
            TokenType::KeywordWhile => write!(f, "while"),
            TokenType::KeywordDo => write!(f, "do"),
            TokenType::KeywordBegin => write!(f, "begin"),
            TokenType::KeywordEnd => write!(f, "end"),
            TokenType::KeywordVar => write!(f, "var"),
            TokenType::KeywordArray => write!(f, "array"),
            TokenType::KeywordProcedure => write!(f, "procedure"),
            TokenType::KeywordFunction => write!(f, "function"),
            TokenType::KeywordProgram => write!(f, "program"),
            TokenType::KeywordAssert => write!(f, "assert"),
            TokenType::KeywordReturn => write!(f, "return"),
            TokenType::KeywordRead => write!(f, "read"),
            TokenType::KeywordWrite => write!(f, "writeln"),
            TokenType::Type => write!(f, "Boolean, int, string or real"),
            TokenType::LiteralBoolean => write!(f, "a literal bool"),
            TokenType::LiteralInt => write!(f, "a literal int"),
            TokenType::LiteralString => write!(f, "a literal string"),
            TokenType::LiteralReal => write!(f, "a literal real"),
            TokenType::EndOfProgram => write!(f, "."),
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
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeType<'a> {
    Program {
        id: Option<TokenData<'a>>,
        block_id: usize,
        sub_id: Option<usize>,
    },
    Block {
        first_statement: usize,
    },
    Subroutines {
        first_subroutine: usize,
    },
    Function {
        id: Option<TokenData<'a>>,
        symbol_type: Option<SymbolType>,
        param_id: Option<usize>,
        block_id: usize,
    },
    Parameters {
        first_parameter: usize,
    },
    Operand {
        token: Option<TokenData<'a>>,
        symbol_type: SymbolType,
    },
    Expression {
        operator: Option<TokenData<'a>>,
        symbol_type: SymbolType,
    },
    Declaration {
        id: Option<TokenData<'a>>,
        symbol_type: SymbolType,
    },
    Assignment {
        id: Option<TokenData<'a>>,
        expression: usize,
    },
    While {
        token: Option<TokenData<'a>>,
        expression: usize,
        first_statement: Option<usize>,
    },
    Read {
        token: Option<TokenData<'a>>,
        first_var: usize,
    },
    Write {
        token: Option<TokenData<'a>>,
        first_expression: usize,
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

impl<'a> From<NodeType<'a>> for u32 {
    fn from(nt: NodeType) -> Self {
        match nt {
            NodeType::Program {
                id: _,
                block_id: _,
                sub_id: _,
            } => 0,
            NodeType::Block { first_statement: _ } => 1,
            NodeType::Subroutines {
                first_subroutine: _,
            } => 2,
            NodeType::Function {
                id: _,
                symbol_type: _,
                param_id: _,
                block_id: _,
            } => 3,
            NodeType::Parameters { first_parameter: _ } => 4,
            NodeType::Operand {
                token: _,
                symbol_type: _,
            } => 5,
            NodeType::Expression {
                operator: _,
                symbol_type: _,
            } => 6,
            NodeType::Declaration {
                id: _,
                symbol_type: _,
            } => 7,
            NodeType::Assignment {
                id: _,
                expression: _,
            } => 8,
            NodeType::While {
                token: _,
                expression: _,
                first_statement: _,
            } => 9,
            NodeType::Read {
                token: _,
                first_var: _,
            } => 10,
            NodeType::Write {
                token: _,
                first_expression: _,
            } => 11,
            NodeType::Assert {
                token: _,
                expression: _,
            } => 12,
            NodeType::Undefined => 13,
        }
    }
}

impl<'a> Serialize for NodeType<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            NodeType::Program {
                ref id,
                block_id: _,
                sub_id: _,
            } => {
                let id = id.unwrap();
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Program",
                    1,
                )?;
                state.serialize_field("id", id.value)?;
                state.end()
            }
            NodeType::Block { first_statement: _ } => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Block",
                    0,
                )?;
                state.end()
            }
            NodeType::Subroutines {
                first_subroutine: _,
            } => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Subroutines",
                    0,
                )?;
                state.end()
            }
            NodeType::Function {
                ref id,
                ref symbol_type,
                param_id: _,
                block_id: _,
            } => {
                let id = id.unwrap();
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Function",
                    2,
                )?;
                state.serialize_field("id", id.value)?;
                state.serialize_field("type", symbol_type)?;
                state.end()
            }
            NodeType::Parameters { first_parameter: _ } => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Parameters",
                    0,
                )?;
                state.end()
            }
            NodeType::Operand {
                ref token,
                symbol_type: _,
            } => {
                let token = token.unwrap();
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Operand",
                    1,
                )?;
                state.serialize_field("opnd", token.value)?;
                state.end()
            }
            NodeType::Expression {
                ref operator,
                symbol_type: _,
            } => {
                let operator = operator.unwrap();
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Expression",
                    1,
                )?;
                state.serialize_field("op", operator.value)?;
                state.end()
            }
            NodeType::Declaration {
                ref id,
                symbol_type: _,
            } => {
                let identifier = id.unwrap();
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Declaration",
                    1,
                )?;
                state.serialize_field("identifier", identifier.value)?;
                state.end()
            }
            NodeType::Assignment {
                ref id,
                expression: _,
            } => {
                let identifier = id.unwrap();
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Assignment",
                    1,
                )?;
                state.serialize_field("identifier", identifier.value)?;
                state.end()
            }
            NodeType::Read {
                ref token,
                first_var: _,
            } => {
                let identifier = token.unwrap();
                let mut state =
                    serializer.serialize_struct_variant("NodeType", u32::from(*self), "Read", 1)?;
                state.serialize_field("identifier", identifier.value)?;
                state.end()
            }
            NodeType::Assert {
                token: _,
                expression: _,
            } => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Assert",
                    0,
                )?;
                state.end()
            }
            NodeType::Undefined => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Undefined",
                    0,
                )?;
                state.end()
            }
        }
    }
}

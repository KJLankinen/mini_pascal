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
    Bool,
    Int,
    Real,
    String,
    ArrayBool(usize),
    ArrayInt(usize),
    ArrayReal(usize),
    ArrayString(usize),
    Undefined,
}

impl fmt::Display for SymbolType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SymbolType::Int => write!(f, "integer"),
            SymbolType::String => write!(f, "string"),
            SymbolType::Bool => write!(f, "Boolean"),
            SymbolType::Real => write!(f, "real"),
            SymbolType::ArrayInt(_) => write!(f, "array of int"),
            SymbolType::ArrayString(_) => write!(f, "array of string"),
            SymbolType::ArrayBool(_) => write!(f, "array of Boolean"),
            SymbolType::ArrayReal(_) => write!(f, "array of real"),
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
    UndeclaredIdentifier(TokenData<'a>),
    IllegalOperation(TokenData<'a>, Vec<SymbolType>),
    UnmatchedComment(u32, u32),
    Redeclaration(TokenData<'a>),
    AssignMismatchedType(TokenData<'a>, SymbolType, SymbolType),
    AssertMismatchedType(TokenData<'a>, SymbolType),
    IndexTypeMismatch(TokenData<'a>, SymbolType),
    IllegalIndexing(TokenData<'a>, SymbolType),
    TooFewArguments(
        TokenData<'a>,
        Vec<SymbolType>,
        TokenData<'a>,
        Vec<SymbolType>,
    ),
    TooManyArguments(
        TokenData<'a>,
        Vec<SymbolType>,
        TokenData<'a>,
        Vec<SymbolType>,
    ),
    MismatchedArgumentTypes(
        TokenData<'a>,
        Vec<SymbolType>,
        TokenData<'a>,
        Vec<SymbolType>,
    ),
    MismatchedReturnType(TokenData<'a>, Option<SymbolType>, Option<SymbolType>),
    ReadMismatchedType(TokenData<'a>, SymbolType),
    ExprTypeMismatch(TokenData<'a>, SymbolType, SymbolType),
    MissingReturnStatements(TokenData<'a>),
    ArraySizeTypeMismatch(TokenData<'a>, SymbolType),
}

impl<'a> fmt::Display for ErrorType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorType::LexicalError(_) => write!(f, "lexical error"),
            ErrorType::SyntaxError(_, _) => write!(f, "syntax error"),
            ErrorType::UndeclaredIdentifier(_) => write!(f, "undeclared identifier"),
            ErrorType::IllegalOperation(_, _) => write!(f, "illegal operation"),
            ErrorType::UnmatchedComment(_, _) => write!(f, "unmatched comment"),
            ErrorType::Redeclaration(_) => write!(f, "redeclaration"),
            ErrorType::AssignMismatchedType(_, _, _) => write!(f, "mismatched type"),
            ErrorType::AssertMismatchedType(_, _) => write!(f, "mismatched type"),
            ErrorType::IndexTypeMismatch(_, _) => write!(f, "mismatched type"),
            ErrorType::IllegalIndexing(_, _) => write!(f, "illegal indexing"),
            ErrorType::TooFewArguments(_, _, _, _) => write!(f, "too few arguments"),
            ErrorType::TooManyArguments(_, _, _, _) => write!(f, "too many arguments"),
            ErrorType::MismatchedArgumentTypes(_, _, _, _) => {
                write!(f, "mismatched argument types")
            }
            ErrorType::MismatchedReturnType(_, _, _) => write!(f, "mismatched return type"),
            ErrorType::ReadMismatchedType(_, _) => write!(f, "mismatched read type"),
            ErrorType::ExprTypeMismatch(_, _, _) => write!(f, "mismatched expression type"),
            ErrorType::MissingReturnStatements(_) => write!(f, "missing return statements"),
            ErrorType::ArraySizeTypeMismatch(_, _) => {
                write!(f, "array size operator type mismatch")
            }
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
    LiteralBool,
    LiteralInt,
    LiteralString,
    LiteralReal,
    EndOfProgram,
    EOF,
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
            TokenType::Type => write!(f, "Boolean, integer, string or real"),
            TokenType::LiteralBool => write!(f, "a literal bool"),
            TokenType::LiteralInt => write!(f, "a literal int"),
            TokenType::LiteralString => write!(f, "a literal string"),
            TokenType::LiteralReal => write!(f, "a literal real"),
            TokenType::EndOfProgram => write!(f, "."),
            TokenType::EOF => write!(f, "EOF"),
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

pub const EMPTY_TOKEN: TokenData<'static> = TokenData {
    column: !0,
    line: !0,
    token_type: TokenType::Undefined,
    value: "Empty Token",
};

// ---------------------------------------------------------------------
// Nodes
// ---------------------------------------------------------------------
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenIdxOptIdx<'a> {
    pub token: Option<TokenData<'a>>,
    pub idx: usize,
    pub opt_idx: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenIdxOptIdxOptIdx<'a> {
    pub token: Option<TokenData<'a>>,
    pub idx: usize,
    pub opt_idx: Option<usize>,
    pub opt_idx2: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenIdxIdxOptIdx<'a> {
    pub token: Option<TokenData<'a>>,
    pub idx: usize,
    pub idx2: usize,
    pub opt_idx: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenIdx<'a> {
    pub token: Option<TokenData<'a>>,
    pub idx: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenOptIdx<'a> {
    pub token: Option<TokenData<'a>>,
    pub opt_idx: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenIdxIdx<'a> {
    pub token: Option<TokenData<'a>>,
    pub idx: usize,
    pub idx2: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenIdxBool<'a> {
    pub token: Option<TokenData<'a>>,
    pub idx: usize,
    pub b: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct IdxIdx {
    pub idx: usize,
    pub idx2: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenSymbolType<'a> {
    pub token: Option<TokenData<'a>>,
    pub st: SymbolType,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenSymbolIdxOptIdx<'a> {
    pub token: Option<TokenData<'a>>,
    pub st: SymbolType,
    pub idx: usize,
    pub opt_idx: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VariableData<'a> {
    pub token: Option<TokenData<'a>>,
    pub st: SymbolType,
    pub is_ref: bool,
    pub count: usize,
    pub depth: i32,
    pub array_idx: Option<usize>,
    pub string_idx: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokenSymbolIdxIdx<'a> {
    pub token: Option<TokenData<'a>>,
    pub st: SymbolType,
    pub idx: usize,
    pub idx2: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NodeType<'a> {
    Undefined,
    Program(TokenIdxOptIdx<'a>),
    Block(usize),
    Subroutines(usize),
    Function(TokenIdxOptIdxOptIdx<'a>),
    ParamList(usize),
    Parameter(TokenIdxBool<'a>),
    VariableType(TokenSymbolType<'a>),
    Assert(TokenIdxOptIdx<'a>),
    Assignment(IdxIdx),
    Call(TokenOptIdx<'a>),
    Declaration(IdxIdx),
    Return(TokenOptIdx<'a>),
    Read(usize),
    Write(TokenIdx<'a>),
    If(TokenIdxIdxOptIdx<'a>),
    While(TokenIdxIdx<'a>),
    RelOp(TokenSymbolIdxIdx<'a>),
    AddOp(TokenSymbolIdxOptIdx<'a>),
    MulOp(TokenSymbolIdxIdx<'a>),
    Variable(VariableData<'a>),
    Literal(TokenOptIdx<'a>),
    Not(TokenIdx<'a>),
    ArraySize(TokenIdx<'a>),
}

impl<'a> Default for NodeType<'a> {
    fn default() -> Self {
        NodeType::Undefined
    }
}

impl<'a> From<NodeType<'a>> for u32 {
    fn from(nt: NodeType) -> Self {
        match nt {
            NodeType::Undefined => 0,
            NodeType::Program(_) => 1,
            NodeType::Block(_) => 2,
            NodeType::Subroutines(_) => 3,
            NodeType::Function(_) => 4,
            NodeType::ParamList(_) => 5,
            NodeType::Parameter(_) => 6,
            NodeType::VariableType(_) => 7,
            NodeType::Assert(_) => 9,
            NodeType::Assignment(_) => 10,
            NodeType::Call(_) => 11,
            NodeType::Declaration(_) => 12,
            NodeType::Return(_) => 13,
            NodeType::Read(_) => 14,
            NodeType::Write(_) => 15,
            NodeType::If(_) => 16,
            NodeType::While(_) => 17,
            NodeType::RelOp(_) => 18,
            NodeType::AddOp(_) => 19,
            NodeType::MulOp(_) => 20,
            NodeType::Variable(_) => 21,
            NodeType::Literal(_) => 22,
            NodeType::Not(_) => 23,
            NodeType::ArraySize(_) => 24,
        }
    }
}

impl<'a> Serialize for NodeType<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match *self {
            NodeType::Undefined => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Undefined",
                    0,
                )?;
                state.end()
            }
            NodeType::Program(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Program",
                    1,
                )?;
                state.serialize_field("id", &data.token.expect("Program token is none.").value)?;
                state.end()
            }
            NodeType::Block(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Block",
                    0,
                )?;
                state.end()
            }
            NodeType::Subroutines(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Subroutines",
                    0,
                )?;
                state.end()
            }
            NodeType::Function(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Function",
                    1,
                )?;
                state.serialize_field("id", data.token.expect("Function token is none.").value)?;
                state.end()
            }
            NodeType::ParamList(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Parameter list",
                    0,
                )?;
                state.end()
            }
            NodeType::Parameter(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Parameter",
                    2,
                )?;

                state
                    .serialize_field("id", &data.token.expect("Parameter token is none.").value)?;
                state.serialize_field("is reference", &data.b)?;
                state.end()
            }
            NodeType::VariableType(data) => {
                let mut state =
                    serializer.serialize_struct_variant("NodeType", u32::from(*self), "Type", 1)?;
                state.serialize_field("type", &data.st)?;
                state.end()
            }
            NodeType::Assert(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Assert",
                    0,
                )?;
                state.end()
            }
            NodeType::Assignment(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Assignment",
                    0,
                )?;
                state.end()
            }
            NodeType::Call(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Function call",
                    1,
                )?;
                state.serialize_field(
                    "identifier",
                    &data.token.expect("Call token is none.").value,
                )?;
                state.end()
            }
            NodeType::Declaration(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Declaration",
                    0,
                )?;
                state.end()
            }
            NodeType::Return(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Return",
                    0,
                )?;
                state.end()
            }
            NodeType::Read(_) => {
                let state =
                    serializer.serialize_struct_variant("NodeType", u32::from(*self), "Read", 0)?;
                state.end()
            }
            NodeType::Write(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Write",
                    0,
                )?;
                state.end()
            }
            NodeType::If(_) => {
                let state =
                    serializer.serialize_struct_variant("NodeType", u32::from(*self), "If", 0)?;
                state.end()
            }
            NodeType::While(_) => {
                let state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "While",
                    0,
                )?;
                state.end()
            }
            NodeType::RelOp(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Relation operator",
                    1,
                )?;
                state.serialize_field(
                    "operator",
                    &data.token.expect("Expression token is none.").value,
                )?;
                state.end()
            }
            NodeType::AddOp(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Add operation",
                    1,
                )?;
                state.serialize_field(
                    "operator",
                    &data.token.unwrap_or_else(|| EMPTY_TOKEN).value,
                )?;
                state.end()
            }
            NodeType::MulOp(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Multiply operation",
                    1,
                )?;
                state.serialize_field(
                    "operator",
                    &data.token.unwrap_or_else(|| EMPTY_TOKEN).value,
                )?;
                state.end()
            }
            NodeType::Variable(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Variable",
                    1,
                )?;
                state.serialize_field(
                    "identifier",
                    &data.token.expect("Variable token is none.").value,
                )?;
                state.end()
            }
            NodeType::Literal(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Literal",
                    1,
                )?;
                state
                    .serialize_field("value", &data.token.expect("Literal token is none.").value)?;
                state.end()
            }
            NodeType::Not(data) => {
                let mut state =
                    serializer.serialize_struct_variant("NodeType", u32::from(*self), "Not", 1)?;
                state
                    .serialize_field("operator", &data.token.expect("Not token is none.").value)?;
                state.end()
            }
            NodeType::ArraySize(data) => {
                let mut state = serializer.serialize_struct_variant(
                    "NodeType",
                    u32::from(*self),
                    "Array size",
                    1,
                )?;
                state.serialize_field("operator", &data.token.expect("Token is none.").value)?;
                state.end()
            }
        }
    }
}

// ---------------------------------------------------------------------
// Function signature & parameter
// ---------------------------------------------------------------------
#[derive(Debug, Clone)]
pub struct Parameter<'a> {
    pub is_ref: bool,
    pub symbol_type: SymbolType,
    pub id: &'a str,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature<'a> {
    pub parameters: Vec<Parameter<'a>>,
    pub return_type: Option<SymbolType>,
    pub token: TokenData<'a>,
}

// ---------------------------------------------------------------------
// Wasm types and Wasm instructions
// ---------------------------------------------------------------------
#[derive(Debug, Copy, Clone)]
pub enum WasmType<'a> {
    I32(Option<i32>),
    F32(Option<f32>),
    Str(&'a str),
}

impl<'a> WasmType<'a> {
    pub fn as_str(&self) -> &'static str {
        match self {
            WasmType::I32(_) => "i32",
            WasmType::F32(_) => "f32",
            WasmType::Str(_) => "i32",
        }
    }
}

impl<'a> fmt::Display for WasmType<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug)]
pub enum Instruction<'a> {
    ProgramBegin(&'a str),
    FunctionBegin(&'a str),
    BlockBegin(Option<&'a str>),
    LoopBegin(Option<&'a str>),
    End,
    DataSegment,
    Imports,
    GlobalPointers,
    Param(WasmType<'a>),
    Result(WasmType<'a>),
    Local(Option<&'a str>, WasmType<'a>),
    GetLocal(WasmType<'a>),
    SetLocal(WasmType<'a>),
    MemLoad(WasmType<'a>),
    MemStore(WasmType<'a>),
    Const(WasmType<'a>),
    Call(WasmType<'a>),
    LibFunc(&'a str),
    Eqz,
    Br(WasmType<'a>),
    BrIf(WasmType<'a>),
    Unreachable,
    Drop,
    Eq(WasmType<'a>),
    NEq(WasmType<'a>),
    GreatEq(WasmType<'a>),
    Great(WasmType<'a>),
    LessEq(WasmType<'a>),
    Less(WasmType<'a>),
    Add(WasmType<'a>),
    Sub(WasmType<'a>),
    Mul(WasmType<'a>),
    Div(WasmType<'a>),
    Mod(WasmType<'a>),
    And(WasmType<'a>),
    Or(WasmType<'a>),
}

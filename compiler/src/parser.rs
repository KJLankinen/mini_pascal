use super::scanner::Scanner;
use super::scanner::TokenData;
use super::scanner::TokenType;

#[derive(Debug)]
enum NodeType {
    Program,
    EndOfProgram,
    StatementList,
    Statement,
    StatementPrefix,
    EndOfStatement,
    Expression,
    Operand,
    Operator,
    UnOperator,
    Type,
    Identifier,
    LiteralInt,
    LiteralString,
    LiteralBool,
    Paren,
    Keyword,
    Assignment,
    TypeSeparator,
    Range,
    Unexpected,
    NumTypes,
}

#[derive(Debug)]
struct ParseNode<'a> {
    parent: usize,
    left_child: usize,
    right_sibling: usize,
    column: u32,
    line: u32,
    node_type: NodeType,
    value: &'a str,
}

impl<'a> ParseNode<'a> {
    fn new(
        parent: usize,
        left_child: usize,
        right_sibling: usize,
        column: u32,
        line: u32,
        node_type: NodeType,
        value: &'a str,
    ) -> ParseNode<'a> {
        ParseNode {
            parent: parent,
            left_child: left_child,
            right_sibling: right_sibling,
            column: column,
            line: line,
            node_type: node_type,
            value: value,
        }
    }
}

impl<'a> Default for ParseNode<'a> {
    fn default() -> ParseNode<'a> {
        ParseNode::new(!0, !0, !0, 0, 0, NodeType::Unexpected, "")
    }
}

pub struct Parser<'a> {
    scanner: Scanner<'a>,
    parse_tree: Vec<ParseNode<'a>>,
    unexpected_nodes: Vec<usize>,
}

// A recursive descent parser
impl<'a> Parser<'a> {
    pub fn new(source_str: &'a str) -> Parser<'a> {
        Parser {
            scanner: Scanner::new(source_str),
            parse_tree: vec![],
            unexpected_nodes: vec![],
        }
    }
}

pub fn parse<'a>(parser: &mut Parser<'a>) {
    process_program(parser);
    print_unexpected(parser);
}

fn print_unexpected<'a>(parser: &mut Parser<'a>) {
    for &item in &parser.unexpected_nodes {
        println!("{:#?}", parser.parse_tree[item]);
    }
}

fn add_node<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) -> usize {
    let next_of_prev: &mut usize;
    let id = parser.parse_tree.len();
    parser.parse_tree.push(ParseNode::default());
    parser.parse_tree[id].parent = parent;
    let previous_node = &mut parser.parse_tree[previous];

    // This node is the first child of the parent node
    if previous == parent {
        next_of_prev = &mut previous_node.left_child;
    } else {
        next_of_prev = &mut previous_node.right_sibling;
    }

    *next_of_prev = id;
    id
}

fn verify_token<'a>(
    parser: &mut Parser<'a>,
    expected: bool,
    token: &TokenData<'a>,
    node_type: NodeType,
    parent: usize,
    previous: usize,
) -> usize {
    let me = add_node(parser, parent, previous);
    let node = &mut parser.parse_tree[me];
    node.column = token.column;
    node.line = token.line;
    node.value = token.value;

    if expected {
        node.node_type = node_type;
    } else {
        parser.unexpected_nodes.push(me);
    }

    me
}

fn process_program<'a>(parser: &mut Parser<'a>) {
    let me = verify_token(parser, true, &TokenData::default(), NodeType::Program, 0, 0);

    let previous = process_statement_list(parser, me, me);
    process_end_of_program(parser, me, previous);
}

fn process_end_of_program<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) {
    let token = parser.scanner.next().copied().unwrap();
    verify_token(
        parser,
        TokenType::EndOfProgram == token.token_type,
        &token,
        NodeType::EndOfProgram,
        parent,
        previous,
    );
}

fn process_statement_list<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) -> usize {
    let me = verify_token(
        parser,
        true,
        &TokenData::default(),
        NodeType::StatementList,
        parent,
        previous,
    );

    // Stop if we've reached end of program or the end of 'for' block
    let token_type = parser.scanner.peek().unwrap().token_type;
    if TokenType::EndOfProgram != token_type && TokenType::KeywordEnd != token_type {
        let previous = process_statement(parser, me, me);
        process_statement_list(parser, me, previous);
    }

    me
}

fn process_statement<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) -> usize {
    let token = parser.scanner.peek().copied().unwrap();
    let me = verify_token(parser, true, &token, NodeType::Statement, parent, previous);
    let previous = process_statement_prefix(parser, me, me);
    process_end_of_statement(parser, me, previous);

    me
}

fn process_statement_prefix<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) -> usize {
    let token = parser.scanner.next().copied().unwrap();
    let me = verify_token(
        parser,
        true,
        &token,
        NodeType::StatementPrefix,
        parent,
        previous,
    );

    match token.token_type {
        TokenType::KeywordVar => {
            // 'var'
            let previous = verify_token(
                parser,
                TokenType::KeywordVar == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                me,
            );

            // identifier
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                TokenType::Identifier == token.token_type,
                &token,
                NodeType::Identifier,
                me,
                previous,
            );

            // ':'
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                TokenType::TypeSeparator == token.token_type,
                &token,
                NodeType::TypeSeparator,
                me,
                previous,
            );

            // type
            let previous = process_type(parser, me, previous);

            // The statement can be longer
            if TokenType::Assignment
                == parser
                    .scanner
                    .peek()
                    .expect("Next token is None")
                    .token_type
            {
                // ':='
                let token = parser.scanner.next().copied().unwrap();
                let previous = verify_token(
                    parser,
                    TokenType::Assignment == token.token_type,
                    &token,
                    NodeType::Assignment,
                    me,
                    previous,
                );

                // expr
                process_expression(parser, me, previous);
            }
        }
        TokenType::KeywordFor => {
            // 'for'
            let previous = verify_token(
                parser,
                TokenType::KeywordFor == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                me,
            );

            // identifier
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                TokenType::Identifier == token.token_type,
                &token,
                NodeType::Identifier,
                me,
                previous,
            );

            // 'in'
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                TokenType::KeywordIn == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                previous,
            );

            // expr
            let previous = process_expression(parser, me, previous);

            // '..'
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                TokenType::Range == token.token_type,
                &token,
                NodeType::Range,
                me,
                previous,
            );

            // expr
            let previous = process_expression(parser, me, previous);

            // 'do'
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                TokenType::KeywordDo == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                previous,
            );

            // statement list
            let previous = process_statement_list(parser, me, previous);

            // 'end'
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                TokenType::KeywordEnd == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                previous,
            );

            // 'for'
            let token = parser.scanner.next().copied().unwrap();
            verify_token(
                parser,
                TokenType::KeywordFor == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                previous,
            );
        }
        TokenType::KeywordRead => {
            // 'read'
            let previous = verify_token(
                parser,
                TokenType::KeywordRead == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                me,
            );

            // identifier
            let token = parser.scanner.next().copied().unwrap();
            verify_token(
                parser,
                TokenType::Identifier == token.token_type,
                &token,
                NodeType::Identifier,
                me,
                previous,
            );
        }
        TokenType::KeywordPrint => {
            // 'print'
            let previous = verify_token(
                parser,
                TokenType::KeywordPrint == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                me,
            );

            // expr
            process_expression(parser, me, previous);
        }
        TokenType::KeywordAssert => {
            // 'assert'
            let previous = verify_token(
                parser,
                TokenType::KeywordAssert == token.token_type,
                &token,
                NodeType::Keyword,
                me,
                me,
            );

            // '('
            let token = parser.scanner.next().copied().unwrap();
            verify_token(
                parser,
                TokenType::LParen == token.token_type,
                &token,
                NodeType::Paren,
                me,
                previous,
            );

            // expr
            let previous = process_expression(parser, me, previous);

            // ')'
            let token = parser.scanner.next().copied().unwrap();
            verify_token(
                parser,
                TokenType::RParen == token.token_type,
                &token,
                NodeType::Paren,
                me,
                previous,
            );
        }
        TokenType::Identifier => {
            // identifier
            let previous = verify_token(
                parser,
                TokenType::Identifier == token.token_type,
                &token,
                NodeType::Identifier,
                me,
                me,
            );

            // ':='
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                TokenType::Assignment == token.token_type,
                &token,
                NodeType::Assignment,
                me,
                previous,
            );

            // expr
            process_expression(parser, me, previous);
        }
        _ => {
            verify_token(parser, false, &token, NodeType::Unexpected, me, me);
        }
    }

    me
}

fn process_end_of_statement<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) -> usize {
    let token = parser.scanner.next().copied().unwrap();
    verify_token(
        parser,
        TokenType::EndOfStatement == token.token_type,
        &token,
        NodeType::EndOfStatement,
        parent,
        previous,
    )
}

fn process_type<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) -> usize {
    let token = parser.scanner.next().copied().unwrap();
    let expected = TokenType::TypeInt == token.token_type
        || TokenType::TypeBool == token.token_type
        || TokenType::TypeString == token.token_type;

    verify_token(parser, expected, &token, NodeType::Type, parent, previous)
}

fn process_expression<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) -> usize {
    let me = verify_token(
        parser,
        true,
        &TokenData::default(),
        NodeType::Expression,
        parent,
        previous,
    );

    // Starts with unary op
    if TokenType::OperatorNot
        == parser
            .scanner
            .peek()
            .expect("Next token is None")
            .token_type
    {
        let token = parser.scanner.next().copied().unwrap();
        let previous = verify_token(
            parser,
            TokenType::OperatorNot == token.token_type,
            &token,
            NodeType::UnOperator,
            me,
            me,
        );
        process_operand(parser, me, previous);
    } else {
        // Either just a single operand or operand operator operand
        let previous = process_operand(parser, me, me);
        let mut is_operator = true;
        match parser
            .scanner
            .peek()
            .expect("Next token is None")
            .token_type
        {
            TokenType::OperatorPlus => {}
            TokenType::OperatorMinus => {}
            TokenType::OperatorMultiply => {}
            TokenType::OperatorDivide => {}
            TokenType::OperatorLessThan => {}
            TokenType::OperatorEqual => {}
            TokenType::OperatorAnd => {}
            _ => is_operator = false, // Not any operator
        }

        if is_operator {
            let token = parser.scanner.next().copied().unwrap();
            let previous = verify_token(
                parser,
                is_operator,
                &token,
                NodeType::Operator,
                me,
                previous,
            );
            process_operand(parser, me, previous);
        }
    }

    me
}

fn process_operand<'a>(parser: &mut Parser<'a>, parent: usize, previous: usize) -> usize {
    let token = parser.scanner.next().copied().unwrap();
    let me = verify_token(parser, true, &token, NodeType::Operand, parent, previous);

    // Operand is '(' <expr> ')'
    if TokenType::LParen == token.token_type {
        let previous = verify_token(
            parser,
            TokenType::LParen == token.token_type,
            &token,
            NodeType::Paren,
            me,
            me,
        );

        let previous = process_expression(parser, me, previous);

        let token = parser.scanner.next().copied().unwrap();
        verify_token(
            parser,
            TokenType::RParen == token.token_type,
            &token,
            NodeType::Paren,
            me,
            previous,
        );
    } else {
        // Operand is a literal or identifier
        let mut node_type: NodeType = NodeType::Unexpected;
        let mut expected = true;
        match token.token_type {
            TokenType::LiteralInt => {
                node_type = NodeType::LiteralInt;
            }
            TokenType::LiteralString => {
                node_type = NodeType::LiteralString;
            }
            TokenType::LiteralBool => {
                node_type = NodeType::LiteralBool;
            }
            TokenType::Identifier => {
                node_type = NodeType::Identifier;
            }
            _ => {
                expected = false;
            }
        }
        verify_token(parser, expected, &token, node_type, me, me);
    }

    me
}

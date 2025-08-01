use nom::{
    branch::alt, bytes::complete::tag, character::complete::{
        alpha1, alphanumeric1, char, digit1, multispace0, none_of, space1
    }, 
    combinator::{all_consuming, map, opt, recognize, value}, 
    multi::{fold_many0, many0, separated_list0}, 
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult
};

use crate::{
    error::{CompilerError, ErrorCode, Span}, 
    parser::ast::{AssignOp, BinaryOp, Expr, RangeKind, Stmt, UnaryOp}
};

pub type Program = Vec<Stmt>;

// Whitespace handling
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(multispace0, inner, multispace0)
}

// ----------------------------------

fn identifier(input: &str) -> IResult<&str, String> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: &str| s.to_string(),
    )(input)
}

fn keyword(kw: &'static str) -> impl Fn(&str) -> IResult<&str, ()> {
    move |input: &str| {
        value((), terminated(tag(kw), alt((space1, tag("("), tag("{")))))(input)
    }
}

fn tuple_expr(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(
            char('('),
            separated_list0(ws(char(',')), ws(assignment)),
            char(')')
        ),
        |elements| {
            if elements.len() == 1 {
                // Single element in parentheses is just a grouped expression
                elements.into_iter().next().unwrap()
            } else {
                Expr::Tuple(elements)
            }
        }
    )(input)
}

fn array(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(
            char('['),
            separated_list0(ws(char(',')), ws(assignment)),
            char(']')
        ),
        |elements| Expr::Array(elements)
    )(input)
}

fn if_let_expr(input: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            keyword("if"),
            keyword("let"),
            ws(identifier), // pattern
            ws(char('=')),
            ws(assignment), // expression to match
            ws(block),      // then block
            opt(preceded(keyword("else"), ws(block))) // optional else block
        )),
        |(_, _, pattern, _, expr, then_block, else_block)| {
            Expr::IfLet {
                pattern,
                expr: Box::new(expr),
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new)
            }
        }
    )(input)
}

fn if_expr(input: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            keyword("if"),
            ws(assignment), // condition
            ws(block),      // then block
            opt(preceded(keyword("else"), ws(alt((if_expr, block))))) // optional else
        )),
        |(_, condition, then_block, else_block)| {
            Expr::If {
                condition: Box::new(condition),
                then_block: Box::new(then_block),
                else_block: else_block.map(Box::new)
            }
        }
    )(input)
}

fn block(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(
            char('{'),
            many0(ws(statement)),
            char('}')
        ),
        |statements| Expr::Block(statements)
    )(input)
}

fn boolean(input: &str) -> IResult<&str, Expr> {
    alt((
        value(Expr::Boolean(true), tag("true")),
        value(Expr::Boolean(false), tag("false"))
    ))(input)
}

fn string(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(
            char('"'),
            many0(alt((
                map(char('\\'), |_| '\\'), // escaped backslash
                map(preceded(char('\\'), char('"')), |_| '"'), // escaped quote
                none_of("\"\\")
            ))),
            char('"')
        ),
        |chars| Expr::String(chars.into_iter().collect())
    )(input)
}

fn number(input: &str) -> IResult<&str, Expr> {
    alt((
        // Float
        map(
            recognize(tuple((
                opt(char('-')),
                digit1,
                char('.'),
                digit1
            ))),
            |s: &str| Expr::Float(s.parse().unwrap())
        ),
        // Integer
        map(
            recognize(tuple((
                opt(char('-')),
                digit1
            ))),
            |s: &str| Expr::Integer(s.parse().unwrap())
        )
    ))(input)
}

fn constants(input: &str) -> IResult<&str, Expr> {
    alt((
        boolean,
        number,
        string,
        map(identifier, |name| Expr::Identifier(name))
    ))(input)
}

fn primary(input: &str) -> IResult<&str, Expr> {
    alt((
        if_let_expr,
        if_expr,
        block,
        tuple_expr,
        array,
        constants
    ))(input)
}

fn postfix(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(primary)(input)?;
    
    fold_many0(
        alt((
            // Array/tuple indexing
            map(delimited(char('['), ws(assignment), char(']')), |index| {
                move |expr: Expr| Expr::Index {
                    object: Box::new(expr),
                    index: Box::new(index.clone())
                }
            }),
            /*
            // Function call
            map(delimited(char('('), separated_list0(ws(char(',')), ws(assignment)), char(')')), |args| {
                move |expr: Expr| Expr::Call {
                    function: Box::new(expr.clone()),
                    args: args.clone()
                }
            }),
            // Field access
            map(preceded(char('.'), identifier), |field| {
                move |expr: Expr| Expr::FieldAccess {
                    object: Box::new(expr.clone()),
                    field: field.clone()
                }
            })
            */
        )),
        move || init.clone(),
        |expr, f| f(expr)
    )(input)
}

fn unary(input: &str) -> IResult<&str, Expr> {
    alt((
        map(preceded(char('!'), ws(unary)), |expr| {
            Expr::Unary {
                op: UnaryOp::Not,
                operand: Box::new(expr)
            }
        }),
        map(preceded(char('-'), ws(unary)), |expr| {
            Expr::Unary {
                op: UnaryOp::Negate,
                operand: Box::new(expr)
            }
        }),
        postfix
    ))(input)
}

fn multiplicative(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(unary)(input)?;

    fold_many0(
        tuple((
            ws(alt((
                value(BinaryOp::Multiply, char('*')),
                value(BinaryOp::Divide, char('/')),
                value(BinaryOp::Modulo, char('%'))
            ))),
            ws(unary)
        )),
        move || init.clone(),
        |left, (op, right)| Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn additive(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(multiplicative)(input)?;

    fold_many0(
        tuple((
            ws(alt((
                value(BinaryOp::Add, char('+')),
                value(BinaryOp::Subtract, char('-'))
            ))),
            ws(multiplicative)
        )),
        move || init.clone(),
        |left, (op, right)| Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn shift(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(additive)(input)?;

    fold_many0(
        tuple((
            ws(alt((
                value(BinaryOp::LeftShift, tag("<<")),
                value(BinaryOp::RightShift, tag(">>")),
            ))),
            ws(additive)
        )),
        move || init.clone(),
        |left, (op, right)| Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn relational(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(shift)(input)?;

    fold_many0(
        tuple((
            ws(alt((
                value(BinaryOp::Leq, tag("<=")),
                value(BinaryOp::Geq, tag(">=")),
                value(BinaryOp::Lt, char('<')),
                value(BinaryOp::Gt, char('>')),
            ))),
            ws(shift)
        )),
        move || init.clone(),
        |left, (op, right)| Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn equality(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(relational)(input)?;

    fold_many0(
        tuple((
            ws(alt((
                value(BinaryOp::Eq, tag("==")),
                value(BinaryOp::Neq, tag("!=")),
            ))),
            ws(relational)
        )),
        move || init.clone(),
        |left, (op, right)| Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn bitwise_and(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(equality)(input)?;

    fold_many0(
        preceded(ws(char('&')), ws(equality)),
        move || init.clone(),
        |left, right| Expr::Binary {
            op: BinaryOp::BitAnd,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn bitwise_xor(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(bitwise_and)(input)?;

    fold_many0(
        preceded(ws(char('^')), ws(bitwise_and)),
        move || init.clone(),
        |left, right| Expr::Binary {
            op: BinaryOp::BitXor,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn bitwise_or(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(bitwise_xor)(input)?;

    fold_many0(
        preceded(ws(char('|')), ws(bitwise_xor)),
        move || init.clone(),
        |left, right| Expr::Binary {
            op: BinaryOp::BitOr,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn logical_and(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(bitwise_or)(input)?;

    fold_many0(
        preceded(ws(tag("&&")), ws(bitwise_or)),
        move || init.clone(),
        |left, right| Expr::Binary {
            op: BinaryOp::LogicalAnd,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn logical_or(input: &str) -> IResult<&str, Expr> {
    let (input, init) = ws(logical_and)(input)?;

    fold_many0(
        preceded(ws(tag("||")), ws(logical_and)),
        move || init.clone(),
        |left, right| Expr::Binary {
            op: BinaryOp::LogicalOr,
            left: Box::new(left),
            right: Box::new(right),
        },
    )(input)
}

fn conditional(input: &str) -> IResult<&str, Expr> {
    ws(logical_or)(input)
}

fn range(input: &str) -> IResult<&str, Expr> {
    let (input, start) = ws(conditional)(input)?;
    
    let start1 = start.clone();
    let start2 = start.clone();
    let start3 = start.clone();
    
    alt((
        map(preceded(tag("..="), ws(conditional)), move |end| {
            Expr::Range {
                start: Box::new(start1.clone()),
                end: Box::new(end),
                kind: RangeKind::Inclusive
            }
        }),
        map(preceded(tag(".."), ws(conditional)), move |end| {
            Expr::Range {
                start: Box::new(start2.clone()),
                end: Box::new(end),
                kind: RangeKind::Exclusive
            }
        }),
        value(start3, tag(""))
    ))(input)
}

fn assignment(input: &str) -> IResult<&str, Expr> {
    let (input, left) = ws(range)(input)?;
    
    let left1 = left.clone();
    let left2 = left.clone();

    alt((
        map(preceded(ws(char('=')), ws(assignment)), move |right| {
            Expr::Assign {
                left: Box::new(left1.clone()),
                right: Box::new(right),
                op: AssignOp::Assign
            }
        }),
        value(left2, tag(""))
    ))(input)
}

fn initialiser(input: &str) -> IResult<&str, Option<Expr>> {
    opt(preceded(ws(char('=')), assignment))(input)
}

fn var_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((
            keyword("let"),
            ws(identifier),
            ws(initialiser),
            ws(char(';')),
        )),
        |(_, name, expr, _)| Stmt::Let { name, expr },
    )(input)
}

fn module(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((
            keyword("mod"),
            ws(identifier),
            ws(char(';')),
        )),
        |(_, name, _)| Stmt::Module {
            name
        },
    )(input)
}

fn use_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((
            keyword("use"),
            ws(identifier), // Simplified - real use statements are more complex
            ws(char(';')),
        )),
        |(_, path, _)| Stmt::Use {
            path
        },
    )(input)
}

fn continue_stmt(input: &str) -> IResult<&str, Stmt> {
   map(
        tuple((
            keyword("continue"),
            ws(char(';')),
        )),
        |_| Stmt::Continue,
    )(input)
}

fn return_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((
            keyword("return"),
            ws(opt(assignment)),
            ws(char(';')),
        )),
        |(_, expr, _)| Stmt::Return(expr),
    )(input)
}

fn break_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((
            keyword("break"),
            ws(char(';')),
        )),
        |_| Stmt::Break,
    )(input)
}

fn struct_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((
            keyword("struct"),
            ws(identifier),
            delimited(
                char('{'),
                separated_list0(
                    ws(char(',')),
                    separated_pair(ws(identifier), ws(char(':')), ws(identifier))
                ),
                char('}')
            )
        )),
        |(_, name, fields)| {
            let field_map: Vec<(String, String)> = fields.into_iter().collect();
            Stmt::Struct { name, fields: field_map }
        }
    )(input)
}

fn enum_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((
            keyword("enum"),
            ws(identifier),
            delimited(
                char('{'),
                separated_list0(ws(char(',')), ws(identifier)),
                char('}')
            )
        )),
        |(_, name, variants)| Stmt::Enum { name, variants }
    )(input)
}

fn export_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        preceded(keyword("export"), ws(statement)),
        |stmt| Stmt::Export(Box::new(stmt))
    )(input)
}

fn expression_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((ws(assignment), ws(char(';')))),
        |(expr, _)| Stmt::ExprStmt(expr)
    )(input)
}

fn fun_stmt(input: &str) -> IResult<&str, Stmt> {
    map(
        tuple((
            keyword("fun"),
            ws(identifier),
            param_list,
            block
        )),
        |(_, name, params, body)| Stmt::Function { name, params, body }
    )(input)
}

fn param_list(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        ws(char('(')),
        separated_list0(
            ws(char(',')),
            ws(identifier)
        ),
        ws(char(')'))
    )(input)
}

fn statement(input: &str) -> IResult<&str, Stmt> {
    alt((
        var_stmt,
        fun_stmt,
        continue_stmt,
        return_stmt,
        break_stmt,
        module,
        use_stmt,
        struct_stmt,
        enum_stmt,
        export_stmt,
        expression_stmt,
    ))(input)
}

fn program(input: &str) -> IResult<&str, Program> {
    all_consuming(many0(ws(statement)))(input)
}

// Result<Vec<Stmt>, CompilerError>
pub fn parse(input: Span) -> Result<Program, CompilerError> {
   match program(&input) {
        Ok((_, stmts)) => Ok(stmts),
        Err(nom::Err::Failure(e)) | Err(nom::Err::Error(e)) => {
            Err(CompilerError::from_code(ErrorCode::ParserError, input))
        }
        Err(_) => Err(CompilerError::from_code(ErrorCode::ParserError, input)),
    }
}
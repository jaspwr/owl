use crate::types::Type;

use super::*;

macro_rules! left_associtive_binary_infix_operator {
    ($name:ident, $alt:ident, $next:ident, $({$comp:expr, $oper:expr}),+) => {
        fn $name<'t, 's>(ts: Tokens<'t, 's>, ctx: ParsingContext) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
            let (ts, lhs) = $next(ts, ctx)?;

            $alt(lhs, ts, ctx)
        }

        fn $alt<'t, 's>(lhs: Ast, ts: Tokens<'t, 's>, ctx: ParsingContext) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
            $(
                if peek_and_compare(&ts, $comp) {
                    let start = peek_range_start(ts);
                    let end = peek_range_end(ts);

                    let (ts, rhs) = $next(&ts[1..], ctx)?;

                    let this_oper_res =
                        NodeInner::BinaryOperation($oper, Box::new(lhs), Box::new(rhs)).to_node((start, end));

                    if let Ok(r) = $alt(this_oper_res.clone(), ts.clone(), ctx) {
                        return Ok(r);
                    }

                    return Ok((ts, this_oper_res));
                }
            )+

            Ok((ts, lhs))
        }
    };
}

macro_rules! right_associtive_binary_infix_operator {
    ($name:ident, $alt:ident, $next:ident, $({$comp:expr, $oper:expr}),+) => {
        fn $name<'t, 's>(ts: Tokens<'t, 's>, ctx: ParsingContext) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
            let (ts, lhs) = $next(ts, ctx)?;

            let mut expr = (ts, lhs);

            while let Ok(new_expr) = $alt(expr.1.clone(), expr.0.clone(), ctx) {
                expr = new_expr;
            }

            Ok(expr)
        }

        fn $alt<'t, 's>(lhs: Ast, ts: Tokens<'t, 's>, ctx: ParsingContext) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
            $(
                if peek_and_compare(&ts, $comp) {
                    let start = peek_range_start(ts);
                    let end = peek_range_end(ts);

                    let (ts, rhs) = $next(&ts[1..], ctx)?;

                    let this_oper_res =
                        NodeInner::BinaryOperation($oper, Box::new(lhs), Box::new(rhs)).to_node((start, end));

                    return Ok((ts, this_oper_res));
                }
            )+

            ParseError::new_misc(
                format!("Expected one of: {}", [$($comp.to_string()),+].join(", ")),
                (0, 0),
            )
        }
    };
}

macro_rules! expect_exact_token {
    ($token:expr, $ts:expr) => {
        if !peek_and_compare(&$ts, $token) {
            return ParseError::new_misc(
                format!("Expected `{}`", $token),
                if $ts.is_empty() { (0, 0) } else { $ts[0].range },
            );
        }
        $ts = &$ts[1..];
    };
}

pub fn parse(ts: Tokens) -> Result<Ast, ParseError> {
    let ctx = ParsingContext {};

    let (ts, ast) = statement_list(ts, ctx)?;

    if !ts.is_empty() {
        return ParseError::new_misc(format!("Unexpected token: {}", ts[0].token), ts[0].range);
    }

    Ok(ast)
}

#[derive(Debug, Clone, Copy)]
struct ParsingContext {
    // parsing_args: bool,
}

fn statement_list<'s, 't>(
    mut ts: Tokens<'s, 't>,
    ctx: ParsingContext,
) -> Result<(Tokens<'s, 't>, Ast), ParseError> {
    let start = peek_range_start(ts);

    let mut statements = vec![];

    while !ts.is_empty() {
        if peek_and_compare(ts, "use") {
            let (new_ts, statement) = import(ts, ctx)?;
            ts = new_ts;
            statements.push(statement);
            expect_exact_token!(";", ts);
        } else {
            let res = primary_expression(ts, ctx);

            if let Err(ParseError {
                inner: ParseErrorInner::EndOfInput,
                ..
            }) = res
            {
                break;
            }

            let (new_ts, statement) = res?;
            ts = new_ts;
            println!("{:?}", statement);
            statements.push(statement);
        }

        while peek_and_compare(ts, ";") {
            ts = &ts[1..];
        }
    }

    let end = peek_range_start(ts);

    Ok((
        ts,
        NodeInner::StatementList(statements).to_node((start, end)),
    ))
}

fn import<'s, 't>(
    ts: Tokens<'s, 't>,
    ctx: ParsingContext,
) -> Result<(Tokens<'s, 't>, Ast), ParseError> {
    if peek_and_compare(ts, "use") {
        let start = peek_range_start(ts);

        let mut ts = &ts[1..];

        let mut qualified = true;

        if peek_and_compare(ts, "unqualified") {
            ts = &ts[1..];
            qualified = false;
        }

        let (ts, path) = literal(ts, ParsingContext { ..ctx })?;

        let end = peek_range_start(ts);

        return Ok((
            ts,
            NodeInner::Import {
                qualified,
                path: Box::new(path),
            }
            .to_node((start, end)),
        ));
    }

    primary_expression(ts, ctx)
}

fn function_assignment_name<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Option<(Tokens<'t, 's>, Ast)> {
    if let Ok((ts, name)) = literal(ts, ParsingContext { ..ctx }) {
        return Some((ts, name));
    }

    if let Some(next) = peek(ts) {
        if next.kind == TokenKind::Operator {
            return Some((
                &ts[1..],
                NodeInner::Indentifier(next.token.to_string()).to_node(next.range),
            ));
        }
    }

    None
}

// fn assignment<'t, 's>(
//     ts: Tokens<'t, 's>,
//     ctx: ParsingContext,
// ) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
//     if let Some((mut ts, name)) = function_assignment_name(ts, ctx) {
//         let mut args = vec![];
//
//         while let Ok((new_ts, arg)) = literal(
//             ts,
//             ParsingContext {
//                 ..ctx
//             },
//         ) {
//             ts = new_ts;
//             args.push(arg);
//         }
//
//         if peek_and_compare(ts, "=") {
//             let (ts, rhs) = semicolon(&ts[1..], ctx)?;
//             let body = Box::new(rhs);
//
//             return Ok((
//                 ts,
//                 NodeInner::Assignment {
//                     name: Box::new(name),
//                     args,
//                     body,
//                 },
//             ));
//         }
//     }
//
//     semicolon(ts, ctx)
// }

fn primary_expression<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    lambda(ts, ctx)
}

fn name_and_type<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, NameAndType), ParseError> {
    let start = peek_range_start(ts);

    let Some((mut ts, ident)) = raw_ident(ts, ctx) else {
        return ParseError::new_misc("Expected identifer".to_string(), ts[0].range);
    };

    let mut type_ = Type::Auto;

    if peek_and_compare(ts, ":") {
        ts = &ts[1..];
        let (new_ts, t) = parse_type(ts, ctx)?;
        ts = new_ts;
        type_ = t;
    }

    let end = peek_range_start(ts);

    Ok((
        ts,
        NameAndType {
            name: ident.to_string(),
            type_,
            range: (start, end),
        },
    ))
}

fn parse_type<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Type), ParseError> {
    if peek_and_compare(ts, "*") {
        let (ts, t) = parse_type(&ts[1..], ctx)?;
        return Ok((ts, Type::Ptr(Box::new(t))));
    }

    let Some((mut ts, ident)) = raw_ident(ts, ctx) else {
        return ParseError::new_misc("Expected type".to_string(), ts[0].range);
    };

    let mut type_ = match ident {
        "f64" => Type::F64,
        "bool" => Type::Boolean,
        _ => Type::Ident(ident.to_string()),
    };

    if peek_and_compare(ts, "<") {
        ts = &ts[1..];

        type_ = Type::GenericInstance {
            base: Box::new(type_),
            args: vec![],
        };

        expect_exact_token!(">", ts);
    }

    Ok((ts, type_))
}

fn lambda<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "λ") {
        let start = peek_range_start(ts);

        let mut ts = &ts[1..];

        let mut args = vec![];

        while let Ok((new_ts, arg)) = literal(ts, ParsingContext { ..ctx }) {
            ts = new_ts;
            args.push(arg);
        }

        if peek_and_compare(ts, "->") {
            let (ts, rhs) = primary_expression(&ts[1..], ctx)?;

            let end = peek_range_start(ts);

            return Ok((
                ts,
                NodeInner::Lambda {
                    args,
                    body: Box::new(rhs),
                }
                .to_node((start, end)),
            ));
        }
    }

    control_flow_statements(ts, ctx)
}

fn control_flow_statements<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let start = peek_range_start(ts);

    if peek_and_compare(ts, "return") {
        let (ts, value) = primary_expression(&ts[1..], ctx)?;

        let end = peek_range_start(ts);

        return Ok((ts, NodeInner::Return(Box::new(value)).to_node((start, end))));
    }

    if peek_and_compare(ts, "defer") {
        let (ts, value) = primary_expression(&ts[1..], ctx)?;

        let end = peek_range_start(ts);

        return Ok((ts, NodeInner::Defer(Box::new(value)).to_node((start, end))));
    }

    if peek_and_compare(ts, "continue") {
        let end = peek_range_end(ts);

        return Ok((&ts[1..], NodeInner::Continue.to_node((start, end))));
    }

    if peek_and_compare(ts, "break") {
        let end = peek_range_end(ts);

        return Ok((&ts[1..], NodeInner::Break.to_node((start, end))));
    }

    if_then_else(ts, ctx)
}

fn if_then_else<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "if") {
        let start = peek_range_start(ts);

        ts = &ts[1..];

        let (ts, condition) = primary_expression(ts, ctx)?;
        // println!("cond {:?}\n {:?}", ts, condition);

        // expect_exact_token!("then", ts);

        let end = peek_range_end(ts);

        let (mut ts, then_branch) = primary_expression(ts, ctx)?;

        let mut else_branch = None;
        if peek_and_compare(ts, "else") {
            let (new_ts, new_else_branch) = primary_expression(&ts[1..], ctx)?;
            ts = new_ts;
            else_branch = Some(new_else_branch);
        }

        while peek_and_compare_kind(ts, TokenKind::ExpressionTerminator) {
            ts = &ts[1..];
        }

        Ok((
            ts,
            NodeInner::IfThenElse {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(|e| Box::new(e)),
            }
            .to_node((start, end)),
        ))
    } else {
        while_loop(ts, ctx)
    }
}

fn while_loop<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "while") {
        let start = peek_range_start(ts);

        ts = &ts[1..];

        let (ts, condition) = primary_expression(ts, ctx)?;

        let (mut ts, body) = primary_expression(ts, ctx)?;

        while peek_and_compare_kind(ts, TokenKind::ExpressionTerminator) {
            ts = &ts[1..];
        }

        let end = peek_range_start(ts);

        Ok((
            ts,
            NodeInner::While {
                condition: Box::new(condition),
                body: Box::new(body),
            }
            .to_node((start, end)),
        ))
    } else {
        for_loop(ts, ctx)
    }
}

fn for_loop<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "for") {
        let start = peek_range_start(ts);

        ts = &ts[1..];

        let (mut ts, first) = primary_expression(ts, ctx)?;

        let (var, range) = if peek_and_compare(ts, "in") {
            ts = &ts[1..];

            let (new_ts, second) = primary_expression(ts, ctx)?;

            ts = new_ts;

            (Some(Box::new(first)), Box::new(second))
        } else {
            (None, Box::new(first))
        };

        let end = peek_range_end(ts);

        let (mut ts, body) = primary_expression(ts, ctx)?;

        while peek_and_compare_kind(ts, TokenKind::ExpressionTerminator) {
            ts = &ts[1..];
        }

        Ok((
            ts,
            NodeInner::For {
                var,
                range,
                body: Box::new(body),
            }
            .to_node((start, end)),
        ))
    } else {
        file(ts, ctx)
    }
}

left_associtive_binary_infix_operator!(file, file_, pipe,
    {">>", BinaryOperation::WriteFile},
    {"$=", BinaryOperation::EnvAssign},
    {"=", BinaryOperation::Assign},
    {":=", BinaryOperation::Declare},
    {"::", BinaryOperation::DeclareConst},
    {"+=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Add))},
    {"-=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Sub))},
    {"*=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Mul))},
    {"//=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Div))},
    {"&&=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::And))},
    {"||=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Or))},
    {"++=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Concat))},
    {"**=", BinaryOperation::AssignAnd(Box::new(BinaryOperation::Pow))}
);

left_associtive_binary_infix_operator!(pipe, pipe_, bool_ops,
    {"|", BinaryOperation::Pipe}
);

right_associtive_binary_infix_operator!(bool_ops, _bool_ops, eq,
    {"&&", BinaryOperation::And},
    {"||", BinaryOperation::Or}
);

left_associtive_binary_infix_operator!(eq, eq_, custom,
    {"==", BinaryOperation::Eq},
    {"!=", BinaryOperation::Neq},
    {"<", BinaryOperation::Lt},
    {">", BinaryOperation::Gt},
    {">=", BinaryOperation::Gte},
    {"<=", BinaryOperation::Lte}
);

fn custom<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let (ts, lhs) = concat(ts, ctx)?;
    custom_(lhs, ts, ctx)
}
fn custom_<'t, 's>(
    lhs: Ast,
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let start = peek_range_start(ts);

    // Unideal
    let known_ops = vec![
        "==", "<", ">", "<=", ">=", "&&", "||", "++", "//", "!!", "=", ":=", "+=", "-=", "*=",
        "//=", "&&=", "||=", "++=", "**=", "+", "-", "*", "$=", "**", "|", "..", "..=", "->", "λ",
        ",", ";", "</", ">>", "!=", "::",
    ];

    let Some(next) = peek(ts) else {
        return Ok((ts, lhs));
    };

    if next.kind == TokenKind::Operator && !known_ops.contains(&next.token) {
        let (ts, rhs) = concat(&ts[1..], ctx)?;
        let this_oper_res = NodeInner::BinaryOperation(
            BinaryOperation::Custom(next.token.to_string()),
            Box::new(lhs),
            Box::new(rhs),
        );

        let end = peek_range_start(ts);

        if let Ok(r) = custom_(this_oper_res.clone().to_node((start, end)), ts, ctx) {
            return Ok(r);
        }
        return Ok((ts, this_oper_res.to_node((start, end))));
    }
    Ok((ts, lhs))
}

left_associtive_binary_infix_operator!(concat, concat_, add,
    {"++", BinaryOperation::Concat}
);

left_associtive_binary_infix_operator!(add, add_, mul,
    {"+", BinaryOperation::Add},
    {"-", BinaryOperation::Sub}
);

left_associtive_binary_infix_operator!(mul, mul_, unary_minus,
    {"*", BinaryOperation::Mul},
    {"//", BinaryOperation::Div}
);

fn unary_minus<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "-") {
        let start = peek_range_start(ts);
        let end = peek_range_start(ts);

        let (ts, n) = unary_not(&ts[1..], ctx)?;
        Ok((
            ts,
            NodeInner::UnaryOperation(UnaryOperation::Negate, Box::new(n)).to_node((start, end)),
        ))
    } else {
        unary_not(ts, ctx)
    }
}

fn unary_not<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "!") {
        let start = peek_range_start(ts);
        let end = peek_range_start(ts);

        let (ts, n) = pow(&ts[1..], ctx)?;
        Ok((
            ts,
            NodeInner::UnaryOperation(UnaryOperation::Not, Box::new(n)).to_node((start, end)),
        ))
    } else {
        pow(ts, ctx)
    }
}

left_associtive_binary_infix_operator!(pow, pow_, index,
    {"**", BinaryOperation::Pow}
);

left_associtive_binary_infix_operator!(index, index_, range,
    {"!!", BinaryOperation::Index}
);

left_associtive_binary_infix_operator!(range, _range, call,
    {"<>", BinaryOperation::Range},
    {"<>=", BinaryOperation::RangeInclusive}
);

fn call<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let next = brackets;

    if let Some((ts, fn_name)) = raw_ident(ts, ctx) {
        let start = peek_range_start(ts);
        let end = peek_range_start(ts);

        if peek_and_compare(ts, "(") {
            let mut ts = &ts[1..];

            let mut args = vec![];

            if let Ok((new_ts, arg)) = primary_expression(ts, ctx) {
                ts = new_ts;
                args.push(arg);
            }

            while peek_and_compare(ts, ",") {
                let (new_ts, arg) = primary_expression(ts, ctx)?;
                ts = new_ts;
                args.push(arg);
            }

            expect_exact_token!(")", ts);

            return Ok((
                ts,
                NodeInner::Call(fn_name.to_string(), args).to_node((start, end)),
            ));
        }
    }

    next(ts, ctx)

    // let mut args = vec![];
    //
    // let mut fn_ts = ts;
    // while let Ok((new_ts, arg)) = brackets(
    //     fn_ts,
    //     ParsingContext {
    //         ..ctx
    //     },
    // ) {
    //     fn_ts = new_ts;
    //     args.push(arg);
    // }
    //
    // if !args.is_empty() {
    //     return Ok((fn_ts, NodeInner::Call(Box::new(first), args)));
    // }
    //
    // Ok((ts, first))
}

fn brackets<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "(") {
        let uncolosed_error = ParseError::new_misc("Expected `)`", ts[0].range);

        let (ts, n) = primary_expression(&ts[1..], ParsingContext { ..ctx })?;

        let Some(next) = ts.iter().next() else {
            return uncolosed_error;
        };

        if next.token == ")" {
            Ok((&ts[1..], n))
        } else {
            uncolosed_error
        }
    } else {
        array(ts, ctx)
    }
}

fn array<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "[") {
        let start = peek_range_start(ts);

        let uncolosed_error = ParseError::new_misc("Expected `]`", ts[0].range);

        ts = &ts[1..];

        let mut items = vec![];

        while let Ok((new_ts, item)) = primary_expression(ts, ParsingContext { ..ctx }) {
            ts = new_ts;

            items.push(item);

            if peek_and_compare(ts, ",") {
                ts = &ts[1..];
            } else {
                break;
            }
        }

        let Some(next) = ts.iter().next() else {
            return uncolosed_error;
        };

        if next.token == "]" {
            let end = peek_range_start(ts);

            Ok((
                &ts[1..],
                NodeInner::ArrayLiteral(items).to_node((start, end)),
            ))
        } else {
            uncolosed_error
        }
    } else {
        scope(ts, ctx)
    }
}

fn scope<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "{") {
        let start = peek_range_start(ts);

        if ts.len() >= 3 && ts[2].token == ":" {
            return object_literal(ts, ctx);
        }

        let uncolosed_error = ParseError::new_misc("Expected `}`", ts[0].range);

        ts = &ts[1..];

        let (ts, body) = statement_list(ts, ParsingContext { ..ctx })?;

        let Some(next) = ts.iter().next() else {
            return uncolosed_error;
        };

        if next.token == "}" {
            let end = peek_range_end(ts);

            Ok((
                &ts[1..],
                NodeInner::Scope(Box::new(body)).to_node((start, end)),
            ))
        } else {
            uncolosed_error
        }
    } else {
        markup(ts, ctx)
    }
}

fn object_literal<'t, 's>(
    mut ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let start = peek_range_start(ts);

    expect_exact_token!("{", ts);

    let mut items = vec![];

    while !peek_and_compare(ts, "}") {
        if !peek_and_compare_kind(ts, TokenKind::Word) {
            return ParseError::new_misc("Expected object key", ts[0].range);
        }

        let name = ts[0].token.to_string();
        ts = &ts[1..];

        expect_exact_token!(":", ts);

        let (new_ts, value) = primary_expression(ts, ParsingContext { ..ctx })?;
        ts = new_ts;

        if peek_and_compare(ts, ",") {
            ts = &ts[1..];
        } else if !peek_and_compare(ts, "}") {
            return ParseError::new_misc("Expected `}`".to_string(), ts[0].range);
        }

        items.push((name, value));
    }

    let end = peek_range_end(ts);

    Ok((
        &ts[1..],
        NodeInner::ObjectLiteral(items).to_node((start, end)),
    ))
}

fn markup<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if peek_and_compare(ts, "<") {
        let start = peek_range_start(ts);

        let ts = &ts[1..];

        if peek_and_compare_kind(ts, TokenKind::Word) {
            let tag = ts[0].token.to_string();

            let mut ts = &ts[1..];

            let mut attributes = vec![];

            let mut self_closing = false;

            while !peek_and_compare(ts, ">") {
                if peek_and_compare(ts, "/>") {
                    self_closing = true;
                    break;
                }

                if ts.len() < 3 {
                    return ParseError::new_misc("Expected `>`", (0, 0));
                }

                let name = ts[0].token.to_string();
                ts = &ts[1..];

                expect_exact_token!("=", ts);

                let (new_ts, value) = brackets(ts, ParsingContext { ..ctx })?;

                ts = new_ts;

                attributes.push((name, value));
            }

            ts = &ts[1..];

            let mut body = None;

            if !self_closing {
                let (new_ts, body_) = primary_expression(ts, ParsingContext { ..ctx })?;

                ts = new_ts;
                body = Some(Box::new(body_));

                while peek_and_compare_kind(ts, TokenKind::ExpressionTerminator) {
                    ts = &ts[1..];
                }

                expect_exact_token!("</", ts);
                expect_exact_token!(&tag, ts);
                expect_exact_token!(">", ts);
            }

            let mut siblings = vec![];

            while let Ok((new_ts, sibling)) = markup(ts, ctx) {
                ts = new_ts;
                siblings.push(sibling);
            }

            let end = peek_range_start(ts);

            return Ok((
                ts,
                NodeInner::MarkupBlock {
                    tag,
                    attributes,
                    body,
                    siblings,
                }
                .to_node((start, end)),
            ));
        }
    }

    struct_def(ts, ctx)
}

fn struct_def<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    if !peek_and_compare(ts, "struct") {
        return literal(ts, ctx);
    }

    let start = peek_range_start(ts);

    let mut ts = &ts[1..];

    expect_exact_token!("{", ts);

    let mut items = vec![];

    loop {
        if peek_and_compare(ts, "}") {
            break;
        }

        let item = name_and_type(ts, ParsingContext { ..ctx })?;

        let (new_ts, item) = item;

        ts = new_ts;
        expect_exact_token!(",", ts);

        items.push(item);
    }

    expect_exact_token!("}", ts);

    let end = peek_range_start(ts);

    Ok((
        ts,
        NodeInner::Type(Type::Struct(items)).to_node((start, end)),
    ))
}

fn raw_ident<'t, 's>(
    ts: Tokens<'t, 's>,
    _ctx: ParsingContext,
) -> Option<(Tokens<'t, 's>, &'s str)> {
    if let Some(token) = peek(ts) {
        if token.kind == TokenKind::Word {
            return Some((&ts[1..], token.token));
        }
    }

    None
}

fn literal<'t, 's>(
    ts: Tokens<'t, 's>,
    ctx: ParsingContext,
) -> Result<(Tokens<'t, 's>, Ast), ParseError> {
    let eof_err = Err(ParseError {
        inner: ParseErrorInner::EndOfInput,
        range: (0, 0),
    });

    if ts.is_empty() {
        return eof_err;
    }

    if let Some(token) = peek(ts) {
        if token.token == "true" {
            return Ok((&ts[1..], NodeInner::BoolLiteral(true).to_node(token.range)));
        }

        if token.token == "false" {
            return Ok((&ts[1..], NodeInner::BoolLiteral(false).to_node(token.range)));
        }

        if token.kind == TokenKind::QuotedString {
            let node = NodeInner::String(token.token.to_string()).to_node(token.range);
            return Ok((&ts[1..], node));
        }

        if token.kind == TokenKind::Word {
            let mut node = NodeInner::Indentifier(token.token.to_string());

            let first_char = token.token.chars().next();

            if first_char.map(|c| c.is_ascii_digit()).unwrap_or(false) {
                // TODO: parse ints and hex literals and stuff
                if let Ok(value) = token.token.parse() {
                    node = NodeInner::DoubleLiteral(value);
                }
            }

            return Ok((&ts[1..], node.to_node(token.range)));
        }
    }

    eof_err
}

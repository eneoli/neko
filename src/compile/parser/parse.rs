use chumsky::input::ValueInput;
use chumsky::prelude::*;
use std::collections::HashMap;

use crate::compile::ast::AssignOp;
use crate::compile::ast::Expr;
use crate::compile::ast::FunctionDecl;
use crate::compile::ast::Op;
use crate::compile::ast::SourcePos;
use crate::compile::ast::Stmt;
use crate::compile::ast::Type;
use crate::compile::parser::lex::Token;

type ErrorParserExtra<'src> = extra::Err<Rich<'src, Token<'src>, SourcePos>>;

pub fn expr_parser<'src, I>() -> impl Parser<'src, I, Expr, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let num = select! {Token::NUM {value, base} => (value, base)};
    let ident = select! {Token::IDENT(ident) => ident};

    recursive(|expr| {
        let atomic_expr = choice((
            num.map_with(|(value, base), ctx| Expr::Int {
                value: value.to_string(),
                base,
                span: ctx.span(),
            }),
            ident.map_with(|ident, ctx| Expr::Ident(ident.to_string(), ctx.span())),
            expr.clone()
                .delimited_by(just(Token::L_ROUND), just(Token::R_ROUND)),
        ));

        let unary = just(Token::MINUS)
            .repeated()
            .foldr(atomic_expr, |_, a| Expr::Unary(Op::Neg, Box::new(a)));

        let mult_op = just(Token::STAR)
            .to(Op::Mul)
            .or(just(Token::SLASH).to(Op::Div))
            .or(just(Token::PERCENT).to(Op::Mod));

        let mult = unary
            .clone()
            .foldl(mult_op.then(unary.clone()).repeated(), |a, (op, b)| {
                Expr::Binary(op, Box::new(a), Box::new(b))
            });

        let sum_op = just(Token::PLUS)
            .to(Op::Add)
            .or(just(Token::MINUS).to(Op::Sub));

        let sum = mult
            .clone()
            .foldl(sum_op.then(mult).repeated(), |a, (op, b)| {
                Expr::Binary(op, Box::new(a), Box::new(b))
            });

        sum
    })
}

fn type_parser<'src, I>() -> impl Parser<'src, I, Type, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let int = just(Token::INT).to(Type::INT);

    choice((int,))
}

pub fn decl_parser<'src, I>() -> impl Parser<'src, I, Stmt, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let ident = select! {Token::IDENT(x) => x};

    type_parser()
        .then(ident)
        .then(just(Token::EQ).then(expr_parser()).or_not())
        .then_ignore(just(Token::SEMICOLON))
        .map_with(|((ty, name), expr), ctx| {
            if let Some((_, expr)) = expr {
                Stmt::Init(ty, name.to_string(), expr, ctx.span())
            } else {
                Stmt::Decl(ty, name.to_string(), ctx.span())
            }
        })
}

pub fn assign_parser<'src, I>() -> impl Parser<'src, I, Stmt, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let ident = select! {Token::IDENT(x) => x};

    let op = just(Token::EQ)
        .to(AssignOp::Eq)
        .or(just(Token::ASSIGN_ADD).to(AssignOp::Op(Op::Add)))
        .or(just(Token::ASSIGN_SUB).to(AssignOp::Op(Op::Sub)))
        .or(just(Token::ASSIGN_MULT).to(AssignOp::Op(Op::Mul)))
        .or(just(Token::ASSIGN_DIV).to(AssignOp::Op(Op::Div)))
        .or(just(Token::ASSIGN_MOD).to(AssignOp::Op(Op::Mod)));

    ident
        .then(op)
        .then(expr_parser())
        .then_ignore(just(Token::SEMICOLON))
        .map_with(|((name, op), expr), ctx| Stmt::Assign(name.to_string(), op, expr, ctx.span()))
}

pub fn return_parser<'src, I>() -> impl Parser<'src, I, Stmt, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    just(Token::RETURN)
        .ignore_then(expr_parser())
        .then_ignore(just(Token::SEMICOLON))
        .map_with(|expr, ctx| Stmt::Return(expr, ctx.span()))
}

pub fn stmt_parser<'src, I>() -> impl Parser<'src, I, Stmt, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    choice((decl_parser(), assign_parser(), return_parser()))
}

pub fn function_parser<'src, I>() -> impl Parser<'src, I, FunctionDecl, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let ident = select! {Token::IDENT(x) => x};

    type_parser()
        .then(ident)
        .then_ignore(just(Token::L_ROUND))
        .then_ignore(just(Token::R_ROUND))
        .then(
            stmt_parser()
                .repeated()
                .collect()
                .delimited_by(just(Token::L_CURLY), just(Token::R_CURLY)),
        )
        .map_with(|((ty, name), body), ctx| FunctionDecl {
            ty,
            name: name.to_string(),
            args: Vec::new(),
            body,
            src_pos: ctx.span(),
        })
}

pub fn program_parser<'src, I>()
-> impl Parser<'src, I, HashMap<String, FunctionDecl>, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    // TODO multiple functions with same name
    function_parser()
        .repeated()
        .collect()
        .map(|fx: Vec<FunctionDecl>| {
            fx.into_iter()
                .map(|f| (f.name.clone(), f))
                .collect::<HashMap<_, _>>()
        })
}

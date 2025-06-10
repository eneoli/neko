use std::borrow::Cow;

use chumsky::input::ValueInput;
use chumsky::prelude::*;

use crate::compile::ast::AssignOp;
use crate::compile::ast::Ast;
use crate::compile::ast::BinaryOp;
use crate::compile::ast::Expr;
use crate::compile::ast::FunctionDecl;
use crate::compile::ast::SourcePos;
use crate::compile::ast::Stmt;
use crate::compile::ast::Type;
use crate::compile::ast::UnaryOp;
use crate::compile::ast::int_literal::IntLiteral;
use crate::compile::parser::lex::Token;

type ErrorParserExtra<'src> = extra::Err<Rich<'src, Token<'src>, SourcePos>>;

pub fn program_parser<'src, I>() -> impl Parser<'src, I, Ast, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    function_parser().map(|fun| Ast::FunctionDecl(fun))
}

fn function_parser<'src, I>() -> impl Parser<'src, I, FunctionDecl, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let ident = select! {Token::IDENT(x) => x};

    type_parser()
        .then(ident)
        .then_ignore(just(Token::L_ROUND))
        .then_ignore(just(Token::R_ROUND))
        .then(
            stmt()
                .repeated()
                .collect()
                .delimited_by(just(Token::L_CURLY), just(Token::R_CURLY)),
        )
        .try_map_with(|((ty, name), body), ctx| {
            let fun = FunctionDecl {
                ty,
                name: name.to_string(),
                args: Vec::new(),
                body,
                span: ctx.span(),
            };

            if "main" != fun.name {
                return Err(Rich::custom(
                    ctx.span(),
                    "For now we support only a single \"main\" function",
                ));
            }

            if Type::INT != fun.ty {
                return Err(Rich::custom(
                    ctx.span(),
                    "\"main\" function needs to have return type \"int\"",
                ));
            }

            Ok(fun)
        })
}

fn type_parser<'src, I>() -> impl Parser<'src, I, Type, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let int = just(Token::INT).to(Type::INT);
    let bool = just(Token::BOOL).to(Type::BOOL);

    choice((int, bool))
}

fn stmt<'src, I>() -> impl Parser<'src, I, Stmt, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    recursive(|stmt| {
        let simp_stmt = choice((decl(), assign())).boxed();

        let if_stmt = just(Token::IF)
            .ignore_then(expr().delimited_by(just(Token::L_ROUND), just(Token::R_ROUND)))
            .then(stmt.clone())
            .then(just(Token::ELSE).ignore_then(stmt.clone()).or_not())
            .map(|((expr, if_stmt), else_stmt)| {
                Stmt::If(expr, Box::new(if_stmt), else_stmt.map(Box::new))
            })
            .boxed();

        let while_stmt = just(Token::WHILE)
            .ignore_then(expr().delimited_by(just(Token::L_ROUND), just(Token::R_ROUND)))
            .then(stmt.clone())
            .map(|(expr, stmt)| Stmt::While(expr, Box::new(stmt)))
            .boxed();

        let for_stmt = just(Token::FOR)
            .ignore_then(just(Token::L_ROUND))
            .ignore_then(simp_stmt.clone().or_not())
            .then_ignore(just(Token::SEMICOLON))
            .then(expr())
            .then_ignore(just(Token::SEMICOLON))
            .then(simp_stmt.clone().or_not())
            .then_ignore(just(Token::R_ROUND))
            .then(stmt.clone())
            .map(|(((init, condition), step), body)| {
                Stmt::For(
                    init.map(Box::new),
                    condition,
                    step.map(Box::new),
                    Box::new(body),
                )
            })
            .boxed();

        let break_stmt = just(Token::BREAK)
            .then_ignore(just(Token::SEMICOLON))
            .map_with(|_, ctx| Stmt::Break(ctx.span()))
            .boxed();

        let continue_stmt = just(Token::CONTINUE)
            .then_ignore(just(Token::SEMICOLON))
            .map_with(|_, ctx| Stmt::Continue(ctx.span()))
            .boxed();

        let return_stmt = just(Token::RETURN)
            .ignore_then(expr())
            .then_ignore(just(Token::SEMICOLON))
            .map_with(|expr, ctx| Stmt::Return(expr, ctx.span()))
            .boxed();

        let block_stmt = recursive(|block| {
            let atomic_stmt = choice((stmt.clone(), block));

            atomic_stmt
                .repeated()
                .collect()
                .delimited_by(just(Token::L_CURLY), just(Token::R_CURLY))
                .map(Stmt::Block)
                .boxed()
        })
        .boxed();

        choice((
            simp_stmt.then_ignore(just(Token::SEMICOLON)),
            if_stmt,
            while_stmt,
            for_stmt,
            continue_stmt,
            break_stmt,
            return_stmt,
            block_stmt,
        ))
        .boxed()
    })
}

fn decl<'src, I>() -> impl Parser<'src, I, Stmt, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let ident = select! {Token::IDENT(x) => x};

    type_parser()
        .then(ident)
        .then(just(Token::EQUAL_SIGN).then(expr()).or_not())
        .map_with(|((ty, name), expr), ctx| {
            let expr = if let Some((_, expr)) = expr {
                Some(expr)
            } else {
                None
            };

            Stmt::Decl(ty, name.to_string(), expr, ctx.span())
        })
}

fn assign<'src, I>() -> impl Parser<'src, I, Stmt, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let op = choice((
        just(Token::EQUAL_SIGN).to(AssignOp::Eq),
        just(Token::ASSIGN_ADD).to(AssignOp::Add),
        just(Token::ASSIGN_SUB).to(AssignOp::Sub),
        just(Token::ASSIGN_MULT).to(AssignOp::Mul),
        just(Token::ASSIGN_DIV).to(AssignOp::Div),
        just(Token::ASSIGN_MOD).to(AssignOp::Mod),
        just(Token::ASSIGN_BIT_AND).to(AssignOp::BitwiseAnd),
        just(Token::ASSIGN_BIT_OR).to(AssignOp::BitwiseOr),
        just(Token::ASSIGN_BIT_XOR).to(AssignOp::BitwiseXor),
        just(Token::ASSIGN_SHIFT_LEFT).to(AssignOp::ShiftLeft),
        just(Token::ASSIGN_SHIFT_RIGHT).to(AssignOp::ShiftRight),
    ));

    lvalue()
        .then(op)
        .then(expr())
        .map_with(|((name, op), expr), ctx| Stmt::Assign(name.to_string(), op, expr, ctx.span()))
}

fn lvalue<'src, I>() -> impl Parser<'src, I, Cow<'src, str>, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let ident = select! {Token::IDENT(x) => x};

    recursive(|lvalue| ident.or(lvalue.delimited_by(just(Token::L_ROUND), just(Token::R_ROUND))))
}

fn expr<'src, I>() -> impl Parser<'src, I, Expr, ErrorParserExtra<'src>>
where
    I: ValueInput<'src, Token = Token<'src>, Span = SourcePos>,
{
    let num = select! {Token::NUM {value, base} => (value, base)}.map_with(|(value, base), ctx| {
        Expr::Int(IntLiteral::new(value.to_string(), base), ctx.span())
    });

    let bool = choice((
        select! {Token::TRUE  => true},
        select! {Token::FALSE => false},
    ))
    .map_with(|value, ctx| Expr::Bool(value, ctx.span()));

    let ident = select! {Token::IDENT(ident) => ident}
        .map_with(|ident, ctx| Expr::Ident(ident.to_string(), ctx.span()));

    recursive(|expr| {
        let atomic_expr = choice((
            num,
            bool,
            ident,
            expr.clone()
                .delimited_by(just(Token::L_ROUND), just(Token::R_ROUND)),
        ))
        .boxed();

        let unary_op = choice((
            just(Token::MINUS).to(UnaryOp::Neg),
            just(Token::EXCLAMATION_MARK).to(UnaryOp::LogicalNot),
            just(Token::TILDE).to(UnaryOp::BitwiseNot),
        ))
        .boxed();
        let unary = unary_op
            .repeated()
            .foldr(atomic_expr, |op, a| Expr::Unary(op, Box::new(a)));

        let mult_op = choice((
            just(Token::STAR).to(BinaryOp::Mul),
            just(Token::SLASH).to(BinaryOp::Div),
            just(Token::PERCENT).to(BinaryOp::Mod),
        ))
        .boxed();
        let mult = unary
            .clone()
            .foldl(mult_op.then(unary.clone()).repeated(), |a, (op, b)| {
                Expr::Binary(op, Box::new(a), Box::new(b))
            });

        let sum_op = choice((
            just(Token::PLUS).to(BinaryOp::Add),
            just(Token::MINUS).to(BinaryOp::Sub),
        ))
        .boxed();
        let sum = mult
            .clone()
            .foldl(sum_op.then(mult).repeated(), |a, (op, b)| {
                Expr::Binary(op, Box::new(a), Box::new(b))
            })
            .boxed();

        let shift_op = choice((
            just(Token::SHIFT_LEFT).to(BinaryOp::ShiftLeft),
            just(Token::SHIFT_RIGHT).to(BinaryOp::ShiftRight),
        ))
        .boxed();
        let shift = sum
            .clone()
            .foldl(shift_op.then(sum).repeated(), |a, (op, b)| {
                Expr::Binary(op, Box::new(a), Box::new(b))
            })
            .boxed();

        let int_comp_op = choice((
            just(Token::L_POINTY).to(BinaryOp::Less),
            just(Token::LESS_EQ).to(BinaryOp::LessEq),
            just(Token::R_POINTY).to(BinaryOp::Greater),
            just(Token::GREATER_EQ).to(BinaryOp::GreaterEq),
        ))
        .boxed();
        let int_comp = shift
            .clone()
            .foldl(int_comp_op.then(shift).repeated(), |a, (op, b)| {
                Expr::Binary(op, Box::new(a), Box::new(b))
            });

        let comp_op = choice((
            just(Token::EQ).to(BinaryOp::Eq),
            just(Token::NOT_EQ).to(BinaryOp::NotEq),
        ))
        .boxed();
        let comp = int_comp
            .clone()
            .foldl(comp_op.then(int_comp).repeated(), |a, (op, b)| {
                Expr::Binary(op, Box::new(a), Box::new(b))
            })
            .boxed();

        let bitwise_and = comp
            .clone()
            .foldl(
                just(Token::AMPERSAND)
                    .to(BinaryOp::BitwiseAnd)
                    .then(comp)
                    .repeated(),
                |a, (op, b)| Expr::Binary(op, Box::new(a), Box::new(b)),
            )
            .boxed();

        let bitwise_xor = bitwise_and
            .clone()
            .foldl(
                just(Token::CARET)
                    .to(BinaryOp::BitwiseXor)
                    .then(bitwise_and)
                    .repeated(),
                |a, (op, b)| Expr::Binary(op, Box::new(a), Box::new(b)),
            )
            .boxed();

        let bitwise_or = bitwise_xor
            .clone()
            .foldl(
                just(Token::PIPE)
                    .to(BinaryOp::BitwiseOr)
                    .then(bitwise_xor)
                    .repeated(),
                |a, (op, b)| Expr::Binary(op, Box::new(a), Box::new(b)),
            )
            .boxed();

        let logical_and = bitwise_or
            .clone()
            .foldl(
                just(Token::LOGICAL_AND)
                    .to(BinaryOp::LogicalAnd)
                    .then(bitwise_or)
                    .repeated(),
                |a, (op, b)| Expr::Binary(op, Box::new(a), Box::new(b)),
            )
            .boxed();

        let logical_or = logical_and
            .clone()
            .foldl(
                just(Token::LOGICAL_OR)
                    .to(BinaryOp::LogicalOr)
                    .then(logical_and)
                    .repeated(),
                |a, (op, b)| Expr::Binary(op, Box::new(a), Box::new(b)),
            )
            .boxed();

        let ternary = logical_or
            .clone()
            .then(
                just(Token::QUESTION_MARK)
                    .ignore_then(expr.clone())
                    .then_ignore(just(Token::COLON))
                    .then(expr.clone())
                    .or_not(),
            )
            .map(|(head_expr, tail_expr)| {
                if let Some((then_expr, else_expr)) = tail_expr {
                    return Expr::Ternary(
                        Box::new(head_expr),
                        Box::new(then_expr),
                        Box::new(else_expr),
                    );
                }

                head_expr
            })
            .boxed();

        ternary
    })
}

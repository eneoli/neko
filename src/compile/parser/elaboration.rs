///
/// Simplifications before Type Checking was performed.
/// Primary used to make Type Checking easier.
///
use std::marker::PhantomData;

use crate::compile::{
    ast::{Ast, Elaborated, FunctionDecl, Parsed, PhaseExpr, PhaseStmt, desugared, parsed},
    semantic::SemanticError,
};

pub fn elab(ast: Ast<Parsed>) -> Result<Ast<Elaborated>, SemanticError> {
    let Ast {
        main:
            FunctionDecl {
                ty,
                name,
                args,
                body,
                span,
                ..
            },
        ..
    } = ast;

    let mut new_body = vec![];
    for stmt in body.into_iter() {
        let new_stmt = elab_stmt(stmt)?;
        new_body.extend(new_stmt);
    }

    let ast = Ast {
        main: FunctionDecl {
            ty,
            name,
            args,
            body: new_body,
            span,
        },
        _marker: PhantomData,
    };

    Ok(ast)
}

fn elab_stmt(stmt: parsed::Stmt) -> Result<Vec<desugared::Stmt>, SemanticError> {
    let stmt = match stmt {
        parsed::Stmt::Decl(ty, name, None, span) => {
            vec![desugared::Stmt::Decl(ty, name, span)]
        }
        parsed::Stmt::Decl(ty, name, Some(init), span) => vec![
            desugared::Stmt::Decl(ty, name.clone(), span.clone()),
            desugared::Stmt::Assign(name, elab_expr(init)?, span),
        ],
        parsed::Stmt::Assign(name, op, rhs, span) => {
            let rhs = elab_expr(rhs)?;

            let expr = match op {
                parsed::AssignOp::Eq => rhs,
                _ => desugared::Expr::Binary(
                    elab_binary_op(op.into()),
                    desugared::Expr::Ident(name.clone(), span.clone()).boxed(),
                    rhs.boxed(),
                ),
            };

            vec![desugared::Stmt::Assign(name, expr, span)]
        }
        parsed::Stmt::Block(stmts) => vec![desugared::Stmt::Block(
            stmts
                .into_iter()
                .map(elab_stmt)
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .flatten()
                .collect(),
        )],
        parsed::Stmt::For(init, cond, step, body) => {
            vec![desugared::Stmt::For(
                init.map(|stmt| elab_stmt(*stmt)).transpose()?,
                elab_expr(cond)?,
                step.map(|stmt| elab_stmt(*stmt))
                    .transpose()?
                    .map(compound)
                    .map(Box::new),
                Box::new(compound(elab_stmt(*body)?)),
            )]

            // vec![desugared::Stmt::For(
            //     init.map(|stmt| elab_stmt(*stmt)).transpose()?,
            //     elab_expr(cond)?,
            //     step.map(|stmt| compound(elab_stmt(*stmt)?).boxed()),
            //     compound(elab_stmt(*body)?).boxed(),
            // )]
        }
        parsed::Stmt::While(expr, stmt) => vec![desugared::Stmt::While(
            elab_expr(expr)?,
            compound(elab_stmt(*stmt)?).boxed(),
        )],
        parsed::Stmt::If(cond, then, otherwise) => {
            vec![desugared::Stmt::If(
                elab_expr(cond)?,
                Box::new(compound(elab_stmt(*then)?)),
                match otherwise {
                    Some(stmt) => Some(Box::new(compound(elab_stmt(*stmt)?))),
                    None => None,
                },
            )]
            //     vec![desugared::Stmt::If(
            //     elab_expr(cond)?,
            //     compound(elab_stmt(*then)?).boxed(),
            //     otherwise.map(|stmt| compound(elab_stmt(*stmt)?).boxed()),
            // )]
        }
        parsed::Stmt::Return(rhs, span) => vec![desugared::Stmt::Return(elab_expr(rhs)?, span)],
        parsed::Stmt::Break(span) => vec![desugared::Stmt::Break(span)],
        parsed::Stmt::Continue(span) => vec![desugared::Stmt::Continue(span)],
    };

    Ok(stmt)
}

fn compound(stmts: Vec<desugared::Stmt>) -> desugared::Stmt {
    if stmts.len() == 1 {
        return stmts.into_iter().next().unwrap();
    }

    desugared::Stmt::Block(stmts)
}

fn elab_expr(expr: parsed::Expr) -> Result<desugared::Expr, SemanticError> {
    let expr = match expr {
        parsed::Expr::Int(value, span) => desugared::Expr::Int(value.parse()?, span),
        parsed::Expr::Bool(value, span) => desugared::Expr::Bool(value, span),
        parsed::Expr::Ident(name, span) => desugared::Expr::Ident(name, span),
        parsed::Expr::Unary(op, rhs) => {
            let rhs_span = rhs.span();
            let rhs = elab_expr(*rhs)?.boxed();
            let true_expr = desugared::Expr::Bool(true, rhs_span.clone()).boxed();
            let false_expr = desugared::Expr::Bool(false, rhs_span.clone()).boxed();

            match op {
                parsed::UnaryOp::LogicalNot => desugared::Expr::Ternary(rhs, false_expr, true_expr),
                parsed::UnaryOp::BitwiseNot => desugared::Expr::Binary(
                    desugared::BinaryOp::BitwiseXor,
                    desugared::Expr::max_int(rhs_span).boxed(),
                    rhs,
                ),
                parsed::UnaryOp::Neg => desugared::Expr::Binary(
                    desugared::BinaryOp::Sub,
                    desugared::Expr::zero(rhs_span).boxed(),
                    rhs,
                ),
            }
        }
        parsed::Expr::Binary(op, lhs, rhs) => {
            let lhs_span = lhs.span();
            let lhs = elab_expr(*lhs)?.boxed();
            let rhs = elab_expr(*rhs)?.boxed();
            let true_expr = desugared::Expr::Bool(true, lhs_span.clone()).boxed();
            let false_expr = desugared::Expr::Bool(false, lhs_span.clone()).boxed();

            match op {
                parsed::BinaryOp::LogicalAnd => desugared::Expr::Ternary(lhs, rhs, false_expr),
                parsed::BinaryOp::LogicalOr => desugared::Expr::Ternary(lhs, true_expr, rhs),
                _ => desugared::Expr::Binary(elab_binary_op(op), lhs, rhs),
            }
        }
        parsed::Expr::Ternary(cond, then, otherwise) => desugared::Expr::Ternary(
            elab_expr(*cond)?.boxed(),
            elab_expr(*then)?.boxed(),
            elab_expr(*otherwise)?.boxed(),
        ),
    };

    Ok(expr)
}

fn elab_unary_op(op: parsed::UnaryOp) -> desugared::UnaryOp {
    match op {
        parsed::UnaryOp::Neg => desugared::UnaryOp::Neg,
        parsed::UnaryOp::BitwiseNot => desugared::UnaryOp::BitwiseNot,
        parsed::UnaryOp::LogicalNot => panic!("Cannot translate this op into Core: {:#?}", op),
    }
}

fn elab_binary_op(op: parsed::BinaryOp) -> desugared::BinaryOp {
    match op {
        parsed::BinaryOp::Add => desugared::BinaryOp::Add,
        parsed::BinaryOp::Sub => desugared::BinaryOp::Sub,
        parsed::BinaryOp::Mul => desugared::BinaryOp::Mul,
        parsed::BinaryOp::Div => desugared::BinaryOp::Div,
        parsed::BinaryOp::Mod => desugared::BinaryOp::Mod,
        parsed::BinaryOp::BitwiseAnd => desugared::BinaryOp::BitwiseAnd,
        parsed::BinaryOp::BitwiseOr => desugared::BinaryOp::BitwiseOr,
        parsed::BinaryOp::BitwiseXor => desugared::BinaryOp::BitwiseXor,
        parsed::BinaryOp::ShiftLeft => desugared::BinaryOp::ShiftLeft,
        parsed::BinaryOp::ShiftRight => desugared::BinaryOp::ShiftRight,
        parsed::BinaryOp::Eq => desugared::BinaryOp::Eq,
        parsed::BinaryOp::NotEq => desugared::BinaryOp::NotEq,
        parsed::BinaryOp::Less => desugared::BinaryOp::Less,
        parsed::BinaryOp::LessEq => desugared::BinaryOp::LessEq,
        parsed::BinaryOp::Greater => desugared::BinaryOp::Greater,
        parsed::BinaryOp::GreaterEq => desugared::BinaryOp::GreaterEq,
        parsed::BinaryOp::LogicalAnd | parsed::BinaryOp::LogicalOr => {
            panic!("Cannot translate this op into Core: {:#?}", op)
        }
    }
}

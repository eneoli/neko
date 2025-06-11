use crate::compile::ast::{core, desugared, Ast, Core, FunctionDecl, PhaseStmt, TypeChecked};

///
/// Simplifications after Type Checking was performed.
/// Primary used to make IR generation easier.
///
use std::marker::PhantomData;

pub fn lower(ast: Ast<TypeChecked>) -> Ast<Core> {
    let Ast {
        main:
            FunctionDecl {
                ty,
                name,
                args,
                body,
                span,
            },
        ..
    } = ast;

    let mut stmts = vec![];
    for stmt in body.into_iter() {
        let mut core_stmt = lower_stmt(stmt);
        stmts.append(&mut core_stmt);
    }

    Ast {
        main: FunctionDecl {
            ty,
            name,
            args,
            body: stmts,
            span,
        },
        _marker: PhantomData,
    }
}

fn lower_stmt(stmt: desugared::Stmt) -> Vec<core::Stmt> {
    match stmt {
        desugared::Stmt::Decl(ty, name, span) => vec![core::Stmt::Decl(ty, name, span)],
        desugared::Stmt::Assign(name, value, span) => vec![core::Stmt::Assign(name, value, span)],
        desugared::Stmt::Return(expr, span) => vec![core::Stmt::Return(expr, span)],
        desugared::Stmt::Block(stmts) => {
            vec![core::Stmt::Block(
                stmts.into_iter().flat_map(lower_stmt).collect(),
            )]
        }
        desugared::Stmt::If(cond, then, otherwise) => vec![core::Stmt::If(
            cond,
            compound(lower_stmt(*then)).boxed(),
            otherwise.map(|stmt| compound(lower_stmt(*stmt)).boxed()),
        )],
        desugared::Stmt::For(init, cond, step, body) => {

            // Note: This inlines step before each continue belonging to the for and the end of the body.
            //       This is sound, because we do not allow shadowing of variables. Therefore variables
            //       in step cannot be bound by declarations inside the loop body.

            let mut init_stmts = vec![];
            if let Some(init) = init {
                init_stmts = init
                    .into_iter()
                    .flat_map(|init| lower_stmt(init))
                    .collect();
            }

            let mut step_stmts = vec![];
            if let Some(step) = step {
                step_stmts = lower_stmt(*step);
            }

            let mut body_stmts = vec![];
            for stmt in lower_stmt(*body) {
                let mut stmts = transform_for(stmt, &step_stmts);
                body_stmts.append(&mut stmts);
            }

            body_stmts.append(&mut step_stmts);

            let mut stmts = init_stmts;
            stmts.push(core::Stmt::While(cond, compound(body_stmts).boxed()));

            stmts
        }
        desugared::Stmt::While(expr, body) => vec![core::Stmt::While(
            expr,
            Box::new(compound(lower_stmt(*body))),
        )],
        desugared::Stmt::Break(span) => vec![core::Stmt::Break(span)],
        desugared::Stmt::Continue(span) => vec![core::Stmt::Continue(span)],
    }
}

fn transform_for(stmt: core::Stmt, step_stmts: &Vec<core::Stmt>) -> Vec<core::Stmt> {
    match stmt {
        core::Stmt::Block(stmts) => vec![core::Stmt::Block(
            stmts
                .into_iter()
                .flat_map(|stmt| transform_for(stmt, step_stmts))
                .collect(),
        )],
        core::Stmt::Continue(_) => vec![step_stmts.clone(), vec![stmt]].concat(),
        core::Stmt::If(expr, then, otherwise) => vec![core::Stmt::If(
            expr,
            compound(transform_for(*then, step_stmts)).boxed(),
            otherwise.map(|stmt| compound(transform_for(*stmt, step_stmts)).boxed()),
        )],
        _ => vec![stmt],
    }
}

fn compound(stmts: Vec<core::Stmt>) -> core::Stmt {
    if stmts.len() == 1 {
        return stmts.into_iter().last().unwrap();
    }

    core::Stmt::Block(stmts)
}

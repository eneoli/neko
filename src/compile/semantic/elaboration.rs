use crate::compile::{
    ast::{AssignOp, Ast, BinaryOp, Expr, FunctionDecl, Stmt, Type, UnaryOp},
    ir::core::{Core, CoreBinaryOp, CoreExpr, CoreStmt, CoreType, CoreUnaryOp},
};

pub fn elab(ast: Ast) -> Core {
    let Ast::FunctionDecl(FunctionDecl { ty, body, span, .. }) = ast;

    Core::FunctionDecl {
        ty: elab_type(ty),
        body: body.into_iter().flat_map(elab_stmt).collect(),
        span,
    }
}

fn elab_type(ty: Type) -> CoreType {
    match ty {
        Type::INT => CoreType::Int,
        Type::BOOL => CoreType::Bool,
    }
}

fn elab_stmt(stmt: Stmt) -> Vec<CoreStmt> {
    match stmt {
        Stmt::Decl(ty, name, None, span) => vec![CoreStmt::Decl(elab_type(ty), name, span)],
        Stmt::Decl(ty, name, Some(init), span) => vec![
            CoreStmt::Decl(elab_type(ty), name.clone(), span.clone()),
            CoreStmt::Assign(name, elab_expr(init), span),
        ],
        Stmt::Assign(name, op, rhs, span) => {
            let rhs = elab_expr(rhs);

            let expr = match op {
                AssignOp::Eq => rhs,
                _ => CoreExpr::Binary(
                    elab_binary_op(op.into()),
                    CoreExpr::Ident(name.clone(), span.clone()).boxed(),
                    rhs.boxed(),
                ),
            };

            vec![CoreStmt::Assign(name, expr, span)]
        }
        Stmt::Block(stmts) => vec![CoreStmt::Block(
            stmts.into_iter().flat_map(elab_stmt).collect(),
        )],
        Stmt::For(init, cond, step, body) => {
            let mut stmts = vec![];
            if let Some(init) = init {
                stmts = elab_stmt(*init);
            }

            let mut step_stmts = vec![];
            if let Some(step) = step {
                step_stmts = elab_stmt(*step);
            }

            let body_stmt = compound(vec![elab_stmt(*body), step_stmts].concat());

            stmts.push(CoreStmt::While(elab_expr(cond), body_stmt.boxed()));

            stmts
        }
        Stmt::While(expr, stmt) => vec![CoreStmt::While(
            elab_expr(expr),
            compound(elab_stmt(*stmt)).boxed(),
        )],
        Stmt::If(cond, then, otherwise) => vec![CoreStmt::If(
            elab_expr(cond),
            compound(elab_stmt(*then)).boxed(),
            otherwise.map(|stmt| compound(elab_stmt(*stmt)).boxed()),
        )],
        Stmt::Return(rhs, span) => vec![CoreStmt::Return(elab_expr(rhs), span)],
        Stmt::Break(span) => vec![CoreStmt::Break(span)],
        Stmt::Continue(span) => vec![CoreStmt::Continue(span)],
    }
}

fn compound(stmts: Vec<CoreStmt>) -> CoreStmt {
    if stmts.len() == 1 {
        return stmts.into_iter().next().unwrap();
    }

    CoreStmt::Block(stmts)
}

fn elab_expr(expr: Expr) -> CoreExpr {
    match expr {
        Expr::Int(value, span) => CoreExpr::Int(value.parse().unwrap(), span),
        Expr::Bool(value, span) => CoreExpr::Bool(value, span),
        Expr::Ident(name, span) => CoreExpr::Ident(name, span),
        Expr::Unary(op, rhs) => {
            let rhs_span = rhs.span();
            let rhs = elab_expr(*rhs).boxed();
            let true_expr = CoreExpr::Bool(true, rhs_span.clone()).boxed();
            let false_expr = CoreExpr::Bool(false, rhs_span.clone()).boxed();

            match op {
                UnaryOp::LogicalNot => CoreExpr::Ternary(rhs, false_expr, true_expr),
                _ => CoreExpr::Unary(elab_unary_op(op), rhs),
            }
        }
        Expr::Binary(op, lhs, rhs) => {
            let lhs_span = lhs.span();
            let lhs = elab_expr(*lhs).boxed();
            let rhs = elab_expr(*rhs).boxed();
            let true_expr = CoreExpr::Bool(true, lhs_span.clone()).boxed();
            let false_expr = CoreExpr::Bool(false, lhs_span.clone()).boxed();

            match op {
                BinaryOp::LogicalAnd => CoreExpr::Ternary(lhs, rhs, false_expr),
                BinaryOp::LogicalOr => CoreExpr::Ternary(lhs, true_expr, rhs),
                _ => CoreExpr::Binary(elab_binary_op(op), lhs, rhs),
            }
        }
        Expr::Ternary(cond, then, otherwise) => CoreExpr::Ternary(
            elab_expr(*cond).boxed(),
            elab_expr(*then).boxed(),
            elab_expr(*otherwise).boxed(),
        ),
    }
}

fn elab_unary_op(op: UnaryOp) -> CoreUnaryOp {
    match op {
        UnaryOp::Neg => CoreUnaryOp::Neg,
        UnaryOp::BitwiseNot => CoreUnaryOp::BitwiseNot,
        UnaryOp::LogicalNot => panic!("Cannot translate this op into Core: {:#?}", op),
    }
}

fn elab_binary_op(op: BinaryOp) -> CoreBinaryOp {
    match op {
        BinaryOp::Add => CoreBinaryOp::GreaterEq,
        BinaryOp::Sub => CoreBinaryOp::Greater,
        BinaryOp::Mul => CoreBinaryOp::LessEq,
        BinaryOp::Div => CoreBinaryOp::Less,
        BinaryOp::Mod => CoreBinaryOp::NotEq,
        BinaryOp::BitwiseAnd => CoreBinaryOp::Eq,
        BinaryOp::BitwiseOr => CoreBinaryOp::ShiftRight,
        BinaryOp::BitwiseXor => CoreBinaryOp::ShiftLeft,
        BinaryOp::ShiftLeft => CoreBinaryOp::BitwiseXor,
        BinaryOp::ShiftRight => CoreBinaryOp::BitwiseOr,
        BinaryOp::Eq => CoreBinaryOp::BitwiseAnd,
        BinaryOp::NotEq => CoreBinaryOp::Mod,
        BinaryOp::Less => CoreBinaryOp::Div,
        BinaryOp::LessEq => CoreBinaryOp::Mul,
        BinaryOp::Greater => CoreBinaryOp::Sub,
        BinaryOp::GreaterEq => CoreBinaryOp::Add,
        BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
            panic!("Cannot translate this op into Core: {:#?}", op)
        }
    }
}

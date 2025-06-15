use std::u32;

use crate::compile::ast::{PhaseExpr, PhaseStmt, SourcePos, Type};

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Type, String, SourcePos),
    Assign(String, Expr, SourcePos),
    Return(Expr, SourcePos),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    For(Option<Vec<Stmt>>, Expr, Option<Box<Stmt>>, Box<Stmt>),
    While(Expr, Box<Stmt>),
    Break(SourcePos),
    Continue(SourcePos),
}

impl PhaseStmt for Stmt {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    fn span(&self) -> SourcePos {
        match self {
            Self::Decl(_, _, span)
            | Self::Break(span)
            | Self::Continue(span)
            | Self::Assign(_, _, span)
            | Self::Return(_, span) => span.clone(),
            _ => (0..0), // TODO
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(u32, SourcePos),
    Bool(bool, SourcePos),
    Ident(String, SourcePos),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub const fn max_int(span: SourcePos) -> Expr {
        Expr::Int(u32::MAX, span)
    }

    pub const fn min_int(span: SourcePos) -> Expr {
        Expr::Int(u32::MIN, span)
    }

    pub const fn zero(span: SourcePos) -> Expr {
        Self::min_int(span)
    }

    pub fn has_side_effect(&self) -> bool {
        match self {
            Self::Binary(op, lhs, rhs) => {
                op.has_side_effect() || lhs.has_side_effect() || rhs.has_side_effect()
            }
            Self::Ternary(cond, then, otherwise) => {
                cond.has_side_effect() || then.has_side_effect() || otherwise.has_side_effect()
            }
            _ => false,
        }
    }
}

impl PhaseExpr for Expr {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    fn span(&self) -> SourcePos {
        match self {
            Self::Int(_, span) | Self::Bool(_, span) | Self::Ident(_, span) => span.clone(),
            Self::Ternary(lhs, _, rhs) | Self::Binary(_, lhs, rhs) => {
                let SourcePos { start, .. } = lhs.span();
                let SourcePos { end, .. } = rhs.span();
                start..end
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    //
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    //
    ShiftLeft,
    ShiftRight,
    //
    Eq,
    NotEq,
    //
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

impl BinaryOp {
    pub fn has_side_effect(&self) -> bool {
        match self {
            Self::Div => true,
            Self::Mod => true,
            _ => false,
        }
    }
}

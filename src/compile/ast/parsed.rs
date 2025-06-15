use crate::compile::ast::{int_literal::IntLiteral, PhaseExpr, PhaseStmt, SourcePos, Type};

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Type, String, Option<Expr>, SourcePos),
    Assign(String, AssignOp, Expr, SourcePos),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    For(Option<Box<Stmt>>, Expr, Option<Box<Stmt>>, Box<Stmt>),
    Return(Expr, SourcePos),
    Break(SourcePos),
    Continue(SourcePos),
}

impl PhaseStmt for Stmt {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    fn span(&self) -> SourcePos {
        (0..0) // TODO
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(IntLiteral, SourcePos),
    Bool(bool, SourcePos),
    Ident(String, SourcePos),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl PhaseExpr for Expr {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    fn span(&self) -> SourcePos {
        match self {
            Self::Ident(_, span) | Self::Int(_, span) | Self::Bool(_, span) => span.clone(),
            Self::Unary(_, expr) => {
                let SourcePos { start, end } = expr.span();
                (start - 1)..end
            }
            Self::Binary(_, lhs, rhs) | Self::Ternary(lhs, _, rhs) => {
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
    LogicalNot,
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
    LogicalAnd,
    LogicalOr,
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

impl From<AssignOp> for BinaryOp {
    fn from(value: AssignOp) -> Self {
        match value {
            AssignOp::Eq => Self::Eq,
            AssignOp::Add => Self::Add,
            AssignOp::Sub => Self::Sub,
            AssignOp::Mul => Self::Mul,
            AssignOp::Div => Self::Div,
            AssignOp::Mod => Self::Mod,
            AssignOp::BitwiseAnd => Self::BitwiseAnd,
            AssignOp::BitwiseOr => Self::BitwiseOr,
            AssignOp::BitwiseXor => Self::BitwiseXor,
            AssignOp::ShiftLeft => Self::ShiftLeft,
            AssignOp::ShiftRight => Self::ShiftRight,
        }
    }
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    //
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

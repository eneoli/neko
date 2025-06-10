use crate::compile::ast::{SourcePos, Type};

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Type, String, SourcePos),
    Assign(String, Expr, SourcePos),
    Return(Expr, SourcePos),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    For(Option<Box<Stmt>>, Expr, Option<Box<Stmt>>, Box<Stmt>),
    While(Expr, Box<Stmt>),
    Break(SourcePos),
    Continue(SourcePos),
}

impl Stmt {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(u32, SourcePos),
    Bool(bool, SourcePos),
    Ident(String, SourcePos),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn span(&self) -> SourcePos {
        match self {
            Self::Int(_, span) | Self::Bool(_, span) | Self::Ident(_, span) => span.clone(),
            Self::Unary(_, expr) => {
                let SourcePos { start, end } = expr.span();
                (start - 1)..end
            }
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

use crate::compile::ast::{PhaseStmt, SourcePos, Type, desugared};

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Type, String, SourcePos),
    Assign(String, desugared::Expr, SourcePos),
    Return(desugared::Expr, SourcePos),
    Block(Vec<Stmt>),
    If(desugared::Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(desugared::Expr, Box<Stmt>),
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

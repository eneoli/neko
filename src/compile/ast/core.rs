use crate::compile::ast::{SourcePos, Type, desugared};

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

impl Stmt {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

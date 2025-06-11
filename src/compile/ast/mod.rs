use std::{fmt::Display, marker::PhantomData};

pub type SourcePos = ::core::ops::Range<usize>;

pub mod core;
pub mod desugared;
pub mod int_literal;
pub mod parsed;

#[derive(Clone, Debug)]
pub struct Parsed;

#[derive(Clone, Debug)]
pub struct Elaborated;

#[derive(Clone, Debug)]
pub struct TypeChecked;

#[derive(Clone, Debug)]
pub struct Core;

pub trait Phase {
    type Stmt: PhaseStmt;
    type Expr: PhaseExpr;
}

pub trait PhaseStmt {
    fn boxed(self) -> Box<Self>;
    fn span(&self) -> SourcePos;
}

pub trait PhaseExpr {
    fn boxed(self) -> Box<Self>;
    fn span(&self) -> SourcePos;
}

impl Phase for Parsed {
    type Stmt = parsed::Stmt;
    type Expr = parsed::Expr;
}

impl Phase for Elaborated {
    type Stmt = desugared::Stmt;
    type Expr = desugared::Expr;
}

impl Phase for TypeChecked {
    type Stmt = desugared::Stmt;
    type Expr = desugared::Expr;
}

impl Phase for Core {
    type Stmt = core::Stmt;
    type Expr = desugared::Expr;
}

#[derive(Clone, Debug)]
pub struct Ast<P>
where
    P: Phase,
{
    pub main: FunctionDecl<P::Stmt>,
    pub _marker: PhantomData<P>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Type {
    Int,
    Bool,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
        }
    }
}

pub type FunctionArgument = (String, Type);

#[derive(Debug, Clone)]
pub struct FunctionDecl<Stmt> {
    pub ty: Type,
    pub name: String,
    pub args: Vec<FunctionArgument>,
    pub body: Vec<Stmt>,
    pub span: SourcePos,
}

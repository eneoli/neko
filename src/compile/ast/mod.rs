use std::{fmt::Display, marker::PhantomData};

pub type SourcePos = ::core::ops::Range<usize>;

pub mod core;
pub mod desugared;
pub mod int_literal;
pub mod parsed;

pub struct Parsed;
pub struct Elaborated;
pub struct TypeChecked;
pub struct Core;
trait Phase {
    type Stmt;
    type Expr;
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

pub struct Ast<P>
where
    P: Phase,
{
    pub main: FunctionDecl<P::Stmt>,
    pub _marker: PhantomData<P>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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

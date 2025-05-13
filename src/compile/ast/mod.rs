use int_literal::IntLiteral;

pub mod int_literal;

pub type SourcePos = core::ops::Range<usize>;

#[derive(Debug, Clone)]
pub enum Type {
    INT,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(IntLiteral, SourcePos),
    Ident(String, SourcePos),
    Unary(Op, Box<Expr>),
    Binary(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Type, String, SourcePos),
    Init(Type, String, Expr, SourcePos),
    Assign(String, AssignOp, Expr, SourcePos),
    Return(Expr, SourcePos),
}

#[derive(Debug, Clone)]
pub enum Op {
    Mul,
    Add,
    Sub,
    Div,
    Neg,
    Mod,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Eq,
    Op(Op),
}

type FunctionArgument = (Type, String);

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub ty: Type,
    pub name: String,
    pub args: Vec<FunctionArgument>,
    pub body: Vec<Stmt>,
    pub src_pos: SourcePos,
}

#[derive(Debug, Clone)]
pub enum AST {
    Block(Vec<Stmt>, SourcePos),
}

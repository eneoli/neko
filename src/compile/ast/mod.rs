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
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Type, String, SourcePos),
    Init(Type, String, Expr, SourcePos),
    Assign(String, AssignOp, Expr, SourcePos),
    Return(Expr, SourcePos),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Mul,
    Add,
    Sub,
    Div,
    Mod,
}

#[derive(Debug, Clone)]
pub enum AssignOp {
    Eq,
    Op(BinaryOp),
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

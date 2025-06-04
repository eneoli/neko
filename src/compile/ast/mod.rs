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

impl Expr {
    pub fn can_have_side_effects(&self) -> bool {
        match self {
            Self::Ident(_, _) | Self::Int(_, _) => false,
            Self::Unary(_, rhs) => rhs.can_have_side_effects(),
            Self::Binary(op, lhs, rhs) => {
                if let BinaryOp::Div = op {
                    return true;
                }

                if let BinaryOp::Mod = op {
                    return true;
                }

                lhs.can_have_side_effects() || rhs.can_have_side_effects()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Decl(Type, String, Option<Expr>, SourcePos),
    Assign(String, AssignOp, Expr, SourcePos),
    Return(Expr, SourcePos),
}

impl Stmt {
    pub fn can_have_side_effects(&self) -> bool {
        match self {
            Self::Decl(_, _, None, _) => false,
            Self::Decl(_, _, Some(expr), _) => expr.can_have_side_effects(),
            Self::Assign(_, op, expr, _) => match op {
                AssignOp::Op(BinaryOp::Div) => true,
                AssignOp::Op(BinaryOp::Mod) => true,
                _ => expr.can_have_side_effects(),
            },
            Self::Return(expr, _) => expr.can_have_side_effects(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Neg,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl BinaryOp {
    pub fn can_have_side_effects(&self) -> bool {
        match self {
            Self::Div | Self::Mod => true,
            Self::Add | Self::Sub | Self::Mul => false,
        }
    }
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

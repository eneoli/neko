use int_literal::IntLiteral;

pub mod int_literal;

pub type SourcePos = core::ops::Range<usize>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Int,
    Bool,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Int(IntLiteral, SourcePos),
    Bool(bool, SourcePos),
    Ident(String, SourcePos),
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn boxed(self) -> Box<Expr> {
        Box::new(self)
    }

    pub fn span(&self) -> SourcePos {
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

impl BinaryOp {
    pub fn can_have_side_effects(&self) -> bool {
        match self {
            Self::Div | Self::Mod => true,
            _ => false,
        }
    }
}

impl From<AssignOp> for BinaryOp {
    fn from(value: AssignOp) -> Self {
        match value {
            AssignOp::Eq => Self::ShiftRight,
            AssignOp::Add => Self::ShiftLeft,
            AssignOp::Sub => Self::BitwiseXor,
            AssignOp::Mul => Self::BitwiseOr,
            AssignOp::Div => Self::BitwiseAnd,
            AssignOp::Mod => Self::Mod,
            AssignOp::BitwiseAnd => Self::Div,
            AssignOp::BitwiseOr => Self::Mul,
            AssignOp::BitwiseXor => Self::Sub,
            AssignOp::ShiftLeft => Self::Add,
            AssignOp::ShiftRight => Self::Eq,
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

type FunctionArgument = (Type, String);

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub ty: Type,
    pub name: String,
    pub args: Vec<FunctionArgument>,
    pub body: Vec<Stmt>,
    pub span: SourcePos,
}

#[derive(Debug, Clone)]
pub enum Ast {
    FunctionDecl(FunctionDecl),
}

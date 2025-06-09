use crate::compile::ast::SourcePos;

#[derive(Debug, Clone)]
pub enum Core {
    FunctionDecl {
        ty: CoreType,
        body: Vec<CoreStmt>,
        span: SourcePos,
    }
}

#[derive(Debug, Clone)]
pub enum CoreStmt {
    Decl(CoreType, String, SourcePos),
    Assign(String, CoreExpr, SourcePos),
    Return(CoreExpr, SourcePos),
    Block(Vec<CoreStmt>),
    If(CoreExpr, Box<CoreStmt>, Option<Box<CoreStmt>>),
    While(CoreExpr, Box<CoreStmt>),
    Break(SourcePos),
    Continue(SourcePos),
}

impl CoreStmt {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug, Clone)]
pub enum CoreType {
    Int,
    Bool,
}

#[derive(Debug, Clone)]
pub enum CoreExpr {
    Int(u32, SourcePos),
    Bool(bool, SourcePos),
    Ident(String, SourcePos),
    Unary(CoreUnaryOp, Box<CoreExpr>),
    Binary(CoreBinaryOp, Box<CoreExpr>, Box<CoreExpr>),
    Ternary(Box<CoreExpr>, Box<CoreExpr>, Box<CoreExpr>),
}

impl CoreExpr {
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug, Clone)]
pub enum CoreUnaryOp {
    Neg,
    BitwiseNot,
}

#[derive(Debug, Clone)]
pub enum CoreBinaryOp {
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

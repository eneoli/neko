use std::fmt::Display;

use crate::compile::ast::desugared::BinaryOp;

#[derive(Clone, Debug)]
pub struct BinaryNode(BinaryNodeOp);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryNodeType {
    Int,
    Bool,
}

#[derive(Clone, Debug)]
pub enum BinaryNodeOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

impl BinaryNodeOp {
    pub fn ty(&self) -> BinaryNodeType {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Mod
            | Self::BitwiseAnd
            | Self::BitwiseOr
            | Self::BitwiseXor
            | Self::ShiftLeft
            | Self::ShiftRight => BinaryNodeType::Int,

            Self::Eq
            | Self::NotEq
            | Self::Less
            | Self::LessEq
            | Self::Greater
            | Self::GreaterEq => BinaryNodeType::Bool,
        }
    }
}

impl From<BinaryOp> for BinaryNodeOp {
    fn from(value: BinaryOp) -> Self {
        match value {
            BinaryOp::Add => Self::Add,
            BinaryOp::Sub => Self::Sub,
            BinaryOp::Mul => Self::Mul,
            BinaryOp::Div => Self::Div,
            BinaryOp::Mod => Self::Mod,
            BinaryOp::BitwiseAnd => Self::BitwiseAnd,
            BinaryOp::BitwiseOr => Self::BitwiseOr,
            BinaryOp::BitwiseXor => Self::BitwiseXor,
            BinaryOp::ShiftLeft => Self::ShiftLeft,
            BinaryOp::ShiftRight => Self::ShiftRight,
            BinaryOp::Eq => Self::Eq,
            BinaryOp::NotEq => Self::NotEq,
            BinaryOp::Less => Self::Less,
            BinaryOp::LessEq => Self::LessEq,
            BinaryOp::Greater => Self::Greater,
            BinaryOp::GreaterEq => Self::GreaterEq,
        }
    }
}

impl Display for BinaryNodeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "BinaryOp(+)"),
            Self::Sub => write!(f, "BinaryOp(-)"),
            Self::Mul => write!(f, "BinaryOp(*)"),
            Self::Div => write!(f, "BinaryOp(/)"),
            Self::Mod => write!(f, "BinaryOp(%)"),
            Self::BitwiseAnd => write!(f, "BinaryOp(&)"),
            Self::BitwiseOr => write!(f, "BinaryOp(|)"),
            Self::BitwiseXor => write!(f, "BinaryOp(^)"),
            Self::ShiftLeft => write!(f, "BinaryOp(<<)"),
            Self::ShiftRight => write!(f, "BinaryOp(>>)"),
            Self::Eq => write!(f, "BinaryOp(=)"),
            Self::NotEq => write!(f, "BinaryOp(!=)"),
            Self::Less => write!(f, "BinaryOp(<)"),
            Self::LessEq => write!(f, "BinaryOp(<=)"),
            Self::Greater => write!(f, "BinaryOp(>)"),
            Self::GreaterEq => write!(f, "BinaryOp(>=)"),
        }
    }
}

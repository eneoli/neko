use scope_node::ScopeNode;

use super::types::Type;

pub mod scope_node;

pub type NodeId = usize;

pub enum NodeType {
    Integer,
}

pub enum UnaryNodeOp {
    Neg,
}

impl From<&crate::compile::ast::UnaryOp> for UnaryNodeOp {
    fn from(value: &crate::compile::ast::UnaryOp) -> Self {
        use crate::compile::ast::UnaryOp;

        match value {
            UnaryOp::Neg => UnaryNodeOp::Neg,
        }
    }
}

pub enum BinaryNodeOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl From<&crate::compile::ast::BinaryOp> for BinaryNodeOp {
    fn from(value: &crate::compile::ast::BinaryOp) -> Self {
        use crate::compile::ast::BinaryOp;

        match value {
            BinaryOp::Add => BinaryNodeOp::Add,
            BinaryOp::Sub => BinaryNodeOp::Sub,
            BinaryOp::Mul => BinaryNodeOp::Mul,
            BinaryOp::Div => BinaryNodeOp::Div,
            BinaryOp::Mod => BinaryNodeOp::Mod,
        }
    }
}

pub enum NodeKind {
    Start,
    Return,
    Constant(Type),

    // Expr
    Unary(UnaryNodeOp),
    Binary(BinaryNodeOp),
    Multi,

    Projection(usize), // Index into tuple
    Scope(ScopeNode),
}

// A single node in the Sea of Nodes
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
}

impl Node {
    pub fn start(id: NodeId) -> Self {
        Self {
            id,
            kind: NodeKind::Start,
        }
    }

    pub fn scope(id: NodeId) -> Self {
        Self {
            id,
            kind: NodeKind::Scope(ScopeNode::new(id)),
        }
    }
}

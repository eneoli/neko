use crate::compile::ir::sea::types::Type;

use super::{NodeId, scope_node::ScopeNode};

#[derive(Clone, Debug)]
pub enum UnaryNodeOp {
    Neg,
    BitwiseNot,
    LogicalNot,
}

impl UnaryNodeOp {
    pub fn has_side_effect(&self) -> bool {
        false
    }
}

impl From<&crate::compile::ast::desugared::UnaryOp> for UnaryNodeOp {
    fn from(value: &crate::compile::ast::desugared::UnaryOp) -> Self {
        use crate::compile::ast::desugared::UnaryOp;

        match value {
            UnaryOp::Neg => Self::Neg,
            UnaryOp::BitwiseNot => Self::BitwiseNot,
        }
    }
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
    LogicalAnd,
    LogicalOr,
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
    pub fn has_side_effect(&self) -> bool {
        match self {
            BinaryNodeOp::Div => true,
            BinaryNodeOp::Mod => true,
            _ => false,
        }
    }
}

impl From<&crate::compile::ast::desugared::BinaryOp> for BinaryNodeOp {
    fn from(value: &crate::compile::ast::desugared::BinaryOp) -> Self {
        use crate::compile::ast::desugared::BinaryOp;

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

#[derive(Clone, Debug)]
pub enum NodeKind {
    Start,
    Return {
        ctrl: NodeId,
        value: NodeId,
    },
    Constant {
        ty: Type,
        ctrl: NodeId,
    },

    Unary {
        op: UnaryNodeOp,
        rhs: NodeId,
    },
    Binary {
        op: BinaryNodeOp,
        lhs: NodeId,
        rhs: NodeId,
    },

    Multi,
    Projection(usize), // Index into tuple
    Scope(ScopeNode),
    Deleted,
}

impl NodeKind {
    pub fn inputs(&self) -> Vec<NodeId> {
        match self {
            NodeKind::Start => vec![],
            NodeKind::Constant { ctrl, .. } => vec![*ctrl],
            NodeKind::Return { ctrl, value } => vec![*ctrl, *value],
            NodeKind::Unary { rhs, .. } => vec![*rhs],
            NodeKind::Binary { lhs, rhs, .. } => vec![*lhs, *rhs],
            NodeKind::Multi | NodeKind::Projection(_) | NodeKind::Scope(_) => vec![],
            NodeKind::Deleted => vec![],
        }
    }

    pub fn inputs_mut(&mut self) -> Vec<&mut NodeId> {
        match self {
            NodeKind::Start => vec![],
            NodeKind::Constant { ctrl, .. } => vec![ctrl],
            NodeKind::Return { ctrl, value } => vec![ctrl, value],
            NodeKind::Unary { rhs, .. } => vec![rhs],
            NodeKind::Binary { lhs, rhs, .. } => vec![lhs, rhs],
            NodeKind::Multi | NodeKind::Projection(_) => vec![],
            NodeKind::Scope(scope) => scope.inputs_mut(),
            NodeKind::Deleted => vec![],
        }
    }

    pub fn reroute(&mut self, old_node_id: NodeId, new_node_id: NodeId) {
        for input in self.inputs_mut().into_iter() {
            if *input == old_node_id {
                *input = new_node_id;
            }
        }
    }
}

use crate::compile::ir::types::Type;

use super::{NodeId, scope_node::ScopeNode};

pub enum UnaryNodeOp {
    Neg,
}

impl UnaryNodeOp {
    pub fn has_side_effect(&self) -> bool {
        match self {
            UnaryNodeOp::Neg => false,
        }
    }
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

impl BinaryNodeOp {
    pub fn has_side_effect(&self) -> bool {
        match self {
            BinaryNodeOp::Add => false,
            BinaryNodeOp::Sub => false,
            BinaryNodeOp::Mul => false,
            BinaryNodeOp::Div => true,
            BinaryNodeOp::Mod => true,
        }
    }
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
            NodeKind::Binary { lhs, rhs, .. } => vec![*rhs, *lhs],
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
            NodeKind::Binary { lhs, rhs, .. } => vec![rhs, lhs],
            NodeKind::Multi | NodeKind::Projection(_) | NodeKind::Scope(_) => vec![],
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

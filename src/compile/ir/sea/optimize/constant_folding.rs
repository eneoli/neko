use crate::compile::ir::sea::{
    Sea,
    node::{
        NodeId,
        node_kind::{BinaryNodeOp, NodeKind, UnaryNodeOp},
    },
    types::Type,
};

use super::Optimizer;

pub struct ConstantFoldingOptimizer;

impl ConstantFoldingOptimizer {
    pub fn new() -> Self {
        Self
    }

    fn optimize_unary(&self, node_id: NodeId, sea: &mut Sea) -> NodeId {
        let node = sea.node(node_id);
        let NodeKind::Unary { ref op, rhs } = node.kind else {
            unreachable!()
        };

        if op.has_side_effect() {
            return node.id;
        }

        let rhs_node = sea.node(rhs);

        let NodeKind::Constant { ty, .. } = &rhs_node.kind else {
            return node.id;
        };

        let Type::Int(a) = ty else {
            return node.id;
        };

        // TODO not good
        let value = match op {
            UnaryNodeOp::Neg => i32::wrapping_neg(*a as i32),
            _ => todo!(),
        };

        let kind = NodeKind::Constant {
            ty: Type::Int(value as u32),
            ctrl: sea.start(),
        };

        sea.add_node(kind, node.effects.clone())
    }

    fn optimize_binary(&self, node_id: NodeId, sea: &mut Sea) -> NodeId {
        let node = sea.node(node_id);
        let NodeKind::Binary { ref op, lhs, rhs } = node.kind else {
            unreachable!()
        };

        let lhs_node = sea.node(lhs);
        let rhs_node = sea.node(rhs);

        let (NodeKind::Constant { ty: lhs_ty, .. }, NodeKind::Constant { ty: rhs_ty, .. }) =
            (&lhs_node.kind, &rhs_node.kind)
        else {
            return node.id;
        };

        let (Type::Int(a), Type::Int(b)) = (lhs_ty, rhs_ty) else {
            return node.id;
        };

        // assuming side effects come from div and mod only
        if op.has_side_effect() && i32::checked_div(*a as i32, *b as i32).is_none() {
            return node.id;
        }

        let value = match op {
            BinaryNodeOp::Add => i32::wrapping_add(*a as i32, *b as i32),
            BinaryNodeOp::Sub => i32::wrapping_sub(*a as i32, *b as i32),
            BinaryNodeOp::Mul => i32::wrapping_mul(*a as i32, *b as i32),
            BinaryNodeOp::Div => i32::wrapping_div(*a as i32, *b as i32),
            BinaryNodeOp::Mod => i32::wrapping_rem(*a as i32, *b as i32),
            _ => todo!(),
        };

        let kind = NodeKind::Constant {
            ty: Type::Int(value as u32),
            ctrl: sea.start(),
        };

        sea.add_node(kind, node.effects.clone())
    }
}

impl Optimizer for ConstantFoldingOptimizer {
    fn optimize(&self, node_id: NodeId, sea: &mut Sea) -> NodeId {
        let node = sea.node(node_id);
        match node.kind {
            NodeKind::Unary { .. } => self.optimize_unary(node_id, sea),
            NodeKind::Binary { .. } => self.optimize_binary(node_id, sea),
            _ => node_id,
        }
    }
}

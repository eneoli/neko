use crate::compile::ir::{
    node::{
        node_kind::{BinaryNodeOp, NodeKind, UnaryNodeOp}, NodeId
    },
    sea::Sea,
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

        let NodeKind::Constant { ty, .. } = &rhs_node.kind  else {
            return node.id;
        };

        let Type::Int(a) = ty else {
            return node.id;
        };

        let value = match op {
            UnaryNodeOp::Neg => -a,
        };

        let kind = NodeKind::Constant {
            ty: Type::Int(value),
            ctrl: sea.start(),
        };

        sea.add_node(kind)


    }

    fn optimize_binary(&self, node_id: NodeId, sea: &mut Sea) -> NodeId {
        let node = sea.node(node_id);
        let NodeKind::Binary { ref op, lhs, rhs } = node.kind else {
            unreachable!()
        };

        if op.has_side_effect() {
            return node.id;
        }

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

        let value = match op {
            BinaryNodeOp::Add => a + b,
            BinaryNodeOp::Sub => a - b,
            BinaryNodeOp::Mul => a * b,
            _ => unreachable!(),
        };

        let kind = NodeKind::Constant {
            ty: Type::Int(value),
            ctrl: sea.start(),
        };

        sea.add_node(kind)
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

use crate::compile::ir::graph::{
    IrGraph, NodeId,
    node::{
        Node,
        binary::{BinaryNodeOp, BinaryNodeType},
        constant::ConstantNode,
    },
    optimization::local::LocalOptimizer,
};

pub struct ConstantFoldingOptimizer;

impl ConstantFoldingOptimizer {
    pub fn new() -> Self {
        Self
    }

    fn optimize_binary(&self, node_id: NodeId, ir: &mut IrGraph) -> NodeId {
        let node = ir.node(node_id).unwrap();
        let Node::Binary(op) = node else {
            unreachable!()
        };

        let lhs_node = ir.node(ir.lhs(node_id)).unwrap();
        let rhs_node = ir.node(ir.rhs(node_id)).unwrap();

        let (Node::Constant(lhs), Node::Constant(rhs)) = (lhs_node, rhs_node) else {
            return node_id;
        };

        if self.effectful(op, lhs, rhs) {
            return node_id;
        }

        let value = match (lhs, rhs) {
            (ConstantNode::Int(a), ConstantNode::Int(b)) => self.simplify_int(op, *a, *b),
            (ConstantNode::Bool(a), ConstantNode::Bool(b)) => self.simplify_bool(op, *a, *b),
            _ => return node_id,
        };

        ir.add_to_start(Node::Constant(value))
    }

    fn effectful(&self, op: &BinaryNodeOp, lhs: &ConstantNode, rhs: &ConstantNode) -> bool {
        if !matches!(op, BinaryNodeOp::Div) && !matches!(op, BinaryNodeOp::Mod) {
            return false;
        }

        let ConstantNode::Int(lhs) = lhs else {
            unreachable!("Error in semantical analysis")
        };

        let ConstantNode::Int(rhs) = rhs else {
            unreachable!("Error in semantical analysis")
        };

        i32::checked_div(*lhs as i32, *rhs as i32).is_none()
    }

    fn simplify_int(&self, op: &BinaryNodeOp, a: u32, b: u32) -> ConstantNode {
        if op.ty() == BinaryNodeType::Int {
            let value = match op {
                BinaryNodeOp::Add => i32::wrapping_add(a as i32, b as i32),
                BinaryNodeOp::Sub => i32::wrapping_sub(a as i32, b as i32),
                BinaryNodeOp::Mul => i32::wrapping_mul(a as i32, b as i32),
                BinaryNodeOp::Div => i32::wrapping_div(a as i32, b as i32),
                BinaryNodeOp::Mod => i32::wrapping_rem(a as i32, b as i32),
                BinaryNodeOp::BitwiseAnd => (a as i32) & (b as i32),
                BinaryNodeOp::BitwiseOr => (a as i32) | (b as i32),
                BinaryNodeOp::BitwiseXor => (a as i32) ^ (b as i32),
                BinaryNodeOp::ShiftLeft => (a as i32) << (b as i32),
                BinaryNodeOp::ShiftRight => (a as i32) >> (b as i32),
                _ => unreachable!(),
            } as u32;

            return ConstantNode::Int(value);
        }

        let value = match op {
            BinaryNodeOp::Eq => a == b,
            BinaryNodeOp::NotEq => a != b,
            BinaryNodeOp::Less => a < b,
            BinaryNodeOp::LessEq => a <= b,
            BinaryNodeOp::Greater => a > b,
            BinaryNodeOp::GreaterEq => a >= b,
            _ => unreachable!(),
        };

        ConstantNode::Bool(value)
    }

    fn simplify_bool(&self, op: &BinaryNodeOp, a: bool, b: bool) -> ConstantNode {
        match op {
            BinaryNodeOp::Eq => ConstantNode::Bool(a == b),
            BinaryNodeOp::NotEq => ConstantNode::Bool(a != b),
            _ => unreachable!(),
        }
    }
}

impl LocalOptimizer for ConstantFoldingOptimizer {
    fn optimize(&self, node_id: NodeId, ir: &mut IrGraph) -> NodeId {
        let node = ir.node(node_id).unwrap();
        match node {
            Node::Binary(_) => self.optimize_binary(node_id, ir),
            _ => node_id,
        }
    }
}

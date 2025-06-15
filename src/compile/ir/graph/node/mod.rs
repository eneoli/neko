use crate::compile::ir::graph::node::{binary::BinaryNodeOp, constant::ConstantNode};

pub mod binary;
pub mod constant;

#[derive(Clone, Debug)]
pub enum Node {
    Block,
    Phi,
    Undef,
    Return,
    Constant(ConstantNode),
    Binary(BinaryNodeOp),
    Ternary,
    Jump,
    CondJump,
}

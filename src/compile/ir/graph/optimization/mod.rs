use crate::compile::ir::graph::{IrGraph, NodeId, optimization::local::LocalOptimization};

pub mod global;
pub mod local;

pub struct Optimization {
    local: LocalOptimization,
}

impl Optimization {
    pub fn new() -> Self {
        Self {
            local: LocalOptimization::new(),
        }
    }

    pub fn local(&self, id: NodeId, ir: &mut IrGraph) -> NodeId {
        self.local.optimize(id, ir)
    }
}

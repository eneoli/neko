use constant_folding::ConstantFoldingOptimizer;

use crate::compile::ir::graph::{IrGraph, NodeId};

pub mod constant_folding;

pub trait LocalOptimizer {
    fn optimize(&self, node_id: NodeId, ir: &mut IrGraph) -> NodeId;
}

pub struct LocalOptimization {
    optimizers: Vec<Box<dyn LocalOptimizer>>,
}

impl LocalOptimization {
    pub fn new() -> Self {
        Self {
            optimizers: vec![Box::new(ConstantFoldingOptimizer::new())],
        }
    }

    pub fn optimize(&self, node_id: NodeId, ir: &mut IrGraph) -> NodeId {
        let mut node_id = node_id;
        for optimizer in self.optimizers.iter() {
            let new_node_id = optimizer.optimize(node_id, ir);

            // todo optimize before adding node
            if new_node_id != node_id {
                self.garbage_collect(node_id, new_node_id, ir);
                node_id = new_node_id;
            }
        }

        node_id
    }

    fn garbage_collect(&self, old_node_id: NodeId, new_node_id: NodeId, ir: &mut IrGraph) {
        ir.reroute(old_node_id, new_node_id);
        ir.remove_node(old_node_id);
    }
}

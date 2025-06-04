use constant_folding::ConstantFoldingOptimizer;

use super::{node::NodeId, sea::Sea};

pub mod constant_folding;

pub trait Optimizer {
    fn optimize(&self, node_id: NodeId, sea: &mut Sea) -> NodeId;
}

pub struct Optimization {
    optimizers: Vec<Box<dyn Optimizer>>,
}

impl Optimization {
    pub fn new() -> Self {
        Self {
            optimizers: vec![Box::new(ConstantFoldingOptimizer::new())],
        }
    }

    pub fn optimize(&self, node_id: NodeId, sea: &mut Sea) -> NodeId {
        let mut node_id = node_id;
        for optimizer in self.optimizers.iter() {
            let new_node_id = optimizer.optimize(node_id, sea);

            // todo optimize before adding node
            if new_node_id != node_id {
                self.garbage_collect(node_id, new_node_id, sea);
                node_id = new_node_id;
            }
        }

        node_id
    }

    fn garbage_collect(&self, old_node_id: NodeId, new_node_id: NodeId, sea: &mut Sea) {
        // reroute users of node to new node
        let users = sea.outputs[old_node_id].clone();
        for user in users.iter() {
            sea.node_mut(*user).reroute(old_node_id, new_node_id);
        }

        // add effects
        sea.nodes[new_node_id].effects = sea.nodes[old_node_id].effects.clone(); // TODO ok to just override and not merge?
        sea.nodes[old_node_id].effects = vec![];

        sea.delete_node(old_node_id);
    }
}

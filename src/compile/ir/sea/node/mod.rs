use node_kind::NodeKind;
use scope_node::ScopeNode;

pub mod node_kind;
pub mod scope_node;

pub type NodeId = usize;

// A single node in the Sea of Nodes
#[derive(Clone, Debug)]
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
    pub effects: Vec<NodeId>,
}

impl Node {
    pub fn start(id: NodeId) -> Self {
        Self {
            id,
            kind: NodeKind::Start,
            effects: Vec::new(),
        }
    }

    pub fn scope(id: NodeId) -> Self {
        Self {
            id,
            kind: NodeKind::Scope(ScopeNode::new(id)),
            effects: Vec::new(),
        }
    }

    pub fn inputs(&self) -> Vec<NodeId> {
        vec![self.effects.clone(), self.kind.inputs()].concat()
    }

    pub fn reroute(&mut self, old_node_id: NodeId, new_node_id: NodeId) {
        if self.effects.contains(&old_node_id) {
            self.effects.retain(|v| *v != old_node_id);
        }

        if !self.effects.contains(&new_node_id) {
            self.effects.push(new_node_id);
        }

        self.kind.reroute(old_node_id, new_node_id);
    }
}

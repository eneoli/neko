use node_kind::NodeKind;
use scope_node::ScopeNode;

pub mod node_kind;
pub mod scope_node;

pub type NodeId = usize;

// A single node in the Sea of Nodes
pub struct Node {
    pub id: NodeId,
    pub kind: NodeKind,
}

impl Node {
    pub fn start(id: NodeId) -> Self {
        Self {
            id,
            kind: NodeKind::Start,
        }
    }

    pub fn scope(id: NodeId) -> Self {
        Self {
            id,
            kind: NodeKind::Scope(ScopeNode::new(id)),
        }
    }

    pub fn inputs(&self) -> Vec<NodeId> {
        self.kind.inputs()
    }

    pub fn reroute(&mut self, old_node_id: NodeId, new_node_id: NodeId) {
        self.kind.reroute(old_node_id, new_node_id)
    }
}

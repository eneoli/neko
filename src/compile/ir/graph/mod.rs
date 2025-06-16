use std::collections::HashMap;

use crate::compile::ir::graph::node::Node;

pub mod export;
pub mod node;
pub mod optimization;
pub mod ssa_translation;

pub type BlockId = NodeId;
pub type NodeId = usize;

pub struct IrGraph {
    next_id: NodeId,
    start: usize,
    end: usize,
    nodes: HashMap<NodeId, Node>,
    block: HashMap<NodeId, NodeId>,
    block_exit: HashMap<BlockId, NodeId>,
    predecessors: HashMap<NodeId, Vec<NodeId>>,
    successors: HashMap<NodeId, Vec<NodeId>>,
}

impl IrGraph {
    pub fn new() -> Self {
        let mut graph = Self {
            next_id: 0,
            start: 0,
            end: 0,
            nodes: HashMap::new(),
            block: HashMap::new(),
            block_exit: HashMap::new(),
            predecessors: HashMap::new(),
            successors: HashMap::new(),
        };

        let start = graph.add_block();
        graph.start = start;

        let end = graph.add_block();
        graph.end = end;

        graph
    }

    pub fn start(&self) -> NodeId {
        self.start
    }

    pub fn end(&self) -> NodeId {
        self.end
    }

    pub fn blocks(&self) -> Vec<NodeId> {
        let mut blocks: Vec<_> = self.block.values().copied().collect();

        blocks.sort();
        blocks.dedup();

        blocks
    }

    pub fn block(&self, id: NodeId) -> NodeId {
        self.block[&id]
    }

    pub fn is_block(&self, id: NodeId) -> bool {
        self.block[&id] == id
    }

    pub fn add_block(&mut self) -> NodeId {
        let id = self.advance_id();
        self.nodes.insert(id, Node::Block);
        self.block.insert(id, id);
        self.predecessors.insert(id, vec![]);
        self.successors.insert(id, vec![]);

        id
    }

    pub fn block_exit(&self, id: BlockId) -> Option<NodeId> {
        self.block_exit.get(&id).copied()
    }

    pub fn set_block_exit(&mut self, block: BlockId, exit: NodeId) {
        self.block_exit.insert(block, exit);
    }

    pub fn node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(&id)
    }

    pub fn node_mut(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(&id)
    }

    pub fn add_node(&mut self, block: NodeId, node: Node) -> NodeId {
        let id = self.advance_id();
        self.nodes.insert(id, node);
        self.block.insert(id, block);
        self.predecessors.insert(id, vec![]);
        self.successors.insert(id, vec![]);

        id
    }

    pub fn add_to_start(&mut self, node: Node) -> NodeId {
        let id = self.add_node(self.start, node);

        id
    }

    pub fn change_node(&mut self, id: NodeId, node: Node) {
        debug_assert!(
            self.nodes.contains_key(&id),
            "Node with this id does not exist"
        );

        self.nodes.insert(id, node);
    }

    pub fn remove_node(&mut self, id: NodeId) {
        debug_assert_eq!(
            0,
            self.successors[&id].len(),
            "The node ({id}) is still used. You really don't want to remove it."
        );

        for pred in self.predecessors[&id].iter() {
            self.successors.get_mut(pred).unwrap().retain(|x| *x != id);
        }

        self.block.remove(&id);
        self.predecessors.remove(&id);
        self.successors.remove(&id);
        self.nodes.remove(&id);
    }

    pub fn reroute(&mut self, old: NodeId, new: NodeId) {
        let users = self.successors[&old].clone();
        for user in users {
            for pred in self.predecessors.get_mut(&user).unwrap().iter_mut() {
                if *pred == old {
                    *pred = new;
                }
            }

            if !self.successors[&new].contains(&user) {
                self.successors.get_mut(&new).unwrap().push(user);
            }
        }
        *self.successors.get_mut(&old).unwrap() = vec![];
    }

    pub fn predecessors(&self, node: NodeId) -> Vec<NodeId> {
        self.predecessors[&node].clone()
    }

    pub fn add_predecessor(&mut self, node: NodeId, predecessor: NodeId) {
        let predecessors = self.predecessors.get_mut(&node).unwrap();
        predecessors.push(predecessor);

        let successors = self.successors.get_mut(&predecessor).unwrap();
        successors.push(node);
    }

    pub fn successors(&self, node: NodeId) -> Vec<NodeId> {
        self.successors[&node].clone()
    }

    pub fn add_successor(&mut self, node: NodeId, successor: NodeId) {
        self.add_predecessor(successor, node);
    }

    pub fn lhs(&self, id: NodeId) -> NodeId {
        self.predecessors[&id][0]
    }

    pub fn rhs(&self, id: NodeId) -> NodeId {
        if self.predecessors[&id].len() == 1 {
            return self.predecessors[&id][0];
        }

        self.predecessors[&id][1]
    }

    pub fn side_effect(&self, id: NodeId) -> Option<NodeId> {
        let node = self.node(id).unwrap();
        match node {
            Node::Return => {
                let preds = self.predecessors(id);
                debug_assert!(preds.len() == 1 || preds.len() == 2);

                if preds.len() == 1 {
                    return None;
                }

                Some(preds[1])
            }
            Node::Jump => {
                let preds = self.predecessors(id);
                debug_assert!(preds.len() == 0 || preds.len() == 1);

                if preds.len() == 0 {
                    return None;
                }

                Some(preds[0])
            }
            Node::CondJump => {
                let preds = self.predecessors(id);
                debug_assert!(preds.len() == 0 || preds.len() == 1);

                if preds.len() == 0 {
                    return None;
                }

                Some(preds[1])
            }
            Node::Phi => None, // TODO?
            Node::Binary(_) => {
                let preds = self.predecessors(id);
                debug_assert!(preds.len() == 2 || preds.len() == 3);

                if preds.len() == 2 {
                    return None;
                }

                Some(preds[2])
            }
            Node::Ternary => None, // TODO
            Node::Block => None,
            Node::Undef => None,
            Node::Constant(_) => {
                let preds = self.predecessors(id);
                debug_assert!(preds.len() == 0 || preds.len() == 1);

                if preds.len() == 0 {
                    return None;
                }

                Some(preds[0])

            },
        }
    }

    fn advance_id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id = self.next_id + 1;
        id
    }
}

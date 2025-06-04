use crate::compile::ast::AST;

use super::{
    node::{Node, NodeId, node_kind::NodeKind, scope_node::ScopeNode},
    ssa_translation,
};

// This represents a Sea of Nodes Graph
#[derive(Clone, Debug)]
pub struct Sea {
    // Indexed by NodeId
    pub nodes: Vec<Node>,

    // Adjacency list of the nodes a node is an input for.
    // Indexed by NodeId
    pub outputs: Vec<Vec<NodeId>>,

    start: NodeId,
    end: Option<NodeId>, // Return Node for now
    scope: NodeId,
}

impl Sea {
    pub fn new() -> Self {
        let start = Node::start(0);
        let scope = Node::scope(1);

        Self {
            nodes: vec![start, scope],
            outputs: vec![vec![], vec![]],
            start: 0,
            end: None,
            scope: 1,
        }
    }

    pub fn from_ast(ast: &AST) -> Self {
        ssa_translation::from_ast_to_sea_of_nodes(ast)
    }

    /////////////////////////////
    // Graph Utility Functions //
    ////////////////////////////

    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id]
    }

    pub fn node_mut(&mut self, id: NodeId) -> &mut Node {
        &mut self.nodes[id]
    }

    pub fn start(&self) -> NodeId {
        self.start
    }

    pub fn end(&self) -> Option<NodeId> {
        self.end
    }

    pub fn scope(&self) -> NodeId {
        self.scope
    }

    pub fn add_node(&mut self, kind: NodeKind, effects: Vec<NodeId>) -> NodeId {
        let id = self.next_id();
        let inputs = kind.inputs();

        self.nodes.push(Node { id, kind, effects });
        self.outputs.push(Vec::new());

        // add output edges
        for input in inputs.iter() {
            if !self.outputs[*input].contains(&id) {
                self.outputs[*input].push(id);
            }
        }

        for effect in self.nodes[id].effects.iter() {
            if !self.outputs[*effect].contains(&id) {
                self.outputs[*effect].push(id);
            }
        }

        // check if return node
        if let NodeKind::Return { .. } = self.nodes[id].kind {
            if self.end.is_some() {
                panic!("Second return?");
            }
            self.end = Some(id);
        }

        id
    }

    pub fn add_output_edge(&mut self, _use: NodeId, def: NodeId) {
        if !self.outputs[_use].contains(&def) {
            self.outputs[_use].push(def);
        }
    }

    // Expects the node is not used anywhere anymore.
    pub fn delete_node(&mut self, node_id: NodeId) {
        self.outputs[node_id] = vec![];

        let node = &mut self.nodes[node_id];

        let mut delete_inputs = vec![];
        for input in node.inputs() {
            self.outputs[input].retain(|x| *x != node.id);

            // remove node if no users
            // if self.outputs[input].len() == 0 {
            //     delete_inputs.push(input);
            // }
        }

        node.kind = NodeKind::Deleted;

        for input in delete_inputs.into_iter() {
            self.delete_node(input);
        }
    }

    fn next_id(&self) -> NodeId {
        self.nodes.len()
    }

    ////////////////////////////
    // Scope Utility Functions //
    ////////////////////////////

    pub fn lookup(&self, name: &String) -> Option<NodeId> {
        let NodeKind::Scope(ref scope_node) = self.nodes[self.scope].kind else {
            panic!("not a scope node ^~^")
        };

        scope_node.lookup(name)
    }

    pub fn define(&mut self, name: String, value: NodeId) -> Result<(), ()> {
        let current_scope = self.scope_node_mut().current_scope_mut();

        if current_scope.contains_key(&name) {
            return Err(());
        }

        self.scope_node_mut()
            .current_scope_mut()
            .insert(name, value);
        self.add_output_edge(self.scope, value);

        Ok(())
    }

    pub fn write(&mut self, name: String, value: NodeId) {
        self.scope_node_mut().write(name, value);
    }

    ////////////////////////////////
    // Accessor Utility Functions //
    ////////////////////////////////

    fn scope_node(&mut self) -> &ScopeNode {
        let NodeKind::Scope(ref scope_node) = self.nodes[self.scope].kind else {
            panic!("Expected scope")
        };

        scope_node
    }

    fn scope_node_mut(&mut self) -> &mut ScopeNode {
        let NodeKind::Scope(ref mut scope_node) = self.nodes[self.scope].kind else {
            panic!("Expected scope")
        };

        scope_node
    }
}

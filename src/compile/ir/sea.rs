// This represents a Sea of Nodes Graph

use crate::compile::ast::AST;

use super::{
    node::{Node, NodeId, NodeKind, scope_node::ScopeNode},
    ssa_translation,
};

pub struct Sea {
    // Indexed by NodeId
    nodes: Vec<Node>,

    // We use an edge list representation of the graph
    // Index is a NodeId and returns the Vec containing the neighbours of the node.

    // inputs:  Edges to inputs of a node
    // outputs: Edges to nodes to which the node is an input
    // invariant: both inputs and outputs represent the same undirected graph.
    // Use add_node() and add_edge()
    pub inputs: Vec<Vec<NodeId>>,
    pub outputs: Vec<Vec<NodeId>>,

    start: NodeId,

    scope: NodeId,
}

impl Sea {
    pub fn new() -> Self {
        let start = Node::start(0);
        let scope = Node::scope(1);

        Self {
            nodes: vec![start, scope],
            inputs: vec![vec![], vec![]],
            outputs: vec![vec![], vec![]],
            start: 0,
            scope: 1,
        }
    }

    pub fn from_ast(ast: &AST) -> Self {
        ssa_translation::from_ast_to_sea_of_nodes(ast)
    }

    pub fn node(&self, id: NodeId) -> &Node {
        &self.nodes[id]
    }

    pub fn define(&mut self, name: String, value: NodeId) -> Result<(), ()> {
        let current_scope = self.scope_node_mut().current_scope_mut();

        if current_scope.contains_key(&name) {
            return Err(());
        }

        let (use_ref, _) = self.add_edge(self.scope, value);
        self.scope_node_mut()
            .current_scope_mut()
            .insert(name, use_ref);

        Ok(())
    }

    pub fn write(&mut self, name: String, value: NodeId) {
        self.scope_node_mut().write(name, value);
    }

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

    pub fn lookup(&self, name: &String) -> Option<NodeId> {
        let NodeKind::Scope(ref scope_node) = self.nodes[self.scope].kind else {
            panic!("not a scope node ^~^")
        };

        scope_node.lookup(name, self)
    }

    // Add a definition node to Start
    pub fn add_def(&mut self, kind: NodeKind) -> NodeId {
        let id = self.add_node(kind, vec![]);
        self.add_edge(self.start, id);

        id
    }

    pub fn add_node(&mut self, kind: NodeKind, inputs: Vec<NodeId>) -> NodeId {
        let id = self.next_id();

        self.nodes.push(Node { id, kind });

        self.inputs.push(Vec::new());
        self.outputs.push(Vec::new());

        for input in inputs.iter() {
            self.add_edge(id, *input);
        }

        id
    }

    // Returns the edge index of both incidence lists
    pub fn add_edge(&mut self, _use: NodeId, def: NodeId) -> (usize, usize) {
        if !self.inputs[_use].contains(&def) {
            self.inputs[_use].push(def);
        }

        let use_ref = self.inputs[_use]
            .iter()
            .enumerate()
            .find(|(_, x)| **x == def)
            .map(|(i, _)| i)
            .unwrap();

        if !self.outputs[def].contains(&_use) {
            self.outputs[def].push(_use);
        }

        let def_ref = self.outputs[def]
            .iter()
            .enumerate()
            .find(|(_, x)| **x == _use)
            .map(|(i, _)| i)
            .unwrap();

        (use_ref, def_ref)
    }

    fn next_id(&self) -> NodeId {
        self.nodes.len()
    }
}

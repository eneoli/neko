// This represents the datastructure concerned with managing scopes.
// We model it as a node to leverage the Sea of Nodes architecture.
// Each graph should have such a node (multiple possible?).

use std::collections::HashMap;

use crate::compile::ir::sea::Sea;

use super::NodeId;

#[derive(Clone, Debug)]
pub struct ScopeNode {
    pub id: NodeId,
    pub scopes: Vec<HashMap<String, NodeId>>,
}

impl ScopeNode {
    // represents under what control node the scope is evaluated.
    pub const CTRL: &'static str = "$ctrl";

    pub fn new(id: NodeId) -> Self {
        ScopeNode {
            id,
            scopes: vec![
                HashMap::new(), // control tokens like $ctrl
                HashMap::new(), // global scope
            ],
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn lookup(&self, name: &String) -> Option<NodeId> {
        let scope = self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.contains_key(name));

        if let Some(scope) = scope {
            let idx = *scope.get(name).unwrap();
            return Some(idx);
        }

        None
    }

    pub fn define(&mut self, name: String, value: NodeId, sea: &mut Sea) -> Result<(), ()> {
        let scope_id = self.id;
        let current_scope = self.current_scope_mut();

        if current_scope.contains_key(&name) {
            return Err(());
        }

        current_scope.insert(name, value);
        sea.add_output_edge(scope_id, value);

        Ok(())
    }

    pub fn write(&mut self, name: String, value: NodeId) {
        let Some(scope) = self.defining_scope_mut(&name) else {
            panic!("Tried to access undeclared variable \"{name}\", error in semantical analysis?")
        };

        scope.insert(name, value);
    }

    pub fn inputs(&self) -> Vec<NodeId> {
        let mut inputs = vec![];
        for scope in self.scopes.iter() {
            let mut scope_inputs = scope.values().into_iter().cloned().collect::<Vec<usize>>();

            inputs.append(&mut scope_inputs);
        }

        inputs
    }

    pub fn inputs_mut(&mut self) -> Vec<&mut NodeId> {
        let mut inputs: Vec<&mut NodeId> = vec![];
        for scope in self.scopes.iter_mut() {
            let mut scope_inputs = scope.values_mut().into_iter().collect();

            inputs.append(&mut scope_inputs);
        }

        inputs
    }

    ////////////////////////////////
    // Accessor Utility Functions //
    ///////////////////////////////

    pub fn current_scope(&self) -> &HashMap<String, usize> {
        let idx = self.scopes.len() - 1;
        &self.scopes[idx]
    }

    pub fn current_scope_mut(&mut self) -> &mut HashMap<String, usize> {
        let idx = self.scopes.len() - 1;
        &mut self.scopes[idx]
    }

    pub fn defining_scope(&self, name: &String) -> Option<&HashMap<String, usize>> {
        let scope = self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.contains_key(name));

        scope
    }

    pub fn defining_scope_mut(&mut self, name: &String) -> Option<&mut HashMap<String, usize>> {
        let scope = self
            .scopes
            .iter_mut()
            .rev()
            .find(|scope| scope.contains_key(name));

        scope
    }
}

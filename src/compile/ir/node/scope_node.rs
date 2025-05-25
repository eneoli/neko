// This represents the datastructure concerned with managing scopes.
// We model it as a node to leverage the Sea of Nodes architecture.
// Each graph should have such a node (multiple possible?).

use std::collections::HashMap;

use crate::compile::ir::sea::Sea;

use super::NodeId;

pub struct ScopeNode {
    pub id: NodeId,

    // A stack of scopes.
    // Each scope is a map of variable names to integers, being indicies
    // into the inputs of the ScopeNode
    // To access the Node associated with a name, first the symbol table is consulted, and then
    // the ScopeNode's inputs to find the required Node.
    pub scopes: Vec<HashMap<String, usize>>,
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

    pub fn lookup(&self, name: &String, sea: &Sea) -> Option<NodeId> {
        let scope = self
            .scopes
            .iter()
            .rev()
            .find(|scope| scope.contains_key(name));

        if let Some(scope) = scope {
            let idx = *scope.get(name).unwrap();
            return Some(sea.inputs[self.id][idx]);
        }

        None
    }

    pub fn define(&mut self, name: String, value: NodeId, sea: &mut Sea) -> Result<(), ()> {
        let scope_id = self.id;
        let current_scope = self.current_scope_mut();

        if current_scope.contains_key(&name) {
            return Err(());
        }

        let (use_ref, _) = sea.add_edge(scope_id, value);
        current_scope.insert(name, use_ref);

        Ok(())
    }

    pub fn write(&mut self, name: String, value: NodeId) {
        let Some(scope) = self.defining_scope_mut(&name) else {
            panic!("Tried to access undeclared variable \"{name}\", errror in semantical analysis?")
        };

        scope.insert(name, value);
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

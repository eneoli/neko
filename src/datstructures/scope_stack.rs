use std::{collections::HashMap, hash::Hash};

use thiserror::Error;

#[derive(Error, Clone, Debug)]
pub enum ScopeError {
    #[error("The stack is empty")]
    EmptyStack,

    #[error("The stack is not deep enough")]
    StackNotDeepEnough,
}

#[derive(Clone, Debug)]
pub struct ScopeStack<K, V> {
    scopes: Vec<HashMap<K, V>>,
}

impl<K: Eq + Hash, V> ScopeStack<K, V> {
    pub fn new(num_scopes: usize) -> Self {
        let mut scopes = Vec::with_capacity(num_scopes);
        for _ in 0..num_scopes {
            scopes.push(HashMap::new());
        }

        ScopeStack { scopes }
    }

    pub fn push(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    pub fn lookup(&mut self, key: &K) -> Option<&V> {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(key) {
                return scope.get(key);
            }
        }

        None
    }

    pub fn insert(&mut self, k: K, v: V) -> Result<Option<V>, ScopeError> {
        if self.scopes.len() == 0 {
            return Err(ScopeError::EmptyStack);
        }

        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(&k) {
                return Ok(scope.insert(k, v));
            }
        }

        let scope = self.scopes.last_mut().unwrap();
        Ok(scope.insert(k, v))
    }

    pub fn insert_at(&mut self, depth: usize, k: K, v: V) -> Result<Option<V>, ScopeError> {
        if self.scopes.len() == 0 {
            return Err(ScopeError::EmptyStack);
        }

        if depth > self.depth() - 1 {
            return Err(ScopeError::StackNotDeepEnough);
        }

        Ok(self.scopes[depth].insert(k, v)) 
    }


    pub fn insert_in_current(&mut self, k: K, v: V) -> Result<Option<V>, ScopeError> {
        self.insert_at(self.depth() - 1, k, v)
    }

    pub fn elements(&self) -> impl Iterator<Item = (&K, &V)> {
        self.scopes
            .iter()
            .flat_map(|m| m.iter())
    }

    pub fn elements_at(&self, depth: usize) -> impl Iterator<Item = (&K, &V)> {
        self.scopes[depth].iter()
    }

    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    pub fn current(&self) -> Option<&HashMap<K, V>> {
        self.scopes.last()
    }
}

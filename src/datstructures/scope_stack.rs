use std::{collections::HashMap, hash::Hash};

pub enum ScopeError {
    EmptyStack,
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

        let scope = self.scopes.last_mut().unwrap();
        Ok(scope.insert(k, v))
    }
}

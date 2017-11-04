// Licensed under MIT. See LICENSE for details.

use std::collections::HashMap;

use types::*;

#[derive(Clone, Debug)]
pub struct State {
    vars: HashMap<Ident, Value>,
}

impl State {
    pub fn new() -> Self {
        Self { vars: HashMap::new() }
    }

    pub fn get(&self, id: &Ident) -> Option<&Value> {
        self.vars.get(id)
    }

    pub fn extend(&self, id: Ident, val: Value) -> Self {
        let mut vars = self.vars.clone();
        vars.insert(id.clone(), val);
        Self { vars }
    }

    pub fn merge(&self, other: &Self) -> Self {
        let mut vars = self.vars.clone();
        for (id, val) in &other.vars {
            vars.insert(id.clone(), *val);
        }
        Self { vars }
    }

    pub fn contradicts(&self, other: &Self) -> bool {
        for (id, val) in &self.vars {
            match other.vars.get(id) {
                Some(ov) if val != ov => return true,
                _ => { /* no conflict */ }
            }
        }

        false
    }
}

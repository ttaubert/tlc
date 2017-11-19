// Licensed under MIT. See LICENSE for details.

use std::collections::BTreeMap;

use types::Ident;
use value::Value;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct State {
    vars: BTreeMap<Ident, Value>,
}

impl State {
    pub fn new() -> Self {
        Self { vars: BTreeMap::new() }
    }

    pub fn has(&self, id: &Ident) -> bool {
        self.vars.contains_key(id)
    }

    pub fn get(&self, id: &Ident) -> Option<&Value> {
        self.vars.get(id)
    }

    pub fn extend(&self, id: Ident, val: Value) -> Self {
        let mut vars = self.vars.clone();
        vars.insert(id.clone(), val);
        Self { vars }
    }

    pub fn merge_with(&self, other: &Self) -> Self {
        let mut vars = self.vars.clone();
        for (id, val) in &other.vars {
            vars.insert(id.clone(), val.clone());
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

    pub fn merge(a: Vec<State>, b: Vec<State>) -> Vec<State> {
        let mut rv = vec![];
        for sta in a {
            for stb in &b {
                if !sta.contradicts(stb) {
                    rv.push(sta.merge_with(stb))
                }
            }
        }
        rv
    }
}

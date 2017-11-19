// Licensed under MIT. See LICENSE for details.

use std::cmp::Ordering;
use std::collections::BTreeSet;

// Value types.
#[derive(Clone, Debug, Hash)]
pub enum Value {
    Boolean(bool),
    Number(u64),
    Set(BTreeSet<Value>),
    String(String),
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match *self {
            Value::Boolean(val) => val,
            _ => panic!("can't convert {:?} to bool", self),
        }
    }

    pub fn contains(&self, val: &Value) -> bool {
        match *self {
            Value::Set(ref set) => set.contains(val),
            _ => panic!("contains() not implemented for {:?}", self),
        }
    }

    pub fn values(&self) -> Vec<&Value> {
        match *self {
            Value::Set(ref set) => set.iter().collect(),
            _ => panic!("values() not implemented for {:?}", self),
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Value) -> Ordering {
        match (self, other) {
            (&Value::Boolean(lhs), &Value::Boolean(rhs)) => lhs.cmp(&rhs),
            (&Value::Number(lhs), &Value::Number(rhs)) => lhs.cmp(&rhs),
            (&Value::String(ref lhs), &Value::String(ref rhs)) => lhs.cmp(rhs),
            (&Value::Set(ref lhs), &Value::Set(ref rhs)) => lhs.cmp(rhs),
            _ => panic!("can't compare values {:?} and {:?}", self, other),
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        match (self, other) {
            (&Value::Boolean(lhs), &Value::Boolean(rhs)) => lhs == rhs,
            (&Value::Number(lhs), &Value::Number(rhs)) => lhs == rhs,
            (&Value::String(ref lhs), &Value::String(ref rhs)) => lhs == rhs,
            (&Value::Set(ref lhs), &Value::Set(ref rhs)) => lhs == rhs,
            _ => panic!("can't compare values {:?} and {:?}", self, other),
        }
    }
}

impl Eq for Value {}

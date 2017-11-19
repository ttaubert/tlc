// Licensed under MIT. See LICENSE for details.

use std::mem;

use state::State;
use types::Ident;
use value::Value;

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    // A /\ B
    Conjunction(Box<ConjunctionAtom>),
    // A \/ B
    Disjunction(Box<DisjunctionAtom>),
    // a = b
    Equality(Box<EqualityAtom>),
    // Init
    Identifier(Ident),
    // 12345, "abcd", TRUE
    Literal(Value),
    // id \in 0..1
    MemberOf(Box<MemberOfAtom>),
    // [][Next]_vars
    NextStateRelation(Box<Atom>, Vec<Ident>),
    // {0,1,2}
    Set(Box<SetAtom>),
    // State == A /\ (B \/ C)
    StatePredicate(Box<StatePredicateAtom>),
}

impl Atom {
    pub fn is_identifier_without_value(&self, state: &State) -> bool {
        match *self {
            Atom::Identifier(ref id) => !state.has(id),
            _ => false,
        }
    }

    pub fn value(&self, state: &State) -> Value {
        match *self {
            Atom::Identifier(ref id) => state.get(id).expect("identifier must have a value").clone(),
            Atom::Literal(ref val) => val.clone(),
            Atom::Set(ref imp) => imp.value(state),
            _ => panic!("unsupported atom type for value()"),
        }
    }

    pub fn possible_states(&self, state: &State) -> Vec<State> {
        match *self {
            Atom::Conjunction(ref imp) => imp.possible_states(state),
            Atom::Disjunction(ref imp) => imp.possible_states(state),
            Atom::Equality(ref imp) => imp.possible_states(state),
            Atom::MemberOf(ref imp) => imp.possible_states(state),
            Atom::StatePredicate(ref imp) => imp.possible_states(state),
            _ => panic!(""), // TODO Result/Err
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ConjunctionAtom {
    lhs: Box<Atom>,
    rhs: Box<Atom>,
}

impl ConjunctionAtom {
    fn possible_states(&self, state: &State) -> Vec<State> {
        State::merge(
            self.lhs.possible_states(state),
            self.rhs.possible_states(state),
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct DisjunctionAtom {
    lhs: Box<Atom>,
    rhs: Box<Atom>,
}

impl DisjunctionAtom {
    // TODO remove duplicate states
    fn possible_states(&self, state: &State) -> Vec<State> {
        [&self.lhs, &self.rhs]
            .iter()
            .flat_map(|c| c.possible_states(state))
            .collect()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EqualityAtom {
    lhs: Box<Atom>,
    rhs: Box<Atom>,
}

impl EqualityAtom {
    fn possible_states(&self, state: &State) -> Vec<State> {
        let mut lhs = &self.lhs;
        let mut rhs = &self.rhs;

        // Move a value-less identifier to the left.
        if rhs.is_identifier_without_value(state) {
            if lhs.is_identifier_without_value(state) {
                panic!("at least one of two variables must be defined");
            }

            mem::swap(&mut lhs, &mut rhs);
        }

        // If an identifier is on the left, update its state value.
        if let Atom::Identifier(ref id) = **lhs {
            return vec![state.extend(id.clone(), rhs.value(state))];
        }

        // Compare values and return or clear the states.
        if lhs.value(state) == rhs.value(state) {
            vec![state.clone()]
        } else {
            vec![]
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MemberOfAtom {
    lhs: Box<Atom>,
    rhs: Box<Atom>,
}

impl MemberOfAtom {
    fn possible_states(&self, state: &State) -> Vec<State> {
        // If lhs is an identifier it will assume all possible values of rhs.
        if let Atom::Identifier(ref id) = *self.lhs {
            return self.rhs.value(state).values().into_iter().map(|v| {
                state.extend(id.clone(), v.clone())
            }).collect();
        }

        // If lhs is a literal, check if rhs contains it.
        if let Atom::Literal(ref val) = *self.lhs {
            return if self.rhs.value(state).contains(val) {
                vec![state.clone()]
            } else {
                vec![]
            };
        }

        panic!("invalid lhs type");
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetAtom {
    values: Vec<Atom>,
}

impl SetAtom {
    fn value(&self, state: &State) -> Value {
      Value::Set(self.values.iter().map(|v| v.value(state)).collect())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StatePredicateAtom {
    id: Box<Atom>,
    pred: Box<Atom>,
}

impl StatePredicateAtom {
    fn possible_states(&self, state: &State) -> Vec<State> {
        self.pred.possible_states(state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Some test helpers.
    fn _b<T>(a: T) -> Box<T> {
        Box::new(a)
    }
    fn _id(s: &str) -> Atom {
        Atom::Identifier(String::from(s))
    }
    fn _num(v: u64) -> Atom {
        Atom::Literal(Value::Number(v))
    }
    fn _str(v: &str) -> Atom {
        Atom::Literal(Value::String(String::from(v)))
    }
    fn _bool(v: bool) -> Atom {
        Atom::Literal(Value::Boolean(v))
    }
    fn _eq(a: Atom, b: Atom) -> Atom {
        Atom::Equality(_b(EqualityAtom {
            lhs: _b(a),
            rhs: _b(b),
        }))
    }
    fn _mem(a: Atom, b: Atom) -> Atom {
        Atom::MemberOf(_b(MemberOfAtom {
            lhs: _b(a),
            rhs: _b(b),
        }))
    }
    fn _set(vals: Vec<Atom>) -> Atom {
        Atom::Set(_b(SetAtom { values: vals }))
    }

    #[test]
    fn test_possible_states_eq() {
        // Init == a = 0
        let init = _eq(_id("a"), _num(0));
        let expected = State::new().extend("a".to_string(), Value::Number(0));
        assert_eq!(init.possible_states(&State::new()), vec![expected]);

        // Init == 0 = a
        let init = _eq(_num(0), _id("a"));
        let expected = State::new().extend("a".to_string(), Value::Number(0));
        assert_eq!(init.possible_states(&State::new()), vec![expected]);

        // Init == 0 = 0
        let init = _eq(_num(0), _num(0));
        assert_eq!(init.possible_states(&State::new()), vec![State::new()]);

        // Init == 0 = 1
        let init = _eq(_num(0), _num(1));
        assert_eq!(init.possible_states(&State::new()), vec![]);

        // Init == a = b  [with a=2]
        let init = _eq(_id("a"), _id("b"));
        let state = State::new().extend("a".to_string(), Value::Number(2));
        let expected = State::new().extend("a".to_string(), Value::Number(2)).extend(
            "b".to_string(),
            Value::Number(2),
        );
        assert_eq!(init.possible_states(&state), vec![expected]);

        // Init == b = a  [with a=2]
        let init = _eq(_id("b"), _id("a"));
        let expected = State::new().extend("a".to_string(), Value::Number(2)).extend(
            "b".to_string(),
            Value::Number(2),
        );
        assert_eq!(init.possible_states(&state), vec![expected]);

        // Init == a = {0, 1}
        let init = _eq(_id("a"), _set(vec![_num(0), _num(1)]));
        let set = [Value::Number(0), Value::Number(1)].iter().cloned().collect();
        let expected = State::new().extend("a".to_string(), Value::Set(set));
        assert_eq!(init.possible_states(&State::new()), vec![expected]);

        // Init == {0, 1} = a
        let init = _eq(_set(vec![_num(0), _num(1)]), _id("a"));
        let set = [Value::Number(0), Value::Number(1)].iter().cloned().collect();
        let expected = State::new().extend("a".to_string(), Value::Set(set));
        assert_eq!(init.possible_states(&State::new()), vec![expected]);

        // Init == TRUE = {1, 0}
        //let init = _eq(_bool(true), _set(vec![_num(1), _num(0)]));
        //assert_eq!(init.possible_states(&State::new()), vec![]);

        // Init == {0, 1} = {0, 1}
        let init = _eq(_set(vec![_num(0), _num(1)]), _set(vec![_num(0), _num(1)]));
        assert_eq!(init.possible_states(&State::new()), vec![State::new()]);

        // Init == {0, 1} = {1, 0}
        let init = _eq(_set(vec![_num(0), _num(1)]), _set(vec![_num(1), _num(0)]));
        assert_eq!(init.possible_states(&State::new()), vec![State::new()]);

        // Init == {0, 1} = {1, 2}
        let init = _eq(_set(vec![_num(0), _num(1)]), _set(vec![_num(1), _num(2)]));
        assert_eq!(init.possible_states(&State::new()), vec![]);

        // Init == {0} = {1, 0}
        let init = _eq(_set(vec![_num(0)]), _set(vec![_num(1), _num(0)]));
        assert_eq!(init.possible_states(&State::new()), vec![]);

        // Init == {"a"} = {1, 0}
        let init = _eq(_set(vec![_str("a")]), _set(vec![_num(1), _num(0)]));
        assert_eq!(init.possible_states(&State::new()), vec![]);

        // Init == {"a", "b"} = {1, 0}
        //let init = _eq(_set(vec![_str("a"), _str("b")]), _set(vec![_num(1), _num(0)]));
        //assert_eq!(init.possible_states(&State::new()), vec![]);
    }

    #[test]
    fn test_possible_states_mem() {
        // Init == 0 \in {0,1}
        let init = _mem(_num(0), _set(vec![_num(0), _num(1)]));
        assert_eq!(init.possible_states(&State::new()), vec![State::new()]);

        // Init == 2 \in {0,1}
        let init = _mem(_num(2), _set(vec![_num(0), _num(1)]));
        assert_eq!(init.possible_states(&State::new()), vec![]);

        // Init == a \in {0,1}
        let init = _mem(_id("a"), _set(vec![_num(0), _num(1)]));
        let exp1 = State::new().extend("a".to_string(), Value::Number(0));
        let exp2 = State::new().extend("a".to_string(), Value::Number(1));
        assert_eq!(init.possible_states(&State::new()), vec![exp1, exp2]);

        // Init == "a" \in {0,1}
        //let init = _mem(_str("a"), _set(vec![_num(0), _num(1)]));
        //assert_eq!(init.possible_states(&State::new()), vec![]);
    }
}

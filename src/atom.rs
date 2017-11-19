// Licensed under MIT. See LICENSE for details.

use std::mem;

use state::State;
use types::*;

// TODO
pub enum PossibleStatesError {
}

pub enum Value2 {
    Number(i64),
    Set(Vec<Value2>),
    String(String),
}

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
    // id \in 0..1
    MemberOf(Box<MemberOfAtom>),
    // 12345
    Number(Value),
    // [][Next]_vars
    NextStateRelation(Box<Atom>, Vec<Ident>),
    // {0,1,2}
    Set(Vec<Atom>),
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
            Atom::Identifier(ref id) => *state.get(id).expect("identifier must have a value"),
            Atom::Number(ref val) => *val,
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
        // TODO if id, get value of lhs, must exist
        // TODO if num, that's the value
        // TODO get rhs seequence values
        // TODO generate new state, if any

        if let Atom::Identifier(ref id) = *self.lhs {
            if let Atom::Set(ref vals) = *self.rhs {
                return vals.iter()
                    .map(|v| {
                        if let Atom::Number(ref n) = *v {
                            return state.extend(id.clone(), *n);
                        }

                        panic!("sets support only numbers for now");
                    })
                    .collect();
            }

            panic!("invalid rhs type");
        }

        if let Atom::Number(ref val) = *self.lhs {
            if let Atom::Set(ref vals) = *self.rhs {
                let contains = vals.iter().any(|v| {
                    if let Atom::Number(ref n) = *v {
                        return n == val;
                    }

                    panic!("sets support only numbers for now");
                });

                return if contains {
                    vec![state.clone()]
                } else {
                    vec![]
                };
            }

            panic!("invalid rhs type");
        }

        panic!("invalid lhs type");
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
    fn _num(n: u64) -> Atom {
        Atom::Number(n)
    }
    /*fn _vars(ids: Vec<AST>) -> AST {
        AST::Variables(ids)
    }
    fn _consts(ids: Vec<AST>) -> AST {
        AST::Constants(ids)
    }
    fn _pred(id: AST, expr: AST) -> AST {
        AST::Predicate(_b(id), _b(expr))
    }*/
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
    /*fn _conj(a: AST, b: AST) -> AST {
        AST::Conjunction(_b(a), _b(b))
    }*/
    fn _set(vals: Vec<Atom>) -> Atom {
        Atom::Set(vals)
    }
    /*fn _tup(vals: Vec<AST>) -> AST {
        AST::Tuple(vals)
    }
    fn _bool(val: bool) -> AST {
        AST::Boolean(val)
    }
    fn _nsr(next: AST, vars: AST) -> AST {
        AST::NextStateRelation(_b(next), _b(vars))
    }
    fn _mod(id: AST, stmts: Vec<AST>) -> AST {
        AST::Module(_b(id), stmts)
    }*/

    #[test]
    fn test_possible_states_eq() {
        // Init == a = 0
        let init = _eq(_id("a"), _num(0));
        let expected = State::new().extend("a".to_string(), 0);
        assert_eq!(init.possible_states(&State::new()), vec![expected]);

        // Init == 0 = a
        let init = _eq(_num(0), _id("a"));
        let expected = State::new().extend("a".to_string(), 0);
        assert_eq!(init.possible_states(&State::new()), vec![expected]);

        // Init == 0 = 0
        let init = _eq(_num(0), _num(0));
        assert_eq!(init.possible_states(&State::new()), vec![State::new()]);

        // Init == 0 = 1
        let init = _eq(_num(0), _num(1));
        assert_eq!(init.possible_states(&State::new()), vec![]);

        // Init == a = b  [with a=2]
        let init = _eq(_id("a"), _id("b"));
        let state = State::new().extend("a".to_string(), 2);
        let expected = State::new().extend("a".to_string(), 2).extend(
            "b".to_string(),
            2,
        );
        assert_eq!(init.possible_states(&state), vec![expected]);

        // Init == b = a  [with a=2]
        let init = _eq(_id("b"), _id("a"));
        let expected = State::new().extend("a".to_string(), 2).extend(
            "b".to_string(),
            2,
        );
        assert_eq!(init.possible_states(&state), vec![expected]);
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
        let exp1 = State::new().extend("a".to_string(), 0);
        let exp2 = State::new().extend("a".to_string(), 1);
        assert_eq!(init.possible_states(&State::new()), vec![exp1, exp2]);
    }
}

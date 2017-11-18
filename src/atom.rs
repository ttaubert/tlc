// Licensed under MIT. See LICENSE for details.

use state::{State, merge};
use types::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Atom {
    // A /\ B
    // [two children]
    Conjunction(Box<Atom>, Box<Atom>),
    // A \/ B
    // [two children]
    Disjunction(Box<Atom>, Box<Atom>),
    // a = b
    // [two children]
    Equality(Box<Atom>, Box<Atom>),
    // [no children]
    Identifier(Ident),
    // id \in 0..1
    // [two children]
    MemberOf(Box<Atom>, Box<Atom>),
    // [no children]
    Number(Value),
    // [][Next]_vars
    // [one child]
    NextStateRelation(Box<Atom>, Vec<Ident>),
    // {0,1,2}
    Set(Vec<Atom>),
    // State == A /\ (B \/ C)
    // [one child]
    StatePredicate(Ident, Box<Atom>),
}

impl Atom {
    pub fn initial_states(&self, state: &State) -> Vec<State> {
        match *self {
            Atom::Conjunction(ref lhs, ref rhs) => {
                merge(lhs.initial_states(state), rhs.initial_states(state))
            }
            Atom::Disjunction(ref lhs, ref rhs) => {
                [lhs, rhs]
                    .iter()
                    // TODO remove duplicate states
                    .flat_map(|ref c| c.initial_states(state))
                    .collect()
            }
            Atom::Equality(ref lhs, ref rhs) => {
                if let Atom::Identifier(ref id) = **lhs {
                    let val = match **rhs {
                        Atom::Number(ref val) => *val,
                        Atom::Identifier(ref rid) => {
                            match state.get(rid) {
                                Some(val) => *val,
                                None => {
                                    match state.get(id) {
                                        Some(val) => return vec![state.extend(rid.clone(), *val)],
                                        None => panic!("variable {} and {} not defined", id, rid),
                                    }
                                }
                            }
                        }
                        _ => panic!("invalid rhs type"),
                    };

                    return vec![state.extend(id.clone(), val)];
                }

                if let Atom::Number(ref val) = **lhs {
                    return match **rhs {
                        Atom::Number(ref rval) => {
                            if val == rval {
                                vec![state.clone()]
                            } else {
                                vec![]
                            }
                        }
                        Atom::Identifier(ref id) => vec![state.extend(id.clone(), *val)],
                        _ => panic!("invalid rhs type"),
                    };
                }

                panic!("invalid lhs type");
            }
            Atom::MemberOf(ref lhs, ref rhs) => {
                if let Atom::Identifier(ref id) = **lhs {
                    if let Atom::Set(ref vals) = **rhs {
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

                if let Atom::Number(ref val) = **lhs {
                    if let Atom::Set(ref vals) = **rhs {
                        let contains = vals.iter().any(|v| {
                            if let Atom::Number(ref n) = *v {
                                return n == val;
                            }

                            panic!("sets support only numbers for now");
                        });

                        return if contains { vec![state.clone()] } else { vec![] };
                    }

                    panic!("invalid rhs type");
                }

                panic!("invalid lhs type");
            }
            Atom::StatePredicate(ref _id, ref rhs) => rhs.initial_states(state),
            _ => panic!(""), // TODO remove this
        }
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
        Atom::Equality(_b(a), _b(b))
    }
    fn _mem(a: Atom, b: Atom) -> Atom {
        Atom::MemberOf(_b(a), _b(b))
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
    fn test_initial_states_eq() {
        // Init == a = 0
        let init = _eq(_id("a"), _num(0));
        let expected = State::new().extend("a".to_string(), 0);
        assert_eq!(init.initial_states(&State::new()), vec![expected]);

        // Init == 0 = a
        let init = _eq(_num(0), _id("a"));
        let expected = State::new().extend("a".to_string(), 0);
        assert_eq!(init.initial_states(&State::new()), vec![expected]);

        // Init == 0 = 0
        let init = _eq(_num(0), _num(0));
        assert_eq!(init.initial_states(&State::new()), vec![State::new()]);

        // Init == 0 = 1
        let init = _eq(_num(0), _num(1));
        assert_eq!(init.initial_states(&State::new()), vec![]);

        // Init == a = b  [with a=2]
        let init = _eq(_id("a"), _id("b"));
        let state = State::new().extend("a".to_string(), 2);
        let expected = State::new().extend("a".to_string(), 2).extend(
            "b".to_string(),
            2,
        );
        assert_eq!(init.initial_states(&state), vec![expected]);

        // Init == b = a  [with a=2]
        let init = _eq(_id("b"), _id("a"));
        let expected = State::new().extend("a".to_string(), 2).extend(
            "b".to_string(),
            2,
        );
        assert_eq!(init.initial_states(&state), vec![expected]);
    }

    #[test]
    fn test_initial_states_mem() {
        // Init == 0 \in {0,1}
        let init = _mem(_num(0), _set(vec![_num(0), _num(1)]));
        assert_eq!(init.initial_states(&State::new()), vec![State::new()]);

        // Init == 2 \in {0,1}
        let init = _mem(_num(2), _set(vec![_num(0), _num(1)]));
        assert_eq!(init.initial_states(&State::new()), vec![]);

        // Init == a \in {0,1}
        let init = _mem(_id("a"), _set(vec![_num(0), _num(1)]));
        let exp1 = State::new().extend("a".to_string(), 0);
        let exp2 = State::new().extend("a".to_string(), 1);
        assert_eq!(init.initial_states(&State::new()), vec![exp1, exp2]);
    }
}

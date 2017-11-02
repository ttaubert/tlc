// https://github.com/ttaubert/tlc
// (c) 2017 Tim Taubert <tim@timtaubert.de>
// TLC may be freely distributed under the MIT license.

pub type Value = u64;
pub type Ident = String;

// AST node types.
#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Boolean(bool),
    Conjunction(Box<AST>, Box<AST>),
    Constants(Vec<AST>),
    Disjunction(Box<AST>, Box<AST>),
    Equals(Box<AST>, Box<AST>),
    Identifier(String),
    MemberOf(Box<AST>, Box<AST>),
    Module(Box<AST>, Vec<AST>),
    NextStateRelation(Box<AST>, Box<AST>),
    Number(u64),
    Predicate(Box<AST>, Box<AST>),
    Set(Vec<AST>),
    Tuple(Vec<AST>),
    Variables(Vec<AST>),
}

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

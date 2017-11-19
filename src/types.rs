// Licensed under MIT. See LICENSE for details.

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

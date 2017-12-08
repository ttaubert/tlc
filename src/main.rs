// Licensed under MIT. See LICENSE for details.

extern crate rtlc;

use std::collections::HashMap;
use std::process;

use rtlc::parser::parse;
use rtlc::types::AST;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn main() {
    println!("         ______ __    ______  ");
    println!("   _____/_  __// /   / ____/  ");
    println!("  / ___/ / /  / /   / /       ");
    println!(" / /    / /  / /___/ /___     ");
    println!("/_/    /_/  /_____/\\____/ v{}", VERSION);
    println!();

    println!("Parsing SimpleClock.tla");
    let bytes = parse(include_bytes!("../assets/SimpleClock.tla"));

    let parsed = match bytes {
        Ok(parsed) => parsed,
        _ => {
            println!("Failed to parse SimpleClock.tla!");
            process::exit(1);
        }
    };

    let stmts = match parsed {
        AST::Module(_, stmts) => stmts,
        _ => unreachable!()
    };

    let mut variables = None;
    let mut predicates = HashMap::new();

    for stmt in stmts {
        match stmt {
            AST::Variables(vars) => {
                if variables.is_none() {
                    variables = Some(vars.clone());
                } else {
                    panic!("duplicate variables statement");
                }
            }
            AST::Predicate(id, pred) => {
                if let AST::Identifier(id) = *id {
                    if predicates.insert(id.clone(), pred).is_some() {
                        panic!("duplicate predicate {}", id);
                    }
                } else {
                    unreachable!();
                }
            }
            _ => { /* continue */ }
        }
    }

    let spec_id = String::from("Spec");
    let spec = predicates.get(&spec_id).expect("Couldn't find predicate 'Spec'.");

    // TODO check that we have _one_ NSR
    // TODO get initial states
}

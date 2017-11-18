// Licensed under MIT. See LICENSE for details.

extern crate nom;
extern crate rtlc;

use rtlc::parser::parse;
use rtlc::types::AST;

#[test]
fn test_parse_simple_clock() {
    match parse(include_bytes!("../assets/SimpleClock.tla")) {
        Ok(AST::Module(id, _)) => {
            match *id {
                AST::Identifier(id) => assert_eq!(id, "SimpleClock"),
                _ => panic!("parsing module failed"),
            }
        }
        _ => panic!("parsing file failed"),
    }
}

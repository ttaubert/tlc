// Licensed under MIT. See LICENSE for details.

extern crate nom;
extern crate rtlc;

use rtlc::parser::parse;
use rtlc::types::AST;

#[test]
fn test_parse_simple_clock() {
    match parse(include_bytes!("../assets/SimpleClock.tla")) {
        nom::IResult::Done(b"", AST::Module(id, _)) => {
            match *id {
                AST::Identifier(id) => assert_eq!(id, "SimpleClock"),
                _ => assert!(false, "parsing module failed"),
            }
        }
        _ => assert!(false, "parsing file failed"),
    }
}

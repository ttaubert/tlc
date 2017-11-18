// Licensed under MIT. See LICENSE for details.

use std::str::from_utf8;

use nom::{IResult, digit, eol, multispace, non_empty, not_line_ending, space};

use types::AST;

// Connectives.
pub enum Oper {
    And,
    Or,
}

// "\*" to comment out the rest of the line.
named!(
    comment_one_line,
    complete!(do_parse!(
        tag!("\\*") >> not_line_ending >> alt!(eof!() | eol) >>
            (b"")
    ))
);

// Block comments wrapped in (* ... *).
named!(
    comment_block,
    complete!(do_parse!(
        tag!("(*") >> take_until_and_consume!("*)") >> (b"")
    ))
);

// Spaces, newlines, and comments.
named!(
    blanks,
    complete!(do_parse!(
        many1!(
            alt!(multispace | comment_one_line | comment_block)
        ) >> (b"")
    ))
);

named!(
    number<AST>,
    complete!(map!(
        map_res!(map_res!(digit, from_utf8), |x: &str| {
            u64::from_str_radix(x, 10)
        }),
        |n| AST::Number(n)
    ))
);

named!(
    identifier<AST>,
    map!(
        map_res!(
            recognize!(terminated!(
                many1!(one_of!(
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890_"
                )),
                opt!(complete!(tag!("'")))
            )),
            |id: &[u8]| { String::from_utf8(id.to_vec()) }
        ),
        |id: String| { AST::Identifier(id) }
    )
);

named!(
    identifier_list<Vec<AST>>,
    separated_nonempty_list!(
        complete!(delimited!(opt!(blanks), tag!(","), opt!(blanks))),
        identifier
    )
);

named!(
    set<AST>,
    complete!(do_parse!(
    tag!("{") >>
    opt!(blanks) >>
    list: separated_list!(
      delimited!(opt!(blanks), tag!(","), opt!(blanks)),
      proposition
    ) >>
    opt!(blanks) >>
    tag!("}") >>
    (AST::Set(list))
  ))
);

named!(
    tuple<AST>,
    complete!(do_parse!(
    tag!("<<") >>
    opt!(blanks) >>
    list: separated_list!(
      delimited!(opt!(blanks), tag!(","), opt!(blanks)),
      proposition
    ) >>
    opt!(blanks) >>
    tag!(">>") >>
    (AST::Tuple(list))
  ))
);

named!(
    variables<AST>,
    complete!(do_parse!(
        tag!("VARIABLE") >> opt!(tag!("S")) >> blanks >> ids: identifier_list
            >> (AST::Variables(ids))
    ))
);

named!(
    constants<AST>,
    complete!(do_parse!(
        tag!("CONSTANT") >> opt!(tag!("S")) >> blanks >> ids: identifier_list
            >> (AST::Constants(ids))
    ))
);

named!(
    nextsr<AST>,
    complete!(do_parse!(
    tag!("[][") >>
    opt!(blanks) >>
    id: identifier >>
    opt!(blanks) >>
    tag!("]_") >>
    opt!(blanks) >>
    vars: alt!(identifier | tuple) >>
    (AST::NextStateRelation(Box::new(id), Box::new(vars)))
  ))
);

named!(
    boolean<AST>,
    complete!(map!(alt!(tag!("TRUE") | tag!("FALSE")), |b| {
        AST::Boolean(b == b"TRUE")
    }))
);

named!(
    atomic<AST>,
    alt!(boolean | number | identifier | set | tuple | nextsr)
);

named!(binary_sub<AST>, alt!(atomic | parens));

named!(
    binary<AST>,
    complete!(do_parse!(
    lhs: binary_sub >>
    opt!(blanks) >>
    res: alt!(
      do_parse!(
        tag!("=") >>
        opt!(blanks) >>
        rhs: binary_sub >>
        (AST::Equals(Box::new(lhs.clone()), Box::new(rhs)))
      ) | do_parse!(
        tag!("\\in") >>
        opt!(blanks) >>
        rhs: binary_sub >>
        (AST::MemberOf(Box::new(lhs), Box::new(rhs)))
      )
    ) >>
    (res)
  ))
);

named!(proposition_sub<AST>, alt!(binary | atomic | parens));

named!(
    proposition<AST>,
    complete!(map!(
        do_parse!(
    init: proposition_sub >>
    rest: many0!(
      complete!(alt!(
        do_parse!(
          opt!(blanks) >>
          tag!("/\\") >>
          opt!(blanks) >>
          p: proposition_sub >>
          (Oper::And, p)
        ) | do_parse!(
          opt!(blanks) >>
          tag!("\\/") >>
          opt!(blanks) >>
          p: proposition_sub >>
          (Oper::Or, p)
        )
      ))
    ) >>
    (init, rest)
  ),
        |(init, rest)| {
            rest.into_iter().fold(init, |acc, (op, node)| match op {
                Oper::And => AST::Conjunction(Box::new(acc), Box::new(node)),
                Oper::Or => AST::Disjunction(Box::new(acc), Box::new(node)),
            })
        }
    ))
);

named!(
    parens<AST>,
    complete!(do_parse!(
        tag!("(") >> opt!(blanks) >> p: proposition >> opt!(blanks) >> tag!(")") >> (p)
    ))
);

named!(
    predicate<AST>,
    complete!(do_parse!(
        id: identifier >> opt!(blanks) >> tag!("==") >> opt!(blanks) >> expr: proposition
            >> (AST::Predicate(Box::new(id), Box::new(expr)))
    ))
);

named!(statement<AST>, alt!(variables | constants | predicate));

named!(
    statements<Vec<AST>>,
    delimited!(
        opt!(blanks),
        separated_list!(blanks, statement),
        opt!(blanks)
    )
);

named!(module<AST>,
complete!(
    do_parse!(
        take_until_and_consume!("----") >>
        many0!(tag!("-")) >>
        opt!(space) >>
        tag!("MODULE") >>
        blanks >>
        id: identifier >>
        opt!(blanks) >>
        tag!("----") >>
        many0!(tag!("-")) >>
        stmts: statements >>
        tag!("====") >>
        many0!(tag!("=")) >>
        many0!(non_empty) >>
        (AST::Module(Box::new(id), stmts))
    )
    )
);

pub fn parse(bytes: &[u8]) -> Result<AST, ()> {
    match module(bytes) {
        IResult::Done(b"", node) => Ok(node),
        _ => Err(()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom;

    // Some test helpers.
    fn _b<T>(a: T) -> Box<T> {
        Box::new(a)
    }
    fn _id(s: &str) -> AST {
        AST::Identifier(String::from(s))
    }
    fn _num(n: u64) -> AST {
        AST::Number(n)
    }
    fn _vars(ids: Vec<AST>) -> AST {
        AST::Variables(ids)
    }
    fn _consts(ids: Vec<AST>) -> AST {
        AST::Constants(ids)
    }
    fn _pred(id: AST, expr: AST) -> AST {
        AST::Predicate(_b(id), _b(expr))
    }
    fn _eq(a: AST, b: AST) -> AST {
        AST::Equals(_b(a), _b(b))
    }
    fn _mem(a: AST, b: AST) -> AST {
        AST::MemberOf(_b(a), _b(b))
    }
    fn _conj(a: AST, b: AST) -> AST {
        AST::Conjunction(_b(a), _b(b))
    }
    fn _set(vals: Vec<AST>) -> AST {
        AST::Set(vals)
    }
    fn _tup(vals: Vec<AST>) -> AST {
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
    }

    fn parse_ok<F>(f: F, input: &[u8], res: AST)
    where
        F: Fn(&[u8]) -> nom::IResult<&[u8], AST>,
    {
        assert_eq!(f(input), nom::IResult::Done(&b""[..], res));
    }

    #[test]
    fn test_blanks() {
        let parse_ok = |i| {
            assert_eq!(blanks(i), nom::IResult::Done(&b""[..], &b""[..]));
        };

        parse_ok(b" ");
        parse_ok(b"   ");
        parse_ok(b" \t\r\n ");

        assert_eq!(blanks(b""), nom::IResult::Error(nom::ErrorKind::Complete));
    }

    #[test]
    fn test_number() {
        let parse_ok = |i, r| parse_ok(number, i, AST::Number(r));
        parse_ok(b"00000", 0);
        parse_ok(b"01234", 1234);
        parse_ok(b"12345", 12345);

        assert!(number(b"a12345").is_err());
    }

    #[test]
    fn test_set() {
        let parse_ok = |i, r| parse_ok(set, i, r);
        parse_ok(b"{}", _set(vec![]));
        parse_ok(b"{0}", _set(vec![_num(0)]));
        parse_ok(b"{ 0,1 }", _set(vec![_num(0), _num(1)]));
        parse_ok(b"{0, 1, a}", _set(vec![_num(0), _num(1), _id("a")]));
        parse_ok(b"{0, 1, TRUE}", _set(vec![_num(0), _num(1), _bool(true)]));

        let subset = _set(vec![_num(0), _num(1)]);
        parse_ok(b"{0, 1, {0, 1}}", _set(vec![_num(0), _num(1), subset]));

        let eq = _eq(_num(0), _num(1));
        let conj = _conj(_bool(true), _id("A'"));
        parse_ok(b"{0=1, TRUE/\\A'}", _set(vec![eq, conj]));

        let parse_err = |i| assert!(set(i).is_err());
        parse_err(b"{,}");
        parse_err(b"{0,}");
        parse_err(b"{0,,1}");
    }

    #[test]
    fn test_tuple() {
        let parse_ok = |i, r| parse_ok(tuple, i, r);
        parse_ok(b"<<>>", _tup(vec![]));
        parse_ok(b"<<0>>", _tup(vec![_num(0)]));
        parse_ok(b"<< 0,1 >>", _tup(vec![_num(0), _num(1)]));
        parse_ok(b"<<0, 1, a>>", _tup(vec![_num(0), _num(1), _id("a")]));
        parse_ok(b"<<0, 1, TRUE>>", _tup(vec![_num(0), _num(1), _bool(true)]));

        let subset = _tup(vec![_num(0), _num(1)]);
        parse_ok(b"<<0, 1, <<0, 1>>>>", _tup(vec![_num(0), _num(1), subset]));

        let eq = _eq(_num(0), _num(1));
        let conj = _conj(_bool(true), _id("A'"));
        parse_ok(b"<<0=1, TRUE/\\A'>>", _tup(vec![eq, conj]));

        let parse_err = |i| assert!(set(i).is_err());
        parse_err(b"<<,>>");
        parse_err(b"<<0,>>");
        parse_err(b"<<0,,1>>");
    }

    #[test]
    fn test_nextsr() {
        let parse_ok = |i, r| parse_ok(nextsr, i, r);
        parse_ok(b"[][Next]_a", _nsr(_id("Next"), _id("a")));
        parse_ok(b"[][Next]_<<>>", _nsr(_id("Next"), _tup(vec![])));
        parse_ok(b"[][ Next ]_<<>>", _nsr(_id("Next"), _tup(vec![])));

        let tup = _tup(vec![_id("a"), _id("b")]);
        parse_ok(b"[][Next]_ << a, b >>", _nsr(_id("Next"), tup));
    }

    #[test]
    fn test_variables() {
        let parse_ok = |i, r| parse_ok(variables, i, r);
        parse_ok(b"VARIABLE a", _vars(vec![_id("a")]));
        parse_ok(b"VARIABLE a,b", _vars(vec![_id("a"), _id("b")]));
        parse_ok(b"VARIABLES a, b", _vars(vec![_id("a"), _id("b")]));
    }

    #[test]
    fn test_constants() {
        let parse_ok = |i, r| parse_ok(constants, i, r);
        parse_ok(b"CONSTANT a", _consts(vec![_id("a")]));
        parse_ok(b"CONSTANT a,b", _consts(vec![_id("a"), _id("b")]));
        parse_ok(b"CONSTANTS a, b", _consts(vec![_id("a"), _id("b")]));
    }

    #[test]
    fn test_proposition() {
        let parse_ok = |i, r| parse_ok(proposition, i, r);
        parse_ok(b"A", _id("A"));
        parse_ok(b"(A)", _id("A"));
        parse_ok(b"A /\\ B", _conj(_id("A"), _id("B")));
        parse_ok(b"A/\\B", _conj(_id("A"), _id("B")));
        parse_ok(b"(A) /\\ (B)", _conj(_id("A"), _id("B")));
        parse_ok(b"(A /\\ B)", _conj(_id("A"), _id("B")));
        parse_ok(b"(A/\\B)", _conj(_id("A"), _id("B")));
        parse_ok(b"A = B", _eq(_id("A"), _id("B")));
        parse_ok(b"A=B", _eq(_id("A"), _id("B")));
        parse_ok(b"(A = B)", _eq(_id("A"), _id("B")));
        parse_ok(b"(A=B)", _eq(_id("A"), _id("B")));
        parse_ok(b"(A) = (B)", _eq(_id("A"), _id("B")));
        parse_ok(b"( A )=( B )", _eq(_id("A"), _id("B")));

        let eq1 = _eq(_id("A"), _id("B"));
        let eq2 = _eq(_id("B"), _id("A"));
        parse_ok(b"(A = B) = (B = A)", _eq(eq1, eq2));

        let eq1 = _eq(_id("A"), _id("B"));
        let eq2 = _eq(_id("C"), _id("D"));
        let eq3 = _eq(_id("A"), eq1);
        parse_ok(b"(A = (A = B)) = (C = D)", _eq(eq3, eq2));

        let eq = _eq(_id("B"), _id("B"));
        let conj = _conj(_id("A"), eq);
        parse_ok(b"A /\\ B = B /\\ A", _conj(conj, _id("A")));

        let eq = _eq(_id("B"), _id("B"));
        let conj = _conj(_id("A"), eq);
        parse_ok(b"(A /\\ B = B) /\\ A", _conj(conj, _id("A")));

        parse_ok(b"TRUE /\\ FALSE", _conj(_bool(true), _bool(false)));
    }

    #[test]
    fn test_predicate() {
        let parse_ok = |i, r| parse_ok(predicate, i, r);
        parse_ok(b"Init == a=b", _pred(_id("Init"), _eq(_id("a"), _id("b"))));
        parse_ok(b"Init==a=b", _pred(_id("Init"), _eq(_id("a"), _id("b"))));

        let eq1 = _eq(_id("a"), _num(1));
        let eq2 = _eq(_id("b"), _num(0));
        parse_ok(
            b"Init == (a = 1) /\\ b = 0",
            _pred(_id("Init"), _conj(eq1, eq2)),
        );

        let eq1 = _eq(_id("a"), _num(1));
        let eq2 = _eq(_id("b"), _num(0));
        parse_ok(
            b"Init == a = 1 /\\ (b = 0)",
            _pred(_id("Init"), _conj(eq1, eq2)),
        );

        let eq1 = _eq(_id("a"), _num(1));
        let eq2 = _eq(_id("b"), _num(0));
        parse_ok(
            b"Init == (a = 1 /\\ b = 0)",
            _pred(_id("Init"), _conj(eq1, eq2)),
        );

        let eq1 = _eq(_id("a'"), _num(1));
        let eq2 = _eq(_id("b'"), _num(0));
        parse_ok(
            b"Step == (a' = 1) = (b' = 0)",
            _pred(_id("Step"), _eq(eq1, eq2)),
        );
    }

    #[test]
    fn test_statements() {
        let parse_ok = |i, v: &Vec<AST>| assert_eq!(statements(i).unwrap().1, *v);

        let vars = _vars(vec![_id("clock")]);
        let consts = _consts(vec![_id("Data")]);
        let res = vec![vars, consts];
        parse_ok(b"VARIABLES clock CONSTANTS Data", &res);
        parse_ok(b"VARIABLES clock CONSTANTS Data ", &res);
        parse_ok(b" VARIABLES clock CONSTANTS Data", &res);
        parse_ok(b" VARIABLES clock CONSTANTS Data ", &res);
        parse_ok(b" VARIABLES clock(*comment*)CONSTANTS Data ", &res);
        parse_ok(b" VARIABLES clock (* comment *) CONSTANTS Data ", &res);

        let vars = _vars(vec![_id("clock")]);
        let consts = _consts(vec![_id("Data")]);
        let res = vec![consts, vars];
        parse_ok(b" CONSTANTS Data VARIABLES clock ", &res);

        let consts = _consts(vec![_id("Data")]);
        let init = _pred(_id("Init"), _eq(_id("a"), _num(1)));
        let res = vec![consts, init];
        parse_ok(b" CONSTANTS Data Init == a = 1 ", &res);

        let vars = _vars(vec![_id("clock")]);
        let consts = _consts(vec![_id("Data")]);
        let init = _pred(
            _id("Init"),
            _mem(_id("clock"), _set(vec![_num(0), _num(1)])),
        );
        let step = _pred(_id("Step"), _eq(_id("clock'"), _num(1)));
        let nxsr = _nsr(_id("Next"), _id("clock"));
        let spec = _pred(_id("Spec"), _conj(_id("Init"), nxsr));
        let statements = vec![vars, consts, init, step, spec];
        parse_ok(
            b"
VARIABLES clock \\* Single-line comment.
CONSTANTS Data
(* This is a comment block. *)
Init == clock \\in {0,1}  Step == clock' = 1
Spec == Init /\\ [][Next]_clock (*
                                 *)
    ",
            &statements,
        );
    }

    #[test]
    fn test_parse() {
        let parse_ok = |i, r| parse_ok(module, i, r);
        parse_ok(b"----MODULE Test----====", _mod(_id("Test"), vec![]));
        parse_ok(b" ---- MODULE\nTest----\n==== ", _mod(_id("Test"), vec![]));

        let vars = _vars(vec![_id("clock")]);
        let consts = _consts(vec![_id("Data")]);
        parse_ok(
            b"
---- MODULE(* Comment here. *)Test(* And here. *)----
VARIABLES clock
CONSTANTS Data
====
        ",
            _mod(_id("Test"), vec![vars, consts]),
        );

        let vars = _vars(vec![_id("clock")]);
        parse_ok(
            b"
Random stuff before.
---- MODULE Test ----
VARIABLES clock
================================
CONSTANTS Data
Moar random stuff after.
        ",
            _mod(_id("Test"), vec![vars]),
        );
    }
}

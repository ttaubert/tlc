// https://github.com/ttaubert/tlc
// (c) 2017 Tim Taubert <tim@timtaubert.de>
// TLC may be freely distributed under the MIT license.

extern crate rtlc;

// use parser::combination;
use rtlc::state::State;
use rtlc::types::*;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

fn merge_states(a: Vec<State>, b: Vec<State>) -> Vec<State> {
    let mut rv = vec![];
    for sta in a {
        for stb in &b {
            if !sta.contradicts(stb) {
                rv.push(sta.merge(stb))
            }
        }
    }
    rv
}

fn states(atom: &Atom, state: &State) -> Vec<State> {
    match *atom {
        Atom::Conjunction(ref lhs, ref rhs) => merge_states(states(lhs, state), states(rhs, state)),
        Atom::Disjunction(ref lhs, ref rhs) => {
            [lhs, rhs]
                .iter()
                .flat_map(|ref c| states(c, state))
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

            panic!("invalid lhs type");
        }
        Atom::StatePredicate(_, ref rhs) => states(rhs, state),
        _ => panic!("unsupported type in states()"),
    }
}

/*fn next_states(arena: &Arena<Atom>, node: &NodeId, state: &State) -> Vec<State> {
  match arena[*node].data {
    /*Atom::Conjunction => {
      // TODO two children
      let mut children = node.children(arena);
      let sts = next_states(arena, &children.next().unwrap(), state);
      let sts2 = next_states(arena, &children.next().unwrap(), state);
      merge_states(&sts, &sts2)
    }*/
    /*Atom::Disjunction => {
      // TODO two children
      node.children(arena).flat_map(|ref c| next_states(arena, c, state)).collect()
    }*/
    /*Atom::Equality => {
      // TODO two children
      /*if id.ends_with("'") {
        vec!(state.extend(id.clone(), *val))
      } else if state.get(id) == Some(val) {
        vec!(state.clone())
      } else {
        vec!()
      }*/
      vec!()
    }*/
    /*Atom::StatePredicate(_) => {
      // TODO one child
      next_states(arena, &node.children(arena).next().unwrap(), state)
    }*/
    _ => panic!("unsupported type in next_states()")
  }
}*/

fn main() {
    /*let state = State::new();
  let init = Atom::Equality(Box::new(Atom::Identifier(String::from("a"))), Box::new(Atom::Number(0)));
  println!("a=0 --- {:?}", states(&init, &state));

  let init = Atom::Equality(Box::new(Atom::Number(0)), Box::new(Atom::Identifier(String::from("a"))));
  println!("0=a --- {:?}", states(&init, &state));

  let init = Atom::Equality(Box::new(Atom::Number(0)), Box::new(Atom::Number(0)));
  println!("0=0 --- {:?}", states(&init, &state));

  let init = Atom::Equality(Box::new(Atom::Number(0)), Box::new(Atom::Number(1)));
  println!("0=1 --- {:?}", states(&init, &state));

  let state = state.extend(String::from("b"), 2);
  let init = Atom::Equality(Box::new(Atom::Identifier(String::from("a"))), Box::new(Atom::Identifier(String::from("b"))));
  println!("a=b --- {:?}", states(&init, &state));

  let init = Atom::Equality(Box::new(Atom::Identifier(String::from("b"))), Box::new(Atom::Identifier(String::from("a"))));
  println!("b=a --- {:?}", states(&init, &state));

  //let init = Atom::Equality(Box::new(Atom::Identifier(String::from("c"))), Box::new(Atom::Identifier(String::from("d"))));
  //println!("c=d --- {:?}", states(&init, &state));


  // clock \in {0,1}
  let pred = combination(b"clock \\in {0,1}").unwrap().1;
  // Init == clock \in {0,1}
  let init = Atom::StatePredicate(String::from("Init"), Box::new(pred));


  // clock = 0 /\ clock' = 1
  let conj = combination(b"clock = 0 /\\ clock' = 1").unwrap().1;
  // Tick == clock = 0 /\ clock' = 1
  let tick = Atom::StatePredicate(String::from("Tick"), Box::new(conj));


  // clock = 1 /\ clock' = 0
  let conj = combination(b"clock = 1 /\\ clock' = 0").unwrap().1;
  // Tick == clock = 1 /\ clock' = 0
  let tock = Atom::StatePredicate(String::from("Tock"), Box::new(conj));


  // Tick \/ Tock
  let disj = Atom::Disjunction(Box::new(tick), Box::new(tock));
  // Next == Tick \/ Tock
  let next = Atom::StatePredicate(String::from("Next"), Box::new(disj));


  // [][Next]_<<clock>>
  let nextsr = Atom::NextStateRelation(Box::new(next), vec!(String::from("clock")));


  // Next /\ [][Next]_<<clock>>
  let conj = Atom::Conjunction(Box::new(init.clone()), Box::new(nextsr));
  // Spec == Next /\ [][Next]_<<clock>>
  let _spec = Atom::StatePredicate(String::from("Spec"), Box::new(conj));


  // XXX TLC next rewrites the next-state relation as a disjunction of as many simple subactions as possible.
  // XXX Find the list of possible subactions
  // MemberOf ... Disjunction

  println!();

  {
    let init = combination(b"a \\in {0,1}").unwrap().1;
    println!("a \\in [0,1] --- {:?}", states(&init, &State::new()));
  }

  {
    let init = combination(b"a=0 \\/ a=1").unwrap().1;
    println!("a=0 \\/ a=1 --- {:?}", states(&init, &State::new()));
  }

  {
    let init = combination(b"a=0 /\\ a=1").unwrap().1;
    println!("a=0 /\\ a=1 --- {:?}", states(&init, &State::new()));
  }

  {
    let init = combination(b"a=0 /\\ a \\in {0,1}").unwrap().1;
    println!("a=0 /\\ a \\in [0,1] --- {:?}", states(&init, &State::new()));
  }

  {
    let init = combination(b"a=2 /\\ a \\in {0,1}").unwrap().1;
    println!("a=2 /\\ a \\in [0,1] --- {:?}", states(&init, &state));
  }

  {
    let init = combination(b"(a=0 \\/ a=1) /\\ (a=1 \\/ a=2)").unwrap().1;
    println!("(a=0 \\/ a=1) /\\ (a=1 \\/ a=2) --- {:?}", states(&init, &State::new()));
  }

  {
    let init = combination(b"(a=0 \\/ a=1) /\\ (a=1 /\\ a=2)").unwrap().1;
    println!("(a=0 \\/ a=1) /\\ (a=1 /\\ a=2) --- {:?}", states(&init, &State::new()));
  }

  {
    let init = combination(b"(a=0 /\\ b=1) \\/ (a=1 /\\ b=0)").unwrap().1;
    println!("(a=0 /\\ b=1) \\/ (a=1 /\\ b=0) --- {:?}", states(&init, &State::new()));
  }

  let state = State::new();
  let ini_states = states(&init, &state);

  println!("  ________    ___            ________              __               ");
  println!(" /_  __/ /   /   |   __     / ____/ /_  ___  _____/ /_____  _____   ");
  println!("  / / / /   / /| |__/ /_   / /   / __ \\/ _ \\/ ___/ //_/ _ \\/ ___/");
  println!(" / / / /___/ ___ /_  __/  / /___/ / / /  __/ /__/ ,< /  __/ /       ");
  println!("/_/ /_____/_/  |_|/_/     \\____/_/ /_/\\___/\\___/_/|_|\\___/_/ v{}", VERSION);
  println!();
  println!("Found {} distinct initial states.", ini_states.len());
  println!("\nSimpleClock::Init {:?}", ini_states);
  println!();*/



    /*
        ________    ______
   ____/_  __/ /   / ____/
  / ___// / / /   / /     
 / /   / / / /___/ /___   
/_/   /_/ /_____/\____/   
*/

    /*{
    let mut arena = Arena::new();
    let next = arena.new_node(Atom::Equality(String::from("a"), 0));
    println!("a=0 --- {:?}", next_states(&arena, &next, &ini_states[0]));
  }

  {
    let mut arena = Arena::new();
    let next = arena.new_node(Atom::Equality(String::from("clock"), 0));
    println!("clock=0 --- {:?}", next_states(&arena, &next, &ini_states[0]));
    println!("clock=0 --- {:?}", next_states(&arena, &next, &ini_states[1]));
  }

  {
    let mut arena = Arena::new();
    let next = arena.new_node(Atom::Equality(String::from("clock'"), 0));
    println!("clock'=0 --- {:?}", next_states(&arena, &next, &ini_states[0]));
    println!("clock'=0 --- {:?}", next_states(&arena, &next, &ini_states[1]));
  }

  {
    let mut arena = Arena::new();
    let eq1 = arena.new_node(Atom::Equality(String::from("clock"), 0));
    let eq2 = arena.new_node(Atom::Equality(String::from("clock"), 1));
    let next = arena.new_node(Atom::Conjunction);
    next.append(eq1, &mut arena);
    next.append(eq2, &mut arena);
    println!("clock=0 /\\ clock=1 --- {:?}", next_states(&arena, &next, &ini_states[0]));
    println!("clock=0 /\\ clock=1 --- {:?}", next_states(&arena, &next, &ini_states[1]));
  }

  {
    let mut arena = Arena::new();
    let eq1 = arena.new_node(Atom::Equality(String::from("clock'"), 0));
    let eq2 = arena.new_node(Atom::Equality(String::from("clock'"), 1));
    let next = arena.new_node(Atom::Conjunction);
    next.append(eq1, &mut arena);
    next.append(eq2, &mut arena);
    println!("clock'=0 /\\ clock'=1 --- {:?}", next_states(&arena, &next, &ini_states[0]));
    println!("clock'=0 /\\ clock'=1 --- {:?}", next_states(&arena, &next, &ini_states[1]));
  }

  {
    let mut arena = Arena::new();
    let eq1 = arena.new_node(Atom::Equality(String::from("clock"), 0));
    let eq2 = arena.new_node(Atom::Equality(String::from("clock'"), 1));
    let next = arena.new_node(Atom::Conjunction);
    next.append(eq1, &mut arena);
    next.append(eq2, &mut arena);
    println!("clock=0 /\\ clock'=1 --- {:?}", next_states(&arena, &next, &ini_states[0]));
    println!("clock=0 /\\ clock'=1 --- {:?}", next_states(&arena, &next, &ini_states[1]));
  }

  {
    let mut arena = Arena::new();
    let eq1 = arena.new_node(Atom::Equality(String::from("clock"), 1));
    let eq2 = arena.new_node(Atom::Equality(String::from("clock'"), 0));
    let next = arena.new_node(Atom::Conjunction);
    next.append(eq1, &mut arena);
    next.append(eq2, &mut arena);
    println!("clock=1 /\\ clock'=0 --- {:?}", next_states(&arena, &next, &ini_states[0]));
    println!("clock=1 /\\ clock'=0 --- {:?}", next_states(&arena, &next, &ini_states[1]));
  }

  println!();
  println!("Next states:");
  println!("------------");
  println!("(clock=0 /\\ clock'=1) \\/ (clock=1 /\\ clock'=0) --- {:?}", next_states(&arena, &next, &ini_states[0]));
  println!("(clock=0 /\\ clock'=1) \\/ (clock=1 /\\ clock'=0) --- {:?}", next_states(&arena, &next, &ini_states[1]));*/
}

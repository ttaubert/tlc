// Licensed under MIT. See LICENSE for details.

extern crate rtlc;

use std::process;

use rtlc::parser::parse;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

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
    println!("         ______ __    ______  ");
    println!("   _____/_  __// /   / ____/  ");
    println!("  / ___/ / /  / /   / /       ");
    println!(" / /    / /  / /___/ /___     ");
    println!("/_/    /_/  /_____/\\____/ v{}", VERSION);
    println!();

    println!("Parsing SimpleClock.tla");
    let bytes = parse(include_bytes!("../assets/SimpleClock.tla"));

    let _ast = match bytes {
        Ok(module) => module,
        _ => {
            println!("Failed to parse SimpleClock.tla!");
            process::exit(1);
        }
    };

    /*
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

  let state = State::new();
  let ini_states = states(&init, &state);

    {
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

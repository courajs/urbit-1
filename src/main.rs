#![recursion_limit="1024"]
// #![feature(trace_macros)]
// trace_macros!(true);

use std::rc::Rc;

macro_rules! noun {
    ()                           => {};
    ($it:literal)                => { Rc::new(Noun::Atom($it)) };
    ($it:literal $($rest:tt)*)   => { Rc::new(Noun::Cell(noun!($it), noun!($($rest)*))) };
    ($it:ident)                  => { $it.clone() };
    ($it:ident $($rest:tt)*)     => { Rc::new(Noun::Cell($it.clone(), noun!($($rest)*))) };
    (@$it:expr)                  => { $it };
    (@$it:expr, $($rest:tt)*)    => { Rc::new(Noun::Cell($it, noun!($($rest)*))) };
    ([$($sub:tt)*])              => { noun!($($sub)*) };
    ([$($sub:tt)*] $($rest:tt)*) => { Rc::new(Noun::Cell(noun!($($sub)*), noun!($($rest)*))) };
    (($($sub:tt)*) $($rest:tt)*) => { noun!([$($sub)*] $($rest)*) };
    ({$($sub:tt)*} $($rest:tt)*) => { noun!([$($sub)*] $($rest)*) };
}

#[derive(Debug,PartialEq)]
enum Noun {
    Atom(u128),
    Cell(Rc<Noun>,Rc<Noun>),
}
use Noun::*;

impl Noun {
    fn value(&self) -> Result<u128, &'static str> {
        match self {
            Atom(n) => Ok(*n),
            Cell(_,_) => Err("Expecting atom, got cell"),
        }
    }
    fn open(&self) -> Result<(&Rc<Noun>,&Rc<Noun>), &'static str> {
        match self {
            Atom(_) => Err("expecting cell, got atom"),
            Cell(ref l, ref r) => Ok((l, r)),
        }
    }
    fn open_or(&self, s: &'static str) -> Result<(&Rc<Noun>,&Rc<Noun>), String> {
        match self {
            Atom(_) => Err(s.to_owned()),
            Cell(ref l, ref r) => Ok((l, r)),
        }
    }
}

// only operations with sub-evaluations
enum Op {
  Cons { left: Box<Computation>, right: Box<Computation> },
  // Two { subject: Computation, formula: Computation },
  // Three { n: Computation },
  // Four { n: Computation },
  // Five { a: Computation, b: Computation },
  // Six { branch: Computation },
  // Branch { branch_index: Computation },
  // BranchIndex { 
  // Seven,
  // Eight,
  // Nine,
  // Ten,
}

struct Computation {
  op: Op,
  reason: String,
  subject: Rc<Noun>,
  output: Option<Result<Rc<Noun>, String>>,
}

enum Output {
    Continuation(Computation),
    Done(Result<Rc<Noun>, String>),
}
use Output::*;

fn nock(noun: &Rc<Noun>) -> Result<Rc<Noun>, String> {
    let (subject, formula) = noun.open_or("Attempt to evaluate an atom")?;
    match apply(subject, formula) {
        Continuation(_) => Err("no continuations yet!".to_owned()),
        Done(r) => r,
    }
}
fn apply(subject: &Rc<Noun>, formula: &Rc<Noun>) -> Output {
    match **formula {
        Atom(_) => Done(Err("Attempt to apply an atom as a formula".to_owned())),
        Cell(ref head, ref tail) => {
            match **head {
                Cell(_,_) => Done(Err("no autocons yet".to_owned())),
                Atom(instruction) => {
                    match instruction {
                        1 => Done(Ok(tail.clone())),
                        _ => Done(Err("unimplemented instruction".to_owned())),
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_const() {
        assert_eq!(
            nock(&noun![0 1 5]),
               Ok(noun![5])
        );
    }

    #[test]
    fn test_autoconst() {
        assert_eq!(
            nock(&noun![0 [1 4] 1 2]),
               Ok(noun![4 2])
        );
    }
}

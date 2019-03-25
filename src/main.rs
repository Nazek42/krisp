#![allow(dead_code)]

extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate lazy_static;

mod expr;
mod parse;
#[macro_use] mod interpreter;
mod repl;
mod builtins;

use interpreter::Interpreter;
use repl::repl;

fn main() {
    let mut env: Interpreter = Interpreter::init().expect("unable to initialize interpreter");
    repl(&mut env);
}

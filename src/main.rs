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

use bacon_rajan_cc::Cc;

use parse::{parse_source_file, parse_expr};
use expr::*;
use interpreter::{Interpreter};
use repl::repl;

fn main() {
    let mut env: Interpreter = Interpreter::init().expect("unable to initialize interpreter");
    repl(&mut env);
    /*
    for expr in parse_source_file("test.ks") {
        println!("{}", env.eval(Cc::new(expr)));
    }
    */

}

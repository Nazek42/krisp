use bacon_rajan_cc::Cc;

use rustyline::Editor;
use rustyline::error::ReadlineError;

use crate::parse::parse_expr;
use crate::interpreter::Interpreter;

pub fn repl(interpreter: &mut Interpreter) {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("K> ");
        match readline {
            Ok(line) => {
                let result = interpreter.eval(Cc::new(parse_expr(line).unwrap()));
                match result {
                    Ok(value) => {
                        println!("{}", value);
                        interpreter.scope.set("%", value);
                    },
                    Err(error) => println!("error: {}", error)
                };
            }
            _ => panic!()
        }
    }
}

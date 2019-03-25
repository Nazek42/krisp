use bacon_rajan_cc::Cc;

use rustyline::Editor;
use rustyline::error::ReadlineError;
use rustyline::config::Builder;

use crate::parse::parse_expr;
use crate::interpreter::Interpreter;

pub fn repl(interpreter: &mut Interpreter) {
    let mut rl = Editor::<()>::with_config(Builder::new()
        .auto_add_history(true)
        .build());
    if let Some(home) = dirs::home_dir() {
        rl.load_history(&home.join(".krisp_history")).ok();
    }
    loop {
        let readline = rl.readline("K> ");
        match readline {
            Ok(line) => match parse_expr(line) {
                    Ok(code) => {
                        let result = interpreter.eval(Cc::new(code));
                        match result {
                            Ok(value) => {
                                println!("{}", value);
                                interpreter.scope.set("%", value);
                            },
                            Err(error) => println!("{}", error)
                        }
                    },
                    Err(error) => println!("{}", error)
                },
            Err(rlerr) => match rlerr {
                ReadlineError::Eof | ReadlineError::Interrupted => break,
                _ => { println!("I/O error: {}", rlerr); break }
            }
        }
    }
    if let Some(home) = dirs::home_dir() {
        if let Err(err) = rl.save_history(&home.join(".krisp_history")) {
            println!("Could not save history: {}", err);
        }
    }
}

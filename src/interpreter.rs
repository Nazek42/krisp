use std::collections::HashMap;
use bacon_rajan_cc::Cc;

use lazy_static::lazy_static;

use crate::expr::*;
use crate::builtins::BUILTINS;
use crate::parse::parse_source_file;

pub type Namespace = HashMap<String, Cc<SExpr>>;
pub type SpecialFunc = fn(&mut Interpreter, Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String>;

#[macro_export]
macro_rules! wrong_args_check {
    ($name:expr, $args:expr, $nparams:expr) => {
        if $args.len() != $nparams { return Err(format!("wrong number of arguments to {} (expected {}, got {})", $name, $nparams, $args.len())) }
    }
}

lazy_static! {
    static ref SPECIAL: HashMap<&'static str, SpecialFunc> = {
        let mut m: HashMap<&'static str, SpecialFunc> = HashMap::new();
        m.insert("let!", let_macro);
        m.insert("eval", eval_macro);
        m.insert("with", with_macro);
        m.insert("defmacro!", defmacro_macro);
        m.insert("if", if_macro);
        m.insert(r"\", lambda_macro);
        m.insert("while", while_macro);
        m.insert("progn", progn_macro);
        m.insert("'", quote_macro);
        m
    };
}

fn generic_fn(interpreter: &mut Interpreter, metargs: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    let body = Cc::clone(&metargs[1]);
    let mut captures: Namespace = Namespace::new();
    for capture in get_captures(body).iter() {
        if let Some(val) = interpreter.scope.lookup(capture) {
            captures.insert(capture.to_owned(), Cc::clone(&val));
        }
    }
    Ok(Cc::new(SExpr::native_fn(NativeFunction::build(&metargs, captures)?)))
}

fn lambda_macro(interpreter: &mut Interpreter, metargs: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    generic_fn(interpreter, metargs)
}

fn defmacro_macro(interpreter: &mut Interpreter, metargs: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("defmacro!", metargs, 3);
    let name = metargs[0].get_ident().ok_or_else(|| "defmacro! expected an identifier".to_owned())?;
    let params = NativeFunction::try_extract_params(Cc::clone(&metargs[1])).ok_or_else(|| format!("defmacro! expected a list of parameters (got {})", metargs[1]))?;
    let body = Cc::clone(&metargs[2]);
    interpreter.macro_scope.insert(name.to_owned(), Cc::new(SExpr::native_fn(NativeFunction { params, body, captures: Namespace::new() })));
    Ok(Cc::new(SExpr::List(vec![])))
}

fn get_captures(body: Cc<SExpr>) -> Vec<String> {
    match &*body {
        SExpr::Atom(Atom::Native(NativeAtom::Ident(id))) => vec![ id.to_owned() ],
        SExpr::List(list) => {
            let mut captures: Vec<String> = vec![];
            for capture_set in list.iter().map(Cc::clone).map(get_captures) {
                captures.extend(capture_set);
            }
            captures
        }
        _ => vec![]
    }
}

fn let_macro(interpreter: &mut Interpreter, args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("let!", args, 2);
    if let SExpr::Atom(Atom::Native(NativeAtom::Ident(name))) = &*args[0] {
        let value = interpreter.eval(Cc::clone(&args[1]))?;
        interpreter.scope.set(name, Cc::clone(&value));
        Ok(value)
    } else { unimplemented!("multiple let! not yet supported") }
}

fn eval_macro(interpreter: &mut Interpreter, args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("eval", args, 1);
    Ok(interpreter.eval(Cc::clone(&args[0]))?)
}

fn with_macro(interpreter: &mut Interpreter, metargs: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("with", metargs, 3);
    if let (Some(_), Some(_)) = (metargs[0].get_list(), metargs[2].get_list()) {
        let names: Vec<String> = NativeFunction::try_extract_params(Cc::clone(&metargs[0])).ok_or_else(|| format!("with! expects a list of names (got {})", metargs[0]))?;
        let values: Vec<_> = interpreter.eval(Cc::clone(&metargs[1]))?.get_list().ok_or_else(|| format!("with! expects a list of values (got {})", metargs[1]))?.to_vec();
        //let values: Vec<_> = values_raw.iter().map(|expr| interpreter.eval(Cc::clone(expr))).collect();
        Ok(interpreter.eval_with(Cc::clone(&metargs[2]), &names, &values)?)
    } else {
        Err("with expects a list of names, a list of values, and an expression to evaluate in the new context".to_owned())
    }
}

fn if_macro(interpreter: &mut Interpreter, args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("if", args, 3);
    let pred = Cc::clone(&args[0]);
    let iftrue = &args[1];
    let iffalse = &args[2];
    Ok(if interpreter.eval(pred)?.as_bool() {
        interpreter.eval(Cc::clone(iftrue))
    } else {
        interpreter.eval(Cc::clone(iffalse))
    }?)
}

fn while_macro(interpreter: &mut Interpreter, args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("while", args, 2);
    let pred = &args[0];
    let body = &args[1];
    let mut result = Cc::new(SExpr::null());
    while interpreter.eval(Cc::clone(pred))?.as_bool() {
        result = interpreter.eval(Cc::clone(body))?;
    }
    Ok(result)
}

fn progn_macro(interpreter: &mut Interpreter, args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    let mut result = Cc::new(SExpr::null());
    for expr in args.iter().map(Cc::clone) {
        result = interpreter.eval(expr)?;
    }
    Ok(result)
}

fn quote_macro(interpreter: &mut Interpreter, args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("'", args, 1);
    Ok(Cc::clone(&args[0]))
}

pub struct Interpreter {
    pub scope: NestedScope,
    pub macro_scope: Namespace
}

impl Interpreter {
    pub fn init() -> Result<Interpreter, String> {
        let mut env = Interpreter { scope: NestedScope::from(vec![BUILTINS()]), macro_scope: Namespace::new() };
        env.scope.push(Namespace::new());
        env.exec_file("prelude.ks")?;
        env.scope.push(Namespace::new());
        Ok(env)
    }

    pub fn eval(&mut self, expr_raw: Cc<SExpr>) -> Result<Cc<SExpr>, String> {
        let expr = self.expand_macros(expr_raw)?;
        match &*expr {
            SExpr::Atom(atom) => Ok(match atom.clone() {
                Atom::Native(native) => match native {
                    NativeAtom::Ident(id) => {
                        let maybe_value = self.scope.lookup(&id);
                        //let success = maybe_value.is_some();
                        maybe_value.unwrap_or_else(|| Cc::clone(&expr))
                    },
                    _ => Cc::clone(&expr)
                },
                Atom::Extern(_) => expr
            }),
            SExpr::List(list) => {
                if list.is_empty() {
                    return Ok(Cc::new(SExpr::List(vec![])));
                }
                let maybe_func = self.eval(Cc::clone(&list[0]))?;
                let raw_args = list.iter().skip(1);
                if let SExpr::Atom(Atom::Native(NativeAtom::Ident(id))) = &*maybe_func {
                    // special thing detected
                    if let Some(special_func) = SPECIAL.get(id.as_ref() as &str) {
                        special_func(self, raw_args.map(Cc::clone).collect::<Vec<Cc<SExpr>>>())
                    } else {
                        return Err(format!("undefined symbol {}", id))
                    }
                } else if let SExpr::Atom(Atom::Native(NativeAtom::Function(func))) = &*maybe_func {
                    let args: Vec<Cc<SExpr>> = raw_args.map(|sub| self.eval(Cc::clone(sub))).collect::<Result<Vec<_>, String>>()?;
                    match func {
                        Function::Native(native) => {
                            self.scope.push(native.captures.clone());
                            let result = self.eval_with(Cc::clone(&native.body), &native.params, &args);
                            self.scope.pop();
                            Ok(result?)
                        }
                        Function::Extern(ext) => ext.call(args)
                    }
                } else {
                    return Err(format!("expected function, got {}", expr))
                }
            }
        }
    }

    pub fn eval_with(&mut self, expr: Cc<SExpr>, names: &[String], values: &[Cc<SExpr>]) -> Result<Cc<SExpr>, String> {
        let mut new_ns = Namespace::new();
        for (name, value) in names.iter().zip(values.iter()) {
            new_ns.insert(name.to_owned(), Cc::clone(value));
        }
        self.scope.push(new_ns);
        let result = self.eval(expr);
        self.scope.pop();
        result
    }

    pub fn exec_file<S>(&mut self, path: S) -> Result<(), String>
    where S: AsRef<str> {
        let module = parse_source_file(path)?;
        for expr in module.into_iter().map(Cc::new) {
            self.eval(expr)?;
        }
        Ok(())
    }

    fn expand_macros(&mut self, expr: Cc<SExpr>) -> Result<Cc<SExpr>, String> {
        if let Some(list) = expr.get_list() {
            if list.len() == 0 { return Ok(Cc::clone(&expr)); }
            if let Some(id) = list[0].get_ident() {
                if let Some(macro_ref) = self.macro_scope.get(id) {
                    let macro_ = Cc::clone(macro_ref);
                    let macro_ns = macro_.get_native_fn().unwrap();
                    let args: Vec<Cc<SExpr>> = list.iter().skip(1).map(Cc::clone).collect();
                    assert_eq!(macro_ns.params.len(), args.len());
                    let expanded = self.eval_with(Cc::clone(&macro_ns.body), &macro_ns.params, &args)?;
                    let substituted = Interpreter::substitute(expanded, &macro_ns.params, &args);
                    //let expanded = Interpreter::substitute(Cc::clone(&macro_ns.body), &macro_ns.params, &args);
                    //self.expand_macros(expanded)
                    Ok(substituted)
                } else {
                    Ok(Cc::new(SExpr::List(list.iter().map(|sub| self.expand_macros(Cc::clone(sub))).collect::<Result<Vec<_>,_>>()?)))
                }
            } else {
                Ok(Cc::new(SExpr::List(list.iter().map(|sub| self.expand_macros(Cc::clone(sub))).collect::<Result<Vec<_>,_>>()?)))
            }
        } else {
            Ok(Cc::clone(&expr))
        }
    }

    fn substitute(expr: Cc<SExpr>, names: &[String], replace: &[Cc<SExpr>]) -> Cc<SExpr> {
        match &*expr {
            SExpr::Atom(Atom::Native(NativeAtom::Ident(id))) => {
                if let Some(i) = names.iter().position(|name| name == id) {
                    Cc::clone(&replace[i])
                } else {
                    Cc::clone(&expr)
                }
            }
            SExpr::List(list) => Cc::new(SExpr::List(list.iter().map(|sub| Interpreter::substitute(Cc::clone(sub), names, replace)).collect())),
            _ => Cc::clone(&expr)
        }
    }
}

pub struct NestedScope {
    stack: Vec<Namespace>
}

impl From<Vec<Namespace>> for NestedScope {
    fn from(stack: Vec<Namespace>) -> NestedScope {
        NestedScope { stack }
    }
}

impl NestedScope {
    pub fn new() -> NestedScope {
        NestedScope { stack: vec![] }
    }

    pub fn push(&mut self, ns: Namespace) {
        self.stack.push(ns)
    }

    pub fn pop(&mut self) -> Option<Namespace> {
        self.stack.pop()
    }

    pub fn lookup<S: AsRef<str>>(&self, id: S) -> Option<Cc<SExpr>> {
        let found = self.stack.iter()
                              .rev()
                              .find(|ns| ns.contains_key(id.as_ref()));
        if let Some(ns) = found {
            let expr = ns.get(id.as_ref()).unwrap(); // this unwrap is safe because we checked earlier
            Some(Cc::clone(expr))
        } else {
            None
        }
    }

    pub fn set<S: AsRef<str>>(&mut self, id: S, val: Cc<SExpr>) {
        let top_ind = self.stack.len() - 1;
        self.stack[top_ind].insert(id.as_ref().to_owned(), Cc::clone(&val));
    }
}

impl From<Namespace> for NestedScope {
    fn from(ns: Namespace) -> NestedScope {
        NestedScope { stack: vec![ns] }
    }
}

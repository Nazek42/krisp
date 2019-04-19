use std::fmt;
use bacon_rajan_cc::{Cc, Trace, Tracer};
use downcast_rs::Downcast;
use downcast_rs::impl_downcast;

use crate::interpreter::Namespace;

#[derive(Clone, Debug)]
pub enum SExpr {
    Atom(Atom),
    List(Vec<Cc<SExpr>>)
}

impl Trace for SExpr {
    fn trace(&mut self, tracer: &mut Tracer) {
        match self {
            SExpr::Atom(atom) => atom.trace(tracer),
            SExpr::List(list) => {
                for item in list.iter_mut() {
                    tracer(item);
                }
            }
        };
    }
}

#[derive(Clone, Debug)]
pub enum Atom {
    Native(NativeAtom),
    Extern(Box<dyn ExternObj>)
}

impl Trace for Atom {
    fn trace(&mut self, tracer: &mut Tracer) {
        match self {
            Atom::Native(na) => na.trace(tracer),
            Atom::Extern(ea) => ea.trace(tracer)
        };
    }
}

#[derive(Clone, Debug)]
pub enum NativeAtom {
    Int(i64),
    Float(f64),
    Str(String),
    Ident(String),
    Function(Function)
}

impl Trace for NativeAtom {
    fn trace(&mut self, tracer: &mut Tracer) {
        if let NativeAtom::Function(Function::Native(nf)) = self {
            nf.trace(tracer)
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    Native(NativeFunction),
    Extern(ExternFunction),
}

#[derive(Clone, Debug)]
pub struct NativeFunction {
    pub params: Vec<String>,
    pub body: Cc<SExpr>,
    pub captures: Namespace,
}

impl Trace for NativeFunction {
    fn trace(&mut self, tracer: &mut Tracer) {
        tracer(&mut self.body);
        //self.captures.trace(tracer);
        for (_, capture) in &mut self.captures.iter_mut() {
            tracer(capture);
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ExternFunction {
    pub func: fn(Vec<Cc<SExpr>>)->Result<Cc<SExpr>, String>,
}

impl NativeFunction {
    pub fn try_extract_params(params_raw: Cc<SExpr>) -> Option<Vec<String>> {
        if let Some(params_vec) = params_raw.get_list() {
            let mut params: Vec<String> = vec![];
            for param in params_vec.iter() {
                if let Some(id) = param.get_ident() {
                    params.push(id.to_owned());
                } else { return None; }
            }
            Some(params)
        } else {
            None
        }
    }

    pub fn build(metargs: &[Cc<SExpr>], captures: Namespace) -> Result<NativeFunction, String> {
        if let Some(params) = NativeFunction::try_extract_params(Cc::clone(&metargs[0])) {
            let body = Cc::clone(&metargs[1]);
            Ok(NativeFunction { params, body, captures })
        } else { Err("need a list of parameters to construct a function".to_owned()) }
    }
}

impl ExternFunction {
    pub fn call(&self, args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
        (self.func)(args)
    }
}

pub trait ExternObj: std::fmt::Debug + Trace + Downcast {
    fn box_clone(&self) -> Box<ExternObj>;
    fn name(&self) -> String;
    fn as_bool(&self) -> bool;
}
impl_downcast!(ExternObj);

impl Clone for Box<ExternObj> {
    fn clone(&self) -> Box<ExternObj> {
        self.box_clone()
    }
}

macro_rules! getter {
    ($fname:ident => $var:ident : $type:ty, $pattern:pat) => {
        pub fn $fname(&self) -> Option<&$type> {
            if let $pattern = &self { Some($var) }
            else                              { None }
        }
    }
}

macro_rules! constructor {
    ($fname:ident => $var:ident : $type:ty, $cons:expr) => {
        pub fn $fname($var: $type) -> SExpr {
            $cons
        }
    }
}

impl SExpr {
    getter!{get_int => i: i64, SExpr::Atom(Atom::Native(NativeAtom::Int(i)))}
    getter!{get_float => f: f64, SExpr::Atom(Atom::Native(NativeAtom::Float(f)))}
    getter!{get_string => s: String, SExpr::Atom(Atom::Native(NativeAtom::Str(s)))}
    getter!{get_ident => id: String, SExpr::Atom(Atom::Native(NativeAtom::Ident(id)))}
    getter!{get_native_fn => nf: NativeFunction, SExpr::Atom(Atom::Native(NativeAtom::Function(Function::Native(nf))))}
    getter!{get_extern_fn => ef: ExternFunction, SExpr::Atom(Atom::Native(NativeAtom::Function(Function::Extern(ef))))}
    getter!{get_extern_obj => obj: Box<ExternObj>, SExpr::Atom(Atom::Extern(obj))}
    getter!{get_list => list: Vec<Cc<SExpr>>, SExpr::List(list)}

    constructor!{int => i: i64, SExpr::Atom(Atom::Native(NativeAtom::Int(i)))}
    constructor!{float => f: f64, SExpr::Atom(Atom::Native(NativeAtom::Float(f)))}
    constructor!{string => s: String, SExpr::Atom(Atom::Native(NativeAtom::Str(s)))}
    constructor!{ident => id: String, SExpr::Atom(Atom::Native(NativeAtom::Ident(id)))}
    constructor!{native_fn => nf: NativeFunction, SExpr::Atom(Atom::Native(NativeAtom::Function(Function::Native(nf))))}
    constructor!{extern_fn => ef: ExternFunction, SExpr::Atom(Atom::Native(NativeAtom::Function(Function::Extern(ef))))}
    constructor!{extern_obj => obj: Box<ExternObj>, SExpr::Atom(Atom::Extern(obj))}
    constructor!{list => list: Vec<Cc<SExpr>>, SExpr::List(list)}

    pub fn null() -> SExpr {
        SExpr::List(vec![])
    }

    pub fn is_null(&self) -> bool {
        if let SExpr::List(list) = self {
            list.len() == 0
        } else {
            false
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            SExpr::Atom(atom) => match atom {
                Atom::Native(native) => match native {
                    NativeAtom::Int(i) => *i != 0,
                    NativeAtom::Float(f) => *f != 0.0,
                    NativeAtom::Str(s) => !s.is_empty(),
                    _ => true
                },
                Atom::Extern(obj) => obj.as_bool()
            },
            SExpr::List(l) => !l.is_empty()
        }
    }
}

impl fmt::Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SExpr::Atom(atom) => match atom {
                Atom::Native(native) => match native {
                    NativeAtom::Int(i) => write!(f, "{}", i),
                    NativeAtom::Float(fl) => write!(f, "{}", fl),
                    NativeAtom::Str(s) => write!(f, "\"{}\"", s),
                    NativeAtom::Ident(id) => write!(f, "{}", id),
                    NativeAtom::Function(func) => match func {
                        Function::Native(nf) => write!(f, r"(\ ({}) {})", nf.params.join(" "), nf.body),
                        Function::Extern(ef) => write!(f, "<extern function @ {:p}>", &ef.func)
                    }
                },
                Atom::Extern(obj) => write!(f, "<extern object `{:?}`>", obj)
            },
            SExpr::List(list) => {
                let mut iter = list.iter();
                write!(f, "(")?;
                if let Some(item) = iter.next() {
                    write!(f, "{}", item)?;
                    for item in iter {
                        write!(f, " {}", item)?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

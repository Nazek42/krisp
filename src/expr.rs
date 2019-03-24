use std::fmt;
use std::rc::Rc;

use crate::interpreter::Namespace;

#[derive(Clone, Debug)]
pub enum SExpr {
    Atom(Atom),
    List(Vec<Rc<SExpr>>)
}

#[derive(Clone, Debug)]
pub enum Atom {
    Native(NativeAtom),
    Extern(Box<ExternObj>)
}

#[derive(Clone, Debug)]
pub enum NativeAtom {
    Int(i64),
    Float(f64),
    Str(String),
    Ident(String),
    Function(Function)
}

#[derive(Clone, Debug)]
pub enum Function {
    Native(NativeFunction),
    Extern(ExternFunction),
}

#[derive(Clone, Debug)]
pub struct NativeFunction {
    pub params: Vec<String>,
    pub body: Rc<SExpr>,
    pub captures: Namespace,
}

#[derive(Clone, Copy, Debug)]
pub struct ExternFunction {
    pub func: fn(Vec<Rc<SExpr>>)->Result<Rc<SExpr>, String>,
}

impl NativeFunction {
    pub fn try_extract_params(params_raw: Rc<SExpr>) -> Option<Vec<String>> {
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

    pub fn build(metargs: &[Rc<SExpr>], captures: Namespace) -> Result<NativeFunction, String> {
        if let Some(params) = NativeFunction::try_extract_params(Rc::clone(&metargs[0])) {
            let body = Rc::clone(&metargs[1]);
            Ok(NativeFunction { params, body, captures })
        } else { Err("need a list of parameters to construct a function".to_owned()) }
    }
}

impl ExternFunction {
    pub fn call(&self, args: Vec<Rc<SExpr>>) -> Result<Rc<SExpr>, String> {
        (self.func)(args)
    }
}

pub trait ExternObj: std::fmt::Debug + fmt::Display {
    fn box_clone(&self) -> Box<ExternObj>;
    fn name(&self) -> String;
    fn as_bool(&self) -> bool;
}

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
    getter!{get_list => list: Vec<Rc<SExpr>>, SExpr::List(list)}

    constructor!{int => i: i64, SExpr::Atom(Atom::Native(NativeAtom::Int(i)))}
    constructor!{float => f: f64, SExpr::Atom(Atom::Native(NativeAtom::Float(f)))}
    constructor!{string => s: String, SExpr::Atom(Atom::Native(NativeAtom::Str(s)))}
    constructor!{ident => id: String, SExpr::Atom(Atom::Native(NativeAtom::Ident(id)))}
    constructor!{native_fn => nf: NativeFunction, SExpr::Atom(Atom::Native(NativeAtom::Function(Function::Native(nf))))}
    constructor!{extern_fn => ef: ExternFunction, SExpr::Atom(Atom::Native(NativeAtom::Function(Function::Extern(ef))))}
    constructor!{extern_obj => obj: Box<ExternObj>, SExpr::Atom(Atom::Extern(obj))}
    constructor!{list => list: Vec<Rc<SExpr>>, SExpr::List(list)}

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

    pub fn pprint(&self) {
        self.pprint_rec(0);
    }

    fn pprint_rec(&self, indent: usize) {
        if let SExpr::List(list) = self {
            println!("{}(", "  ".repeat(indent));
            for expr in list.iter() {
                expr.pprint_rec(indent + 1);
            }
            println!("{})", "  ".repeat(indent));
        } else if let SExpr::Atom(atom) = self {
            println!("{}{}", "  ".repeat(indent), match atom {
                Atom::Native(native) => match native {
                    NativeAtom::Int(i) => format!("int {}", i),
                    NativeAtom::Float(f) => format!("float {}", f),
                    NativeAtom::Str(s) => format!("string {}", s),
                    NativeAtom::Ident(id) => format!("identifier {}", id),
                    NativeAtom::Function(_) => "function".to_owned(),
                },
                Atom::Extern(obj) => format!("extern atom {}", obj)
            });
        } else { unreachable!() }
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
                Atom::Extern(obj) => write!(f, "<extern object `{}`>", obj)
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

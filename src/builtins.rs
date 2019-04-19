#![allow(non_snake_case)]
use bacon_rajan_cc::Cc;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};
use std::iter::once;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use crate::expr::*;
use crate::interpreter::Namespace;
use crate::parse::parse_source_string;

macro_rules! arithmetic {
    ($($method:ident => $kname:expr),+) => {
        $(
            fn $method(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
                wrong_args_check!($kname, args, 2);
                let a = Cc::clone(&args[0]);
                let b = Cc::clone(&args[1]);
                Ok(Cc::new(match (&*a, &*b) {
                    (SExpr::Atom(Atom::Native(na)), SExpr::Atom(Atom::Native(nb)))
                        => SExpr::Atom(Atom::Native({
                            use NativeAtom::{Int, Float};
                            match (na, nb) {
                                (Int(ia),   Int(ib)  ) => Int(ia.$method(ib)),
                                (Int(ia),   Float(fb)) => Float((*ia as f64).$method(fb)),
                                (Float(fa), Int(ib)  ) => Float(fa.$method(*ib as f64)),
                                (Float(fa), Float(fb)) => Float(fa.$method(fb)),
                                _ => { return Err(format!("{} expects int or float (got {} . {})", $kname, a, b)) }
                            }
                        })),
                    _ => { return Err(format!("{} expects int or float (got {} . {})", $kname, a, b)) }
                }))
            }
        )+
    }
}

macro_rules! namespace {
    ($nsname:ident; $($func:ident => $kname:expr),*) => {
        pub fn $nsname() -> Namespace {
            let mut module = Namespace::new();
            $(module.insert($kname.to_owned(), Cc::new(SExpr::extern_fn(ExternFunction{ func: $func })));)*
            module
        }
    }
}

arithmetic!{ add => "+", sub => "-", mul => "*", div => "/", rem => "mod" }

fn cmp(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("=", args, 2);
    let a = Cc::clone(&args[0]);
    let b = Cc::clone(&args[1]);
    Ok(Cc::new(match (&*a, &*b) {
        (SExpr::Atom(Atom::Native(na)), SExpr::Atom(Atom::Native(nb)))
            => SExpr::Atom(Atom::Native({
                use NativeAtom::{Int, Float, Str, Ident};
                Int(match (na, nb) {
                    (Int(ia),   Int(ib)  ) => if ia < ib { -1 } else if ia > ib { 1 } else { 0 },
                    (Int(ia),   Float(fb)) => if (*ia as f64) < *fb { -1 } else if (*ia as f64) > *fb { 1 } else { 0 },
                    (Float(fa), Int(ib)  ) => if *fa < (*ib as f64) { -1 } else if *fa > (*ib as f64) { 1 } else { 0 },
                    (Float(fa), Float(fb)) => if fa < fb { -1 } else if fa > fb { 1 } else { 0 },
                    (Str(sa),   Str(sb)  ) => if sa < sb { -1 } else if sa > sb { 1 } else { 0 },
                    (Str(sa),   Ident(sb)  ) => if sa < sb { -1 } else if sa > sb { 1 } else { 0 },
                    (Ident(sa),   Str(sb)  ) => if sa < sb { -1 } else if sa > sb { 1 } else { 0 },
                    (Ident(sa),   Ident(sb)  ) => if sa < sb { -1 } else if sa > sb { 1 } else { 0 },
                    _ => { return Err(format!("cmp expects int, float, or string (got {} . {})", a, b)) }
                })
            })),
        _ => { return Err(format!("cmp expects int, float, or string (got {} . {})", a, b)) }
    }))
}

fn head(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("head", args, 1);
    Ok(Cc::clone(&args[0].get_list().ok_or_else(|| format!("head expected list, got {}", args[0]))?[0]))
}

fn tail(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("tail", args, 1);
    let result: Vec<_> = (&args[0].get_list().ok_or_else(|| format!("tail expected list, got {}", args[0]))?[1..]).iter().map(Cc::clone).collect();
    Ok(if result.len() == 1 {
        Cc::clone(&result[0])
    } else {
        Cc::new(SExpr::List(result))
    })
}

fn cons(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("cons", args, 2);
    let first = Cc::clone(&args[0]);
    let rest = Cc::clone(&args[1]);
    if let Some(list) = rest.get_list() {
        let mut new_list: Vec<_> = vec![first];
        new_list.extend(list.iter().map(Cc::clone));
        Ok(Cc::new(SExpr::List(new_list)))
    } else {
        Ok(Cc::new(SExpr::List(vec![first, rest])))
    }
}

fn append(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("append", args, 2);
    let first = Cc::clone(&args[0]);
    if first.get_list().is_none() { return Err("append expects a list as its first argument".to_owned()); }
    let rest = Cc::clone(&args[1]);
    if let Some(list) = rest.get_list() {
        let new_list: Vec<_> = first.get_list().unwrap().iter().chain(list.iter()).map(Cc::clone).collect();
        Ok(Cc::new(SExpr::List(new_list)))
    } else {
        let new_list: Vec<_> = first.get_list().unwrap().iter().chain(once(&rest)).map(Cc::clone).collect();
        Ok(Cc::new(SExpr::List(new_list)))
    }
}

fn len(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("len", args, 1);
    Ok(Cc::new(SExpr::int(args[0].get_list().ok_or_else(|| format!("len expected list, got {}", args[0]))?.len() as i64)))
}

macro_rules! type_check {
    ($($getter:ident => $kname:expr),+) => {
        $(
            fn $getter(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
                wrong_args_check!($kname, args, 1);
                Ok(Cc::new(SExpr::int(args[0].$getter().is_some() as i64)))
            }
        )+
    }
}

type_check!{
    get_int => "int?",
    get_float => "float?",
    get_string => "string?",
    get_ident => "ident?",
    get_native_fn => "native-fn?",
    get_extern_fn => "extern-fn?",
    get_extern_obj => "extern-obj?",
    get_list => "list?"
}

fn make_list(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    Ok(Cc::new(SExpr::list(args)))
}

fn println_fn(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    for expr in args.iter() {
        if let Some(s) = expr.get_string() {
            println!("{}", s);
        } else {
            println!("{}", expr);
        }
    }
    Ok(Cc::new(SExpr::null()))
}

fn lookup(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("get", args, 2);
    if let Some(i) = args[0].get_int() {
        if let Some(list) = args[1].get_list() {
            Ok(Cc::clone(&list[*i as usize]))
        } else {
            Err(format!("get expected a list (got {})", args[1]))
        }
    } else {
        Err(format!("get expected an int (got {})", args[0]))
    }
}

fn parse_fn(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("parse", args, 1);
    if let Some(s) = args[0].get_string() {
        let parsed = parse_source_string(s)?;
        Ok(if parsed.is_empty() {
            Cc::new(SExpr::null())
        } else if parsed.len() == 1 {
            Cc::new(parsed[0].clone())
        } else {
            Cc::new(SExpr::List(parsed.iter().map(|e| Cc::new(e.clone())).collect()))
        })
    } else {
        Err("parse expects a string".to_owned())
    }
}

type KrispHashMap = HashMap<u64, Cc<SExpr>>;

impl ExternObj for KrispHashMap {
    fn box_clone(&self) -> Box<ExternObj> {
        Box::new(self.clone())
    }
    fn name(&self) -> String {
        "hashmap".to_owned()
    }
    fn as_bool(&self) -> bool {
        self.is_empty()
    }
}

trait TryHash {
    fn try_hash(&self) -> Option<u64>;
}

impl TryHash for SExpr {
    fn try_hash(&self) -> Option<u64> {
        match self {
            SExpr::Atom(atom) => if let Atom::Native(native) = atom {
                use NativeAtom::*;
                match native {
                    Int(x) => {
                        let mut h = DefaultHasher::new();
                        x.hash(&mut h);
                        Some(h.finish())
                    },
                    Str(x) | Ident(x) => {
                        let mut h = DefaultHasher::new();
                        x.hash(&mut h);
                        Some(h.finish())
                    },
                    _ => None
                }
            } else { None },
            SExpr::List(list) => {
                let hash_attempt: Option<Vec<u64>> = list.iter().map(|expr| expr.try_hash()).collect();
                hash_attempt.map(|hashes| {
                    let mut h = DefaultHasher::new();
                    for &hash in hashes.iter() {
                        h.write_u64(hash);
                    }
                    h.finish()
                })
            }
        }
    }
}

fn hash_constructor(args_outer: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("#new", args_outer, 1);
    if let Some(args) = args_outer[0].get_list() {
        let pairs = args.iter().enumerate().map(|(i, arg)| {
            if let Some(list) = arg.get_list() {
                if list.len() == 2 {
                    Ok((Cc::clone(&list[0]), Cc::clone(&list[1])))
                } else {
                    Err(format!("#new expected a list of pairs (length of index {} was {})", i, list.len()))
                }
            } else {
                Err(format!("#new expected a list of pairs (index {} was {})", i, arg))
            }
        }).collect::<Result<Vec<(Cc<SExpr>, Cc<SExpr>)>, String>>()?;
        let hash_pairs = pairs.iter().map(|(k, v)| {
            if let Some(hash) = k.try_hash() {
                Ok((hash, Cc::clone(v)))
            } else {
                Err(format!("keys must be hashable ({} is not hashable)", k))
            }
        }).collect::<Result<Vec<(u64, Cc<SExpr>)>, String>>()?;
        Ok(Cc::new(SExpr::extern_obj({
            let mut hm = KrispHashMap::new();
            for (hash, v) in hash_pairs {
                hm.insert(hash, Cc::clone(&v));
            }
            Box::new(hm)
        })))
    } else {
        Err(format!("#new expected a list of pairs (got {})", args_outer[0]))
    }
}

fn hash_lookup(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("#get", args, 2);
    if let Some(eo) = args[0].get_extern_obj() {
        if let Some(hm) = eo.downcast_ref::<KrispHashMap>() {
            if let Some(key) = args[1].try_hash() {
                if let Some(val) = hm.get(&key) {
                    Ok(Cc::clone(val))
                } else {
                    Err(format!("key {} not found", args[1]))
                }
            } else {
                Err(format!("keys must be hashable ({} is not hashable)", args[1]))
            }
        } else {
            Err(format!("#get expected a hash map (got {:?})", eo))
        }
    } else {
        Err(format!("#get expected a hash map (got {})", args[0]))
    }
}

namespace! { BUILTINS;
    add => "+",
    sub => "-",
    mul => "*",
    div => "/",
    rem => "mod",
    cmp => "cmp",
    head => "head",
    tail => "tail",
    cons => "cons",
    append => "cat",
    len => "len",
    lookup => "get",
    make_list => "list",
    println_fn => "print",
    parse_fn => "parse",
    hash_constructor => "#new",
    hash_lookup => "#get",
    get_int => "int?",
    get_float => "float?",
    get_string => "string?",
    get_ident => "ident?",
    get_native_fn => "native-fn?",
    get_extern_fn => "extern-fn?",
    get_extern_obj => "extern-obj?",
    get_list => "list?"
}

use bacon_rajan_cc::Cc;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Shl, Shr, Sub};
use std::iter::once;

use crate::expr::*;
use crate::interpreter::Namespace;

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

fn eq(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("=", args, 2);
    let a = Cc::clone(&args[0]);
    let b = Cc::clone(&args[1]);
    Ok(Cc::new(match (&*a, &*b) {
        (SExpr::Atom(Atom::Native(na)), SExpr::Atom(Atom::Native(nb)))
            => SExpr::Atom(Atom::Native({
                use NativeAtom::{Int, Float, Str};
                Int(match (na, nb) {
                    (Int(ia),   Int(ib)  ) => ia == ib,
                    (Int(ia),   Float(fb)) => (*ia as f64) == *fb,
                    (Float(fa), Int(ib)  ) => *fa == (*ib as f64),
                    (Float(fa), Float(fb)) => fa == fb,
                    (Str(sa),   Str(sb)  ) => sa == sb,
                    _ => { return Err(format!("= expects int, float, or string (got {} . {})", a, b)) }
                } as i64)
            })),
        _ => { return Err(format!("= expects int, float, or string (got {} . {})", a, b)) }
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

fn len(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    wrong_args_check!("len", args, 1);
    Ok(Cc::new(SExpr::int(args[0].get_list().ok_or_else(|| format!("len expected list, got {}", args[0]))?.len() as i64)))
}

fn quote(args: Vec<Cc<SExpr>>) -> Result<Cc<SExpr>, String> {
    Ok(Cc::new(SExpr::list(args)))
}

namespace! { BUILTINS;
    add => "+",
    sub => "-",
    mul => "*",
    div => "/",
    rem => "mod",
    eq => "=",
    head => "head",
    tail => "tail",
    cons => "cons",
    len => "len",
    quote => "'"
}

// lazy_static! {
//     static ref BUILTINS: Namespace = {
//         let mut module = Namespace::new();
//         module.insert("+", SExpr::extern_fn( ExternFunction{ func: add } ));
//         module.insert("-", SExpr::extern_fn( ExternFunction{ func: sub } ));
//         module.insert("*", SExpr::extern_fn( ExternFunction{ func: mul } ));
//         module.insert("/", SExpr::extern_fn( ExternFunction{ func: div } ));
//         module.insert("mod", SExpr::extern_fn( ExternFunction{ func: rem } ));
//         module.insert("head", SExpr::extern_fn( ExternFunction{ func: head } ));
//         module.insert("tail", SExpr::extern_fn( ExternFunction{ func: tail } ));
//         module.insert("'", SExpr::extern_fn( ExternFunction{ func: quote } ));
//         module
//     }
// }

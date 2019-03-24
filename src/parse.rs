use std::fs;
use std::rc::Rc;
use std::iter::once;
use std::char::from_u32;

use pest::Parser;
use pest::iterators::Pair;
use pest::error::Error;

use crate::expr::{SExpr, Atom, NativeAtom};

#[derive(Parser)]
#[grammar = "krisp.pest"]
struct KrispParser;

pub fn parse_source_file<S: AsRef<str>>(path: S) -> Result<Vec<SExpr>, String> {
    let code = fs::read_to_string(path.as_ref()).map_err(|e| format!(r#"error reading file "{}": {}"#, path.as_ref(), e))?;
    Ok(KrispParser::parse(Rule::program, &code)
        .map_err(|e| format!("syntax error: {}", e))
        ?
        .filter(|pair| if let Rule::EOI = pair.as_rule() { false } else { true })
        .map(SExpr::from)
        .collect())
}

pub fn parse_expr<S: Into<String>>(code: S) -> Result<SExpr, String> {
    let code_owned = code.into();
    KrispParser::parse(Rule::single_expr, &code_owned)
        .map(|mut iter| iter.next().unwrap())
        .map(SExpr::from)
        .map_err(|e| format!("syntax error: {}", e))
}

impl <'a> From<Pair<'a, Rule>> for SExpr {
    fn from(pair: Pair<'a, Rule>) -> SExpr {
        match pair.as_rule() {
            Rule::sexpr => SExpr::from(pair.into_inner().next().unwrap()),
            Rule::atom => SExpr::Atom(Atom::Native({
                let atom = pair.into_inner().next().unwrap();
                match atom.as_rule() {
                    Rule::literal => {
                        let literal = atom.into_inner().next().unwrap();
                        match literal.as_rule() {
                            Rule::float => NativeAtom::Float(literal.as_str().parse::<f64>().unwrap()),
                            Rule::int => NativeAtom::Int(literal.as_str().parse::<i64>().unwrap()),
                            Rule::string => NativeAtom::Str(apply_string_escapes(literal)),
                            _ => unreachable!()
                        }
                    },
                    Rule::ident => NativeAtom::Ident(atom.as_str().to_owned()),
                    _ => unreachable!()
                }
            })),
            
            Rule::list => SExpr::List(pair.into_inner().map(SExpr::from).map(Rc::new).collect()),

            Rule::quoted_list => SExpr::List(once(SExpr::ident("'".to_owned()))
                                                .chain(pair.into_inner().map(SExpr::from))
                                                .map(Rc::new).collect()),

            Rule::progn_list => SExpr::List(once(SExpr::ident("progn".to_owned()))
                                                .chain(pair.into_inner().map(SExpr::from))
                                                .map(Rc::new).collect()),
            _ => unreachable!()
        }
    }
}

fn apply_string_escapes(unescaped: Pair<Rule>) -> String {
    let inner = unescaped.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::squoted_ident => (&inner.as_str()[1..]).to_owned(),
        Rule::dquoted_string => {
            let mut escaped = String::new();
            for outer_char in inner.into_inner() {
                let ch = outer_char.into_inner().next().unwrap();
                escaped.push(match ch.as_rule() {
                    Rule::unescaped_char => ch.as_str().chars().next().unwrap(),
                    Rule::classic_escape => match ch.as_str().chars().nth(1).unwrap() {
                        '\\' => '\\',
                        '"'  => '"',
                        'b'  => '\x08',
                        'f'  => '\x0c',
                        'n'  => '\n',
                        'r'  => '\r',
                        't'  => '\t',
                        _ => unreachable!()
                    },
                    Rule::ascii_escape => char::from(u8::from_str_radix(&ch.as_str()[2..], 16).unwrap()),
                    Rule::unicode4_escape | Rule::unicode8_escape => from_u32(u32::from_str_radix(&ch.as_str()[2..], 16).unwrap()).unwrap(),
                    _ => unreachable!()
                });
            }
            escaped
        },
        _ => unreachable!()
    }
}

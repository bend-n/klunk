use super::*;
use chumsky::prelude::*;
use match_deref::match_deref;
use std::collections::{hash_map::Entry, HashMap};

#[derive(Eq, Clone, Copy, Debug)]
pub struct Proto<'s> {
    name: &'s str,
    prec: Precedence,
}

impl<'s> Proto<'s> {
    pub fn new(name: &'s str) -> Proto<'s> {
        Proto {
            name,
            prec: Precedence::Ambigous,
        }
    }

    pub fn then(self, prec: Precedence) -> Proto<'s> {
        Self { prec, ..self }
    }
}

impl std::hash::Hash for Proto<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl PartialEq for Proto<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Precedence {
    Ambigous,
    Prefix(u16),
    Postfix(u16),
    Infix(u16),
}

#[derive(Debug, Clone)]
pub struct Context<'s> {
    precedence: HashMap<Proto<'s>, Vec<&'s str>>,
}

#[derive(thiserror::Error, Debug, Clone)]
pub enum Err<'s> {
    #[error("unable to find function {0}. are you sure it exists?")]
    CannotFind(Spanned<&'s str>),
    #[error("problem.")]
    Mismatch(Spanned<(Precedence, Fix)>),
    #[error("{0} != 2")]
    InfixWithNot2(u8),
    #[error("you cant bind higher than top")]
    TtTop(Span),
    #[error("you cant bind lower than bottom")]
    LtBot(Span),
}

/// precedence of "function" defined as (infix) tt bottom (postfix) tt bottom (prefix) lt top
impl<'s> Context<'s> {
    fn add(&mut self, dat: FixMeta, proto: Proto<'s>) -> Result<(), Err> {
        use Fix::*;
        use Precedence::*;
        if dat.fix() == In && proto.argc != 2 {
            return Err(Err::InfixWithNot2(*Spanned::from((proto.argc, dat.span()))));
        }
        if let Some(n) = match_deref! {
            match &dat {
                FixMeta::Data(Deref @ FixMetaData { looser_than: Some(Deref @ &"top"), .. }) =>Some( 0xfffd),
                FixMeta::Default(Deref @ Pre) =>Some( 0xfffe),
                _ => None,
            }
        } {
            let value = match dat.fix() {
                In => Infix(n),
                Pre => Prefix(n),
                Post => Postfix(n),
            };
            match self.precedence.entry(proto) {
                Entry::Occupied(x) => {
                    x.into_mut().push(proto.then(value));
                }
                Entry::Vacant(x) => {
                    x.insert(vec![]);
                }
            };
            return Ok(());
        }
        if let Some(x) = tt
            && *x == "top"
        {
            return Err(Err::TtTop(s));
        } // TODO graphs
        if let Some(lt) = lt {
            let &position = self
                .precedence
                .get(&Proto {
                    name: &*lt,
                    argc: 0,
                })
                .ok_or(Err::CannotFind(lt))?;
            let value = match (position, fix) {
                (Infix(x), In) => Infix(x - 1),
                (Prefix(x), Pre) => Prefix(x - 1),
                (Postfix(x), Post) => Postfix(x - 1),
                (x, y) => return Err(Err::Mismatch(lt.copys((x, y)))),
            };
            self.precedence.insert(proto, value);
        }
        if let Some(lt) = tt {
            let &position = self
                .precedence
                .get(&Proto {
                    name: &*lt,
                    argc: 0,
                })
                .ok_or(Err::CannotFind(lt))?;
            let value = match (position, fix) {
                (Infix(x), In) => Infix(x - 1),
                (Prefix(x), Pre) => Prefix(x - 1),
                (Postfix(x), Post) => Postfix(x - 1),
                (x, y) => return Err(Err::Mismatch(lt.copys((x, y)))),
            };
            self.precedence.insert(proto, value);
        }
        if let Some(gt) = tt {
            let &position = self
                .precedence
                .get(&Proto {
                    name: &*gt,
                    argc: 0,
                })
                .ok_or(Err::CannotFind(gt))?;
            let value = match (position, fix) {
                (Infix(x), In) => Infix(x + 1),
                (Prefix(x), Pre) => Prefix(x + 1),
                (Postfix(x), Post) => Postfix(x + 1),
                (x, y) => return Err(Err::Mismatch(gt.copys((x, y)))),
            };
            self.precedence.insert(proto, value);
        }
        Ok(())
    }
}

#[test]
fn hmm() {
    let mut hm = std::collections::HashMap::new();
    hm.insert(Proto { name: "*", argc: 2 }, 32768 - 1);
    hm.insert(Proto { name: "/", argc: 2 }, 32768 - 1);
    hm.insert(Proto { name: "+", argc: 2 }, 32768 - 2);
    hm.insert(Proto { name: "-", argc: 2 }, 32768 - 2);
    let mut c = Context {
        precedence: HashMap::default(),
    };
    c.add(
        Proto { name: "*", argc: 2 },
        None,
        Some(Spanned::dummy("top")),
        Fix::In,
        (0..0).into(),
    )
    .unwrap();
    c.add(
        Proto { name: "/", argc: 2 },
        None,
        Some(Spanned::dummy("top")),
        Fix::In,
        (0..0).into(),
    )
    .unwrap();
    println!("{c:#?}");
    c.add(
        Proto { name: "+", argc: 2 },
        None,
        Some(Spanned::dummy("*")),
        Fix::In,
        (0..0).into(),
    )
    .unwrap();
    c.add(
        Proto { name: "-", argc: 2 },
        None,
        Some(Spanned::dummy("*")),
        Fix::In,
        (0..0).into(),
    )
    .unwrap();
    panic!("{c:#?}");
}

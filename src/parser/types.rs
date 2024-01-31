use std::ops::Deref;

use crate::lexer::Token;
use beef::lean::Cow;
use chumsky::{
    input::{SpannedInput, Stream},
    prelude::*,
};
use match_deref::match_deref;
pub type Span = SimpleSpan<usize>;
pub type Error<'s> = Rich<'s, Token<'s>, Span>;
pub type Input<'s> = SpannedInput<Token<'s>, SimpleSpan, Stream<crate::lexer::Lexer<'s>>>;

#[derive(Clone)]
pub struct FnDef<'s> {
    pub name: &'s str,
    pub args: Vec<(Type<'s>, Type<'s>)>,
    pub ret: Type<'s>,
    pub block: Option<Vec<Token<'s>>>,
    pub meta: Vec<Meta<'s>>,
}

impl std::fmt::Debug for FnDef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {:?} -> {:?}", self.name, self.args, self.ret)?;
        if let Some(b) = &self.block {
            write!(f, " {b:?}")?;
        }
        write!(f, " {:?}", self.meta)
    }
}

#[derive(Debug, Clone)]
pub enum Stmt<'s> {
    Fn(FnDef<'s>),
}

#[derive(Debug, Clone)]
pub enum Ast<'s> {
    Module(Vec<Stmt<'s>>),
}

#[derive(Clone)]
pub enum Value<'s> {
    Float(f64),
    Int(u64),
    String(Cow<'s, str>),
    Unit,
}

impl std::fmt::Debug for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Float(x) => write!(f, "{x}f"),
            Self::Int(x) => write!(f, "{x}i"),
            Self::String(x) => write!(f, "\"{x}\""),
            Self::Unit => write!(f, "()"),
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub enum Associativity {
    Left,
    Right,
    None,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Fix {
    Pre,
    Post,
    In,
}

#[derive(Clone, Copy)]
pub struct FixMetaData<'s> {
    pub looser_than: Option<Spanned<&'s str>>,
    pub tighter_than: Option<Spanned<&'s str>>,
    pub fixness: Fix,
    pub assoc: Option<Spanned<Associativity>>,
}

impl std::fmt::Debug for FixMetaData<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {{", self.fixness)?;
        if let Some(x) = self.assoc {
            write!(f, " assoc {x:?}")?;
        }
        if let Some(x) = self.looser_than {
            write!(f, " looser {x}")?;
        }
        if let Some(x) = self.tighter_than {
            write!(f, " tighter {x}")?;
        }
        write!(f, " }}")
    }
}

#[derive(Clone, Copy)]
pub enum FixMeta<'s> {
    Default(Spanned<Fix>), // function precedence
    Like(Fix, &'s str, Span),
    Data(Spanned<FixMetaData<'s>>),
}

impl<'s> FixMeta<'s> {
    pub fn span(&self) -> Span {
        match self {
            Self::Default(x) => x.span,
            Self::Like(_, _, x) => x,
            Self::Data(x) => x.span,
        }
    }

    pub fn fix(&self) -> Fix {
        match_deref! {
            match self {
                Self::Default(Deref @ x) => *x,
                Self::Like(x,..) => *x,
                Self::Data(Deref @ FixMetaData { fixness, .. }) => *fixness,
            }
        }
    }
}

impl std::fmt::Debug for FixMeta<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Default(x) => write!(f, "{x:?}"),
            Self::Like(x, y, _) => write!(f, "{x:?} {{ like {y} }}"),
            Self::Data(x) => write!(f, "{x:?}"),
        }
    }
}

#[derive(Clone)]
pub enum Meta<'s> {
    Fix(FixMeta<'s>),
    Alias(Vec<&'s str>),
}

impl std::fmt::Debug for Meta<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fix(fix) => write!(f, "{fix:?}"),
            Self::Alias(what) => write!(f, "alias {what:?}"),
        }
    }
}

#[derive(Clone)]
pub enum Expr<'s> {
    Value(Value<'s>),
    Ident(&'s str),

    Let {
        name: &'s str,
        rhs: Box<Expr<'s>>,
    },
    If {
        cond: Box<Expr<'s>>,
        when: Box<Expr<'s>>,
        or: Box<Expr<'s>>,
    },
    Semicolon(Box<Expr<'s>>, Box<Expr<'s>>),
    Call(&'s str, Vec<Expr<'s>>),
}

impl std::fmt::Debug for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(x) => write!(f, "{x:?}"),
            Self::Ident(x) => write!(f, "{x}"),
            Self::Let { name, rhs } => write!(f, "let {name} = {rhs:?}"),
            Self::If { cond, when, or } => {
                write!(f, "if {cond:?} {{ {when:?} }} else {{ {or:?} }}")
            }
            Self::Semicolon(arg0, arg1) => f.debug_list().entries([arg0, arg1]).finish(),
            Self::Call(arg, x) => f.debug_tuple("callu").field(arg).field(x).finish(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> Spanned<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            inner: f(self.inner),
            span: self.span,
        }
    }

    pub fn dummy(inner: T) -> Spanned<T> {
        Spanned {
            inner,
            span: SimpleSpan::new(0, 0),
        }
    }

    pub fn copys<U>(&self, with: U) -> Spanned<U> {
        Spanned {
            inner: with,
            span: self.span,
        }
    }
}

impl<T> From<(T, Span)> for Spanned<T> {
    fn from((inner, span): (T, Span)) -> Self {
        Self { inner, span }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner)
    }
}

#[derive(Clone)]
pub enum Type<'s> {
    Tuple(Box<[Type<'s>]>),
    Path(&'s str),
    Unit,
}

impl std::fmt::Debug for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tuple(x) => write!(
                f,
                "{}",
                std::iter::once("(".to_string())
                    .chain(x.iter().map(|x| format!("{x:?}")).intersperse(", ".into()),)
                    .chain([")".to_string()])
                    .reduce(|acc, x| acc + &x)
                    .unwrap()
            ),
            Self::Path(x) => write!(f, "{x}"),
            Self::Unit => write!(f, "()"),
        }
    }
}

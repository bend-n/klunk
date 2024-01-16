use super::types::*;

macro_rules! tok {
    (ident) => {
        select! { Token::Ident(ident) => ident }.labelled("ident")
    };
    (fnname) => {
        select! { Token::Ident(x) | Token::FnIdent(x) => x }.labelled("function name")
    };
    (let) => {
        just(Token::Let)
    };
    (looser_than) => {
        just(Token::LooserThan)
    };
    (tighter_than) => {
        just(Token::TighterThan)
    };
    (associativity) => {
        just(Token::Associativity)
    };
    (if) => {
        just(Token::If)
    };
    (alias) => {
        just(Token::Alias)
    };
    (like) => {
        just(Token::Like)
    };
    (infix) => {
        just(Token::Infix)
    };
    (prefix) => {
        just(Token::Prefix)
    };
    (postfix) => {
        just(Token::Postfix)
    };
    (else) => {
        just(Token::Else)
    };
    (=) => {
        just(Token::Equal)
    };
    (;) => {
        just(Token::Semicolon)
    };
    (,) => {
        just(Token::Comma)
    };
    (:) => {
        just(Token::Colon)
    };
    (->) => {
        just(Token::ThinArrow)
    };
    (()) => {
        just(Token::Unit)
    };
    (lparen) => {
        just(Token::OpeningBracket('('))
    };
    (rparen) => {
        just(Token::ClosingBracket(')'))
    };
    (lbrack) => {
        just(Token::OpeningBracket('['))
    };
    (rbrack) => {
        just(Token::ClosingBracket(']'))
    };
    (lbrace) => {
        just(Token::OpeningBracket('{'))
    };
    (rbrace) => {
        just(Token::ClosingBracket('}'))
    };
}
macro_rules! parser {
    ($t:ty) => {
        impl Parser<'s, SpannedInput<Token<'s>, SimpleSpan, Stream<Lexer<'s>>>, $t, extra::Err<Error<'s>>> + Clone
    }
}

macro_rules! spanned {
    () => {
        |a, extra| (a, extra.span())
    };
}

pub(crate) use parser;
pub(crate) use spanned;
pub(crate) use tok;

pub trait Unit<T> {
    fn empty(&self) -> T;
}

impl<T> Unit<Option<()>> for Option<T> {
    fn empty(&self) -> Option<()> {
        self.as_ref().map(|_| ())
    }
}

pub trait Spanner {
    fn spun(self, s: Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        (self, s)
    }
}
impl<T> Spanner for T {}

pub trait MapLeft<T, V> {
    fn ml<U>(self, f: impl Fn(T) -> U) -> (U, V);
}

impl<T, V> MapLeft<T, V> for (T, V) {
    fn ml<U>(self, f: impl Fn(T) -> U) -> (U, V) {
        (f(self.0), self.1)
    }
}

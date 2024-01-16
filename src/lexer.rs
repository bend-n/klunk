use beef::lean::Cow;
use chumsky::span::SimpleSpan;
use logos::{Lexer as RealLexer, Logos, SpannedIter};

macro_rules! tokens {
    ($($z:literal $( | $y:literal)? => $v:ident,)+) => {
        #[derive(Logos, Debug, PartialEq, Clone)]
        #[logos(skip r"[\n\s]+")]
        pub enum Token<'strings> {
            #[regex("//[^\n]+", priority = 8)]
            // #[regex(r"/\*[\s\S]+\*/", priority = 8)]
            Comment(&'strings str),
            #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
            #[regex(r"0[xX][0-9a-fA-F]+", |lex| u64::from_str_radix(&lex.slice()[2..], 16).ok())]
            #[regex(r"0[bB][01]+", |lex| u64::from_str_radix(&lex.slice()[2..], 2).ok())]
            Int(u64),
            #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
            Float(f64),
            #[regex(r#""([^\\"\n])*""#, callback = |lex| Cow::from(&lex.slice()[1..lex.slice().len()-1]), priority = 12)]
            #[regex(r#""[^"]*""#, callback = |lex| Cow::from(lex.slice()[1..lex.slice().len()-1].replace(r"\n", "\n")), priority = 8)]
            String(Cow<'strings, str>),
            #[regex(r"[a-z_α-ωA-Z]['̇A-Za-z0-9_α-ω]*", priority = 7)]
            Ident(&'strings str),
            #[regex(r"[^\{\[\(\)\]\}:λ0-9,\?\s][^'̇\?\{\[\(\)\]\},:\s]*", priority = 6)]
            FnIdent(&'strings str),

            #[token("{", chr::<'{'>)]
            #[token("[", chr::<'['>)]
            #[token("(", chr::<'('>)]
            OpeningBracket(char),
            #[token("}", chr::<'}'>)]
            #[token("]", chr::<']'>)]
            #[token(")", chr::<')'>)]
            ClosingBracket(char),

            $(#[token($z, priority = 8)] $(#[token($y, priority = 8)])? $v,)+
        }

        impl std::fmt::Display for Token<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                match self {
                    $(Self::$v => write!(f, $z),)+
                    Self::FnIdent(s) | Self::Ident(s) | Self::Comment(s) => write!(f, "{s}"),
                    Self::String(s) => write!(f, "{s}"),
                    Self::Float(n) => write!(f, "{n}"),
                    Self::Int(n) => write!(f, "{n}"),
                    Self::OpeningBracket(x) | Self::ClosingBracket(x) => write!(f,"{x}"),
                }
            }
        }
    }
}

tokens! {
    "mut" => Mut,
    "let" => Let,
    "static" => Static,
    "impl" => Impl,
    "mod" => Mod,
    "match" => Match,
    "for" => For,
    "break" => Break,
    "enum" => Enum,
    "union" => Union,
    "pub" => Public,
    "typeck" => TypeCheck,
    "struct" => Struct,
    "if" => If,
    "else" => Else,
    "=>" | "⇒" => FatArrow,
    "->" | "→" => ThinArrow,
    "," => Comma,
    ":" => Colon,
    ";" => Semicolon,
    "::" | "∷" => Access,
    "=" => Equal,
    "λ" => Lamba,
    "()" => Unit,
    "prefix" => Prefix,
    "infix" => Infix,
    "postfix" => Postfix,
    "alias" => Alias,
    "associativity" => Associativity,
    "looser_than" => LooserThan,
    "tighter_than" => TighterThan,
    "like" => Like,
}

pub fn lex(s: &str) -> Lexer {
    Lexer {
        inner: Token::lexer(s).spanned(),
    }
}

fn chr<'src, const CHR: char>(_: &mut RealLexer<'src, Token<'src>>) -> Result<char, ()> {
    Ok(CHR)
}
pub struct Lexer<'s> {
    inner: SpannedIter<'s, Token<'s>>,
}

impl<'s> Iterator for Lexer<'s> {
    type Item = (Token<'s>, SimpleSpan<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.find_map(|(x, s)| match x.ok()? {
            Token::Comment(_) => None,
            x => Some((x, SimpleSpan::new(s.start, s.end))),
        })
    }
}

#[test]
fn lexer() {
    let mut lex = lex(r#"
let sint: typeset = { i8, i16 }
- (x: T -> T) [alias neg, prefix { like ¬ }]
mod intrinsics {
	or ((a: T, b: T) -> T) { compiler_defined } [prefix, infix, T ∈ int]
}
enum bool { true, false }
∧ ((a: bool, b: bool) -> bool) { a & b } [infix { associativity <, looser_than « }]
impl bool {
	∧ ((a, b: λ(() -> me)) -> me) {
		match a {
			true => b (),
			false => false,
		}
	}
}
    "#);
    // while let Some(x) = lex.next() { print!("{x} "); }
    macro_rules! test {
        ($($tok:ident$(($var:literal))?)+) => {{
            $(assert_eq!(lex.next().map(|(x,_)|x), Some(Token::$tok$(($var.into()))?));)+
            assert_eq!(lex.next(), None);
        }}
    }
    test! [
    Let
    Ident("sint")
    Colon
    Ident("typeset")
    Equal
    OpeningBracket('{')
    Ident("i8")
    Comma
    Ident("i16")
    ClosingBracket('}')
    FnIdent("-")
    OpeningBracket('(')
    Ident("x")
    Colon
    FnIdent("T")
    ThinArrow
    FnIdent("T")
    ClosingBracket(')')
    OpeningBracket('[')
    Alias
    Ident("neg")
    Comma
    Prefix
    OpeningBracket('{')
    Like
    FnIdent("¬")
    ClosingBracket('}')
    ClosingBracket(']')
    Mod
    Ident("intrinsics")
    OpeningBracket('{')
    Ident("or")
    OpeningBracket('(')
    OpeningBracket('(')
    Ident("a")
    Colon
    FnIdent("T")
    Comma
    Ident("b")
    Colon
    FnIdent("T")
    ClosingBracket(')')
    ThinArrow
    FnIdent("T")
    ClosingBracket(')')
    OpeningBracket('{')
    Ident("compiler_defined")
    ClosingBracket('}')
    OpeningBracket('[')
    Prefix
    Comma
    Infix
    Comma
    FnIdent("T")
    FnIdent("∈")
    Ident("int")
    ClosingBracket(']')
    ClosingBracket('}')
    Enum
    Ident("bool")
    OpeningBracket('{')
    Ident("true")
    Comma
    Ident("false")
    ClosingBracket('}')
    FnIdent("∧")
    OpeningBracket('(')
    OpeningBracket('(')
    Ident("a")
    Colon
    Ident("bool")
    Comma
    Ident("b")
    Colon
    Ident("bool")
    ClosingBracket(')')
    ThinArrow
    Ident("bool")
    ClosingBracket(')')
    OpeningBracket('{')
    Ident("a")
    FnIdent("&")
    Ident("b")
    ClosingBracket('}')
    OpeningBracket('[')
    Infix
    OpeningBracket('{')
    Associativity
    FnIdent("<")
    Comma
    LooserThan
    FnIdent("«")
    ClosingBracket('}')
    ClosingBracket(']')
    Impl
    Ident("bool")
    OpeningBracket('{')
    FnIdent("∧")
    OpeningBracket('(')
    OpeningBracket('(')
    Ident("a")
    Comma
    Ident("b")
    Colon
    Lamba
    OpeningBracket('(')
    Unit
    ThinArrow
    Ident("me")
    ClosingBracket(')')
    ClosingBracket(')')
    ThinArrow
    Ident("me")
    ClosingBracket(')')
    OpeningBracket('{')
    Match
    Ident("a")
    OpeningBracket('{')
    Ident("true")
    FatArrow
    Ident("b")
    Unit
    Comma
    Ident("false")
    FatArrow
    Ident("false")
    Comma
    ClosingBracket('}')
    ClosingBracket('}')
    ClosingBracket('}')
    ]
}

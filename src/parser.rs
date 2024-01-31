use crate::lexer::{Lexer, Token};
use chumsky::{
    input::{SpannedInput, Stream},
    prelude::*,
    Parser,
};
mod expression;
mod types;
mod util;
use types::*;
use util::*;

impl<'s> FixMetaData<'s> {
    pub fn from_pieces(
        x: &[Spanned<FixMetaPiece<'s>>],
        fixness: Fix,
        e: Span,
    ) -> Result<Spanned<Self>, Error<'s>> {
        let mut looser_than = None;
        let mut tighter_than = None;
        let mut assoc = None;
        for &Spanned { inner: x, span } in x {
            match x {
                FixMetaPiece::A(_) if assoc.is_some() => {
                    return Err(Rich::custom(span, "duplicate associatity meta elements"))
                }
                FixMetaPiece::L(_) if looser_than.is_some() => {
                    return Err(Rich::custom(span, "duplicate loosseness meta elements"))
                }
                FixMetaPiece::T(_) if tighter_than.is_some() => {
                    return Err(Rich::custom(span, "duplicate tightness meta elements"))
                }
                FixMetaPiece::A(x) => assoc = Some(Spanned::from((x, span))),
                FixMetaPiece::L(x) => looser_than = Some(Spanned::from((x, span))),
                FixMetaPiece::T(x) => tighter_than = Some(Spanned::from((x, span))),
            }
        }

        Ok(FixMetaData {
            looser_than,
            tighter_than,
            fixness,
            assoc,
        })
        .and_then(|x| match x.looser_than.empty().or(x.tighter_than.empty()) {
            None => Err(Rich::custom(e, "precedence meta required")),
            Some(()) => Ok(x),
        })
        .map(|x| Spanned::from((x, e)))
    }
}

fn expr<'s>() -> parser![Expr<'s>] {
    recursive::<_, Expr, _, _, _>(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Unit => Expr::Value(Value::Unit),
                Token::Int(x) => Expr::Value(Value::Int(x)),
                Token::Float(x) => Expr::Value(Value::Float(x)),
                Token::String(s) => Expr::Value(Value::String(s)),
            }
            .labelled("value");

            let decl = tok![let]
                .ignore_then(tok![ident])
                .then_ignore(tok![=])
                .then(inline_expr.clone())
                .map(|(name, body)| Expr::Let {
                    name,
                    rhs: Box::new(body),
                })
                .labelled("declare")
                .boxed();

            choice((
                tok![ident].map(Expr::Ident),
                decl,
                val,
                expr.clone().delimited_by(tok![lparen], tok![rparen]),
            ))
            .boxed()
        });

        let block = expr
            .clone()
            .delimited_by(tok![lbrace], tok![rbrace])
            .boxed();

        let r#if = recursive(|if_| {
            tok![if]
                .ignore_then(expr.clone())
                .then(block.clone())
                .then(tok![else].ignore_then(block.clone().or(if_)).or_not())
                .map(|((cond, a), b)| Expr::If {
                    cond: Box::new(cond),
                    when: Box::new(a),
                    or: Box::new(b.unwrap_or_else(|| Expr::Value(Value::Unit))),
                })
        });

        let block_expr = block.or(r#if);

        let block_chain = block_expr
            .clone()
            .foldl(block_expr.clone().repeated(), |a, b| {
                Expr::Semicolon(Box::new(a), Box::new(b))
            });

        block_chain.labelled("block").or(inline_expr.clone()).foldl(
            tok![;].ignore_then(expr.or_not()).repeated(),
            |a, b| {
                Expr::Semicolon(
                    Box::new(a),
                    Box::new(b.unwrap_or_else(|| Expr::Value(Value::Unit))),
                )
            },
        )
    })
}

impl<'s> Type<'s> {
    pub fn parse() -> parser![Type<'s>] {
        recursive(|ty| {
            let tuple = ty
                .separated_by(tok![,])
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(tok![lparen], tok![rparen])
                .map(|x| Type::Tuple(x.into_boxed_slice()))
                .boxed();
            let unit = tok![()].map(|_| Type::Unit);
            let path = tok![ident].map(Type::Path);

            choice((path, tuple, unit))
        })
        .labelled("type")
    }
}

#[derive(Debug, Copy, Clone)]
pub enum FixMetaPiece<'s> {
    A(Associativity),
    L(&'s str),
    T(&'s str),
}

impl<'s> Meta<'s> {
    fn fns() -> parser![Vec<&'s str>] {
        tok![fnname]
            .separated_by(tok![,])
            .collect::<Vec<_>>()
            .delimited_by(tok![lbrace].or_not(), tok![rbrace].or_not())
            .labelled("functions")
    }

    fn associativity() -> parser![Spanned<Associativity>] {
        tok![associativity]
            .ignore_then(select! {
                Token::FnIdent("<") => Associativity::Left,
                Token::Ident("none") => Associativity::None,
                Token::FnIdent(">") => Associativity::Right
            })
            .map_with(spanned!())
    }
    fn looser() -> parser![Spanned<&'s str>] {
        tok![looser_than]
            .ignore_then(tok![fnname])
            .map_with(spanned!())
            .labelled("looser than")
    }

    fn tighter() -> parser![Spanned<&'s str>] {
        tok![tighter_than]
            .ignore_then(tok![fnname])
            .map_with(spanned!())
            .labelled("tighter than")
    }

    fn like() -> parser![&'s str] {
        tok![like]
            .ignore_then(tok![fnname])
            .delimited_by(tok![lbrace], tok![rbrace])
            .labelled("like")
    }

    fn alias() -> parser![Meta<'s>] {
        tok![alias]
            .ignore_then(Meta::fns())
            .map(Meta::Alias)
            .labelled("alias meta")
    }

    fn pieces() -> parser![Vec<Spanned<FixMetaPiece<'s>>>] {
        use FixMetaPiece::*;
        choice((
            Meta::associativity().map(|x| x.map(A)),
            Meta::looser().map(|x| x.map(L)),
            Meta::tighter().map(|x| x.map(T)),
        ))
        .separated_by(tok![,])
        .collect::<Vec<_>>()
        .labelled("meta pices")
    }

    fn prefix() -> parser![Meta<'s>] {
        tok![prefix]
            .ignore_then(choice((
                Meta::like().map_with(|x, e| Meta::Fix(FixMeta::Like(Fix::Pre, x, e.span()))),
                Meta::pieces()
                    .try_map(|x, e| {
                        FixMetaData::from_pieces(&x, Fix::Pre, e)
                            .and_then(|x| match x.assoc {
                                Some(_) => {
                                    Err(Rich::custom(e, "prefix does not have associativity"))
                                }
                                None => Ok(x),
                            })
                            .map(|x| Meta::Fix(FixMeta::Data(x)))
                    })
                    .delimited_by(tok![lbrace], tok![rbrace]),
                empty().map_with(|(), e| Meta::Fix(FixMeta::Default(e.tspn(Fix::Pre)))),
            )))
            .labelled("prefix meta")
    }

    fn postfix() -> parser![Meta<'s>] {
        tok![postfix]
            .ignore_then(choice((
                Meta::like().map_with(|x, e| Meta::Fix(FixMeta::Like(Fix::Post, x, e.span()))),
                Meta::pieces()
                    .try_map(|x, e| {
                        FixMetaData::from_pieces(&x, Fix::Post, e)
                            .and_then(|x| match x.assoc {
                                Some(_) => {
                                    Err(Rich::custom(e, "postfix does not have associativity"))
                                }
                                None => Ok(x),
                            })
                            .map(|x| Meta::Fix(FixMeta::Data(x)))
                    })
                    .delimited_by(tok![lbrace], tok![rbrace]),
                empty().map_with(|(), e| Meta::Fix(FixMeta::Default(e.tspn(Fix::Post)))),
            )))
            .labelled("postfix meta")
    }

    fn infix() -> parser![Meta<'s>] {
        tok![infix]
            .ignore_then(choice((
                Meta::like().map_with(|x, e| Meta::Fix(FixMeta::Like(Fix::In, x, e.span()))),
                Meta::pieces()
                    .try_map(|x, e| {
                        FixMetaData::from_pieces(&x, Fix::In, e)
                            .and_then(|x| match x.assoc {
                                None => Err(Rich::custom(e, "infix requires associativity")),
                                Some(_) => Ok(x),
                            })
                            .map(|x| Meta::Fix(FixMeta::Data(x)))
                    })
                    .delimited_by(tok![lbrace], tok![rbrace]),
                empty().map_with(|(), e| Meta::Fix(FixMeta::Default(e.tspn(Fix::In)))),
            )))
            .labelled("infix meta")
    }

    fn parse() -> parser![Vec<Meta<'s>>] {
        choice((
            Meta::alias(),
            Meta::infix(),
            Meta::prefix(),
            Meta::postfix(),
        ))
        .separated_by(tok![,])
        .collect::<Vec<_>>()
        .delimited_by(tok![lbrack], tok![rbrack])
        .labelled("meta")
    }
}

impl<'s> FnDef<'s> {
    pub fn args() -> parser![(Vec<(Type<'s>, Type<'s>)>, Option<Type<'s>>)] {
        Type::parse()
            .then_ignore(tok![:])
            .then(Type::parse())
            .separated_by(tok![,])
            .collect::<Vec<_>>()
            // note: `(a: _, b: _) -> c` is technically invalid, and should be (a, b): (_, _) -> c, but
            .delimited_by(tok![lparen].or_not(), tok![rparen].or_not())
            .then(tok![->].ignore_then(Type::parse()).or_not())
            .labelled("function signature")
    }

    fn block() -> parser![Vec<Token<'s>>] {
        any()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(tok![lbrace], tok![rbrace])
            .labelled("block")
    }

    fn parse() -> parser![Self] {
        tok![fnname]
            .then(Self::args().delimited_by(tok![lparen], tok![rparen]))
            .then(Self::block().or_not())
            .then(Meta::parse())
            .map(|(((name, (args, ret)), block), meta)| Self {
                name,
                args,
                ret: ret.unwrap_or(Type::Unit),
                block,
                meta,
            })
            .labelled("function")
    }
}

fn parser<'s>() -> parser![Ast<'s>] {
    FnDef::parse()
        .map(Stmt::Fn)
        .repeated()
        .collect()
        .map(Ast::Module)
}

pub fn stream(lexer: Lexer<'_>, len: usize) -> SpannedInput<Token<'_>, Span, Stream<Lexer<'_>>> {
    Stream::from_iter(lexer).spanned((len..len).into())
}

#[cfg(test)]
pub fn code<'s>(x: &'s str) -> SpannedInput<Token<'s>, Span, Stream<Lexer<'s>>> {
    stream(crate::lexer::lex(x), x.len())
}

pub fn parse(tokens: Lexer<'_>, len: usize) -> Result<Ast<'_>, Vec<Error<'_>>> {
    parser().parse(stream(tokens, len)).into_result()
}

#[test]
fn fn_test() {
    assert_eq!(Meta::fns().parse(code("{ !, not }")).unwrap(), ["!", "not"]);
    assert_eq!(Meta::fns().parse(code("!")).unwrap(), ["!"]);
}

#![feature(iter_intersperse)]
use chumsky::error::RichReason;
use comat::cformat as cmt;
use std::process::ExitCode;
mod lexer;
mod parser;

fn main() -> ExitCode {
    let Some(Ok(code)) = std::env::args().nth(1).map(std::fs::read_to_string) else {
        comat::cprintln!("{bold_red}error{reset}: where file?");
        return ExitCode::FAILURE;
    };
    let lexer = lexer::lex(&code);
    match parser::parse(lexer, code.len()) {
        Ok(x) => dbg!(x),
        Err(e) => {
            for e in e.into_iter().map(|e| e.map_token(|c| c.to_string())) {
                let mut o = lerr::Error::new(&code);
                o.label((e.span().into_range(), "here"));
                match e.reason() {
                    RichReason::Custom(x) => {
                        o.message(cmt!("{red}error{reset}: {x}"));
                    }
                    RichReason::ExpectedFound { .. } => {
                        o.message(cmt!("{red}error{reset}: {e}"));
                    }
                    RichReason::Many(x) => {
                        match &x[..] {
                            [x, rest @ ..] => {
                                o.message(cmt!("{red}error{reset}: {x}"));
                                for elem in rest {
                                    o.note(cmt!("{yellow}also{reset}: {elem}"));
                                }
                            }
                            _ => unreachable!(),
                        };
                    }
                }
                for (l, span) in e.contexts() {
                    o.label((
                        span.into_range(),
                        cmt!("{yellow}while parsing this{reset}: {l}"),
                    ));
                }
                eprintln!("{o}");
            }
            return ExitCode::FAILURE;
        }
    };

    ExitCode::SUCCESS
}

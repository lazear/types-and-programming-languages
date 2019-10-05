mod context;
mod lexer;
mod parser;
use parser::Parser;

use parser::Term;

fn shift1(d: usize, c: usize, tm: &Term) -> Term {
    match tm {
        Term::TmVar(sp, x) => {
            if *x >= c {
                Term::TmVar(*sp, *x + d)
            } else {
                Term::TmVar(*sp, *x)
            }
        }
        Term::TmAbs(sp, x) => Term::TmAbs(*sp, shift1(d, c + 1, &x).into()),
        Term::TmApp(sp, a, b) => Term::TmApp(*sp, shift1(d, c, &a).into(), shift1(d, c, &b).into()),
    }
}

fn shift(d: usize, tm: &Term) -> Term {
    shift1(d, 0, tm)
}

fn main() {
    let input = "(λ x. x x) (λ x. x x) λ x. λ y. y λ x. λ x. x";
    let mut p = Parser::new(input);
    while let Some(tm) = p.parse_term() {
        println!("{:?}", &tm);
        dbg!(shift(1, &tm));
    }

    dbg!(p.ctx());

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("\n{} error(s) detected while parsing!", diag.error_count());
        println!("{}", diag.emit());
    }
}

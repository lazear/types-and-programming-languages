mod context;
mod lexer;
mod parser;
use parser::Parser;

fn main() {
    let input = "(λ x. λ y. x) z";
    let mut p = Parser::new(input);
    while let Some(tm) = p.parse_term() {
        println!("{:?}", tm);
    }

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("\n{} error(s) detected while parsing!", diag.error_count());
        println!("{}", diag.emit());
    }
}

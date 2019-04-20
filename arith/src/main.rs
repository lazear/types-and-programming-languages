mod lexer;
mod parser;
mod span;

use parser::{Parser, Term};

fn main() {
    let input = "succ(succ(succ(succ(0)))); 0; 0; 0";
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

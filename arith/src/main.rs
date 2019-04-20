mod lexer;
mod parser;
mod span;

use lexer::Lexer;
use parser::{Parser, Term};

fn main() {
    let input = "iszero(succ(succ(succ(0))))";
    let mut p = Parser::new(input);
    while let Some(tm) = p.parse_term() {
        println!("{:?}", tm);
    }
}

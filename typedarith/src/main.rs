use util;
mod ast;
mod lexer;
mod parser;
use parser::Parser;
use ast::*;


fn main() {
    let input = "if iszero(succ(zero)) then pred(0) else succ(4)";
    let mut p = Parser::new(input);
    while let Some(tm) = p.parse_term() {
        print!("{:?} ==> ", tm);
        println!("{:?}", typing(tm));
    }

    let diag = p.diagnostic();
    if diag.error_count() > 0 {
        println!("\n{} error(s) detected while parsing!", diag.error_count());
        println!("{}", diag.emit());
    }
}

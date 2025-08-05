#![allow(dead_code)]
mod parse;
use parse::tokenize;

fn main() {
    let input = String::from("4 + 5 * (1 + var)");
    dbg!(&input);
    let tokens = tokenize(input);
    tokens.print_all();

}

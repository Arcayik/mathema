mod lexer;
mod token;

use lexer::*;

fn main() {
    let input = String::from("4 + 5 * (1 + var)");
    dbg!(&input);
    let tokens = tokenize(input);
    tokens.print();
}

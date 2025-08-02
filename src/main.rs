#![allow(unused)]
mod lexer;
mod token;

use lexer::*;

fn main() {
    let input = String::from(" + - var");
    let tokens = tokenize(input);
}

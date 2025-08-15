mod parse;

use parse::tokenize;

use crate::parse::parse_expr;

fn main() {
    assert!(parse("1 + 1"));
    assert!(parse("1 + 3 * 5"));
    assert!(parse("1 * 3 + 5"));
    assert!(!parse("1 + 3 * 5 var"));
    assert!(!parse("1 3 * 5 var"));

    assert!(parse("1 + -1"));
    assert!(parse("-4 + -1"));
    assert!(parse("--4 + -1"));
}

fn parse(input: &'static str) -> bool {
    let input = String::from(input);
    let (tokens, errors) = tokenize(&input);
    if !errors.is_empty() {
        errors.iter().for_each(|e| println!("{e}"));
        return false;
    } 

    let mut output = true;

    let ast = parse_expr(tokens);
    if let Err(ref e) = ast {
        let span = e.span();
        println!("{}", input);
        (0..span.start).for_each(|_| print!(" "));
        (span.start..span.end).for_each(|_| print!("^"));
        println!();
        output = false;
    }
    println!("AST: \n{:#?}", ast);
    output
}

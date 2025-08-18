mod parse;
mod diagnostic;

use std::io::Write;

use parse::{tokenize, parse_expr, Expr, AstNode};
use diagnostic::Diagnostic;

pub struct Prompt {
    prefix: String,
}

impl Prompt {
    pub fn new(prefix: String) -> Self {
        Prompt { prefix }
    }

    pub fn get_line(&self) -> String {
        let mut input = String::new();
        print!("{}", self.prefix);
        let _ = std::io::stdout().flush();

        match std::io::stdin().read_line(&mut input) {
            Ok(_) => input = input.chars().filter(char::is_ascii).collect(),
            Err(e) => println!("I/O error: {}", e),
        }
        input.trim().to_string()
    }

    pub fn show_diagnostic(&self, diagnostic: &Diagnostic) {
        diagnostic.highlight_span(3);
        println!("{}", diagnostic);
    }
}

fn main() {
    let prompt = Prompt::new(">> ".to_string());
    loop {
        let input = prompt.get_line();
        if input == "exit" { break }

        let ast = parse_ast(&input);
        if let Err(errors) = ast {
            errors.iter().for_each(|d| prompt.show_diagnostic(d));
            continue;
        }
        let expr = ast.unwrap();

        let eval_result = expr.eval();
        if let Err(e) = eval_result {
            prompt.show_diagnostic(&e.into());
            continue;
        };
        let answer = eval_result.unwrap();
        println!("{}", answer);
    }
}

fn parse_ast<T: ToString>(input: T) -> Result<Expr, Vec<Diagnostic>> {
    let input = input.to_string();
    let (tokens, errors) = tokenize(&input);
    if !errors.is_empty() {
        Err(Diagnostic::from_vec(errors))
    } else {
        parse_expr(tokens).map_err(|e| vec![e.into()])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sum_product_precedence() {
        assert!(parse_test("1 + 1"));
        assert!(parse_test("1 + 3 * 5"));
        assert!(parse_test("1 * 3 + 5"));
    }

    #[test]
    fn unexpected_and_trailing() {
        assert!(!parse_test("1 + +"));
        assert!(!parse_test("1 + 3 * 5 var"));
        assert!(!parse_test("1 3 * 5 var"));
    }

    #[test]
    fn simple_unary() {
        assert!(parse_test("1 + -1"));
        assert!(parse_test("-4 + -1"));
        assert!(parse_test("--4 + -1"));
    }

    fn parse_test(input: &str) -> bool {
        let input = String::from(input);
        let (tokens, errors) = tokenize(&input);
        if !errors.is_empty() {
            errors.iter().for_each(|e| println!("{e}"));
            return false;
        } 

        let ast = parse_expr(tokens);
        println!("AST: \n{:#?}", ast);
        ast.is_ok()
    }
}

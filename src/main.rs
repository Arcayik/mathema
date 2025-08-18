mod parse;

use std::io::Write;

use parse::{tokenize, parse_expr, Expr, ParseError};

fn main() {
    let prompt = Prompt::new(">> ".to_string());
    loop {
        let input = prompt.get_line();
        if input == "exit" { break }

        let ast = parse_ast(&input);
        match ast {
            Ok(expr) => {
                println!("{:?}", expr);
            },
            Err(errors) => {
                errors.iter().for_each(|e| {
                    println!("{}", input);
                    e.show_user(0)
                });
            }
        }
    }
}

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
}

fn parse_ast<T: ToString>(input: T) -> Result<Expr, Vec<ParseError>> {
    let input = input.to_string();
    let (tokens, errors) = tokenize(&input);
    if !errors.is_empty() {
        Err(errors.into_iter().map(|le| ParseError::from(le)).collect::<Vec<_>>())
    } else {
        parse_expr(tokens).map_err(|e| vec![e])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_tests() {
        assert!(parse_test("1 + 1"));
        assert!(parse_test("1 + 3 * 5"));
        assert!(parse_test("1 * 3 + 5"));
        assert!(!parse_test("1 + 3 * 5 var"));
        assert!(!parse_test("1 3 * 5 var"));

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

        let mut output = true;

        let ast = parse_expr(tokens);
        if let Err(ref e) = ast {
            e.show_user(0);
            output = false;
        }
        println!("AST: \n{:#?}", ast);
        output
    }
}

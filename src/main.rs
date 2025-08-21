mod parse;
mod diagnostic;

use std::io::Write;

use parse::{tokenize, parse_stmt, Stmt, AstNode};
use diagnostic::Diagnostic;

use crate::context::Context;

mod context {
    use std::{collections::HashMap, f64::consts::{E, PI}};

    pub struct Context {
        variables: HashMap<Box<str>, f64>,
    }

    impl Default for Context {
        fn default() -> Self {
            let mut variables = HashMap::new();
            variables.insert("pi".into(), PI);
            variables.insert("e".into(), E);
            Context { variables }
        }
    }
    
    impl Context {
        pub fn get_variable(&self, name: Box<str>) -> Option<f64> {
            self.variables.get(&name).copied()
        }

        pub fn set_variable(&mut self, name: Box<str>, value: f64) {
            self.variables.insert(name, value);
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

    pub fn show_diagnostic(&self, diagnostic: &Diagnostic) {
        diagnostic.highlight_span(self.prefix.len());
        println!("{}", diagnostic);
    }

    pub fn show_answer(&self, answer: f64) {
        println!("{}", answer);
    }
}

fn main() {
    let mut context = Context::default();
    let prompt = Prompt::new(">> ".to_string());

    loop {
        let input = prompt.get_line();
        if input.is_empty() { continue }
        if input == "exit" { break }

        let ast = parse_ast(&input);
        if let Err(errors) = ast {
            errors.iter().for_each(|d| prompt.show_diagnostic(d));
            continue;
        }
        let stmt = ast.unwrap();

        let process_result = process_statement(&mut context, stmt);
        match process_result {
            Ok(ans) => prompt.show_answer(ans),
            Err(diags) => diags.iter().for_each(|d| prompt.show_diagnostic(d)),
        }
    }
}

fn parse_ast<T: ToString>(input: T) -> Result<Stmt, Vec<Diagnostic>> {
    let input = input.to_string();
    let (tokens, errors) = tokenize(&input);
    if !errors.is_empty() {
        Err(Diagnostic::from_vec(errors))
    } else {
        parse_stmt(tokens).map_err(|e| vec![e.into()])
    }
}

fn process_statement(ctxt: &mut Context, stmt: Stmt) -> Result<f64, Vec<Diagnostic>> {
    match stmt {
        Stmt::Expr(expr) => {
            expr.eval(ctxt).map_err(|e| vec![e.into()])
        },
        Stmt::VarDecl(decl) => {
            let name = decl.var_name.repr;
            match decl.expr.eval(ctxt) {
                Ok(ans) => {
                    ctxt.set_variable(name, ans);
                    Ok(ans)
                }
                Err(e) => Err(vec![e.into()])
            }
        }
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
        assert!(!parse_test("1 / 3 x y"));
        assert!(!parse_test("1 3 * 5 var"));
    }

    #[test]
    fn simple_unary() {
        assert!(parse_test("1 + -1"));
        assert!(parse_test("-4 + -1"));
        assert!(parse_test("--4 + -1"));
    }

    #[test]
    fn parens() {
        assert!(!parse_test("()"));
        assert!(parse_test("(59.9)"));
        assert!(parse_test("1 + (1)"));
    }

    fn parse_test(input: &str) -> bool {
        let input = String::from(input);
        let (tokens, errors) = tokenize(&input);
        if !errors.is_empty() {
            errors.iter().for_each(|e| println!("{e}"));
            return false;
        } 

        let ast = parse_stmt(tokens);
        println!("Input: [{}]", input);
        println!("AST: \n{:#?}", ast);
        ast.is_ok()
    }
}

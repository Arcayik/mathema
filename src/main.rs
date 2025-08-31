mod parse;
mod diagnostic;

use std::io::Write;

use parse::{tokenize, parse_stmt, Stmt};
use diagnostic::Diagnostic;

use crate::{context::Context, parse::{create_function}};

mod context {
    use std::{collections::HashMap, f64::consts::{E, PI}, rc::Rc};

    use crate::parse::Function;

    pub struct Context {
        variables: HashMap<Box<str>, f64>,
        functions: HashMap<Box<str>, Rc<Function>>,
    }

    impl Default for Context {
        fn default() -> Self {
            let mut variables = HashMap::new();
            variables.insert("pi".into(), PI);
            variables.insert("e".into(), E);

            let functions = HashMap::new();
            Context { variables, functions }
        }
    }
    
    impl Context {
        pub fn get_variable(&self, name: &str) -> Option<f64> {
            self.variables.get(name).copied()
        }

        pub fn set_variable(&mut self, name: &str, value: f64) {
            self.variables.insert(name.into(), value);
        }

        pub fn get_function(&self, name: &str) -> Option<Rc<Function>> {
            self.functions.get(name).map(Rc::clone)
        }

        pub fn set_function(&mut self, name: &str, func: Function) {
            // println!("Function '{}' declared", name);
            self.functions.insert(name.into(), func.into());
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
            Outcome::ShowAnswer(ans) => prompt.show_answer(ans),
            Outcome::ShowDiagnostics(diags) => diags.iter().for_each(|d| prompt.show_diagnostic(d)),
            Outcome::AssignVar(..) => {},
            Outcome::DefineFn(..) => {},
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

pub enum Outcome {
    ShowDiagnostics(Vec<Diagnostic>),
    ShowAnswer(f64),
    AssignVar(Box<str>, f64),
    DefineFn(Box<str>)
}

fn process_statement(ctxt: &mut Context, stmt: Stmt) -> Outcome {
    match stmt {
        Stmt::Expr(expr) => {
            let result = expr.eval(ctxt).map_err(Diagnostic::from_vec);
            match result {
                Ok(num) => {
                    ctxt.set_variable("ans", num);
                    Outcome::ShowAnswer(num)
                },
                Err(diags) => Outcome::ShowDiagnostics(diags)
            }
        },
        Stmt::VarDecl(var_decl) => {
            let name = &var_decl.var_name.repr;
            match var_decl.expr.eval(ctxt) {
                Ok(ans) => {
                    ctxt.set_variable(name, ans);
                    Outcome::AssignVar(name.clone(), ans)
                }
                Err(e) => Outcome::ShowDiagnostics(Diagnostic::from_vec(e))
            }
        }
        Stmt::FnDecl(fn_decl) => {
            let name = fn_decl.sig.fn_name.repr;
            let body = fn_decl.body;
            let params: Box<_> = fn_decl.sig.inputs
                .iter()
                .cloned()
                .map(|p| p.repr)
                .collect();

            let result = create_function(name.clone(), params, body, ctxt)
                .map_err(Diagnostic::from);
            match result {
                Ok(f) => {
                    ctxt.set_function(&name, f);
                    Outcome::DefineFn(name)
                },
                Err(d) => Outcome::ShowDiagnostics(vec![d])
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

    #[test]
    fn var_declaration() {
        assert!(parse_test("x = 50"));
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

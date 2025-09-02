use std::io::Write;

use mathema_core::{
    parsing::{tokenize, parse_stmt, Stmt},
    Diagnostic, Context, create_function,
};

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


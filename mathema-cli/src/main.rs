use std::io::Write;

use mathema_core::{
    context::{mathema_parse, Context, MathemaError, Outcome},
    Name
};

mod diagnostic;
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
        diagnostic.highlight_span(self.prefix.len());
        println!("{}", diagnostic);
    }

    pub fn show_answer(&self, answer: f64) {
        println!("{}", answer);
    }

    pub fn show_var_decl(&self, name: &Name, value: f64) {
        println!("set {name} to {value}");
    }

    pub fn show_fn_decl(&self, name: &Name) {
        println!("defined {name}()");
    }
}

fn main() {
    let mut context = Context::default();
    let prompt = Prompt::new(">> ".to_string());

    loop {
        let input = prompt.get_line();
        if input.is_empty() { continue }
        if input == "exit" { break }

        let result = mathema_parse(&mut context, &input);
        match result {
            Ok(outcome) => handle_outcome(&prompt, outcome),
            Err(errs) => handle_errors(&prompt, errs)
        }
    }
}

fn handle_outcome(state: &Prompt, outcome: Outcome) {
    match outcome {
        Outcome::Answer(ans) => state.show_answer(ans),
        Outcome::Var(ref n, v) => state.show_var_decl(n, v),
        Outcome::Fn(ref n) => state.show_fn_decl(n),
    }
}

fn handle_errors(state: &Prompt, errors: Vec<MathemaError>) {
    for e in errors {
        let diag: Diagnostic = e.into();
        state.show_diagnostic(&diag);
    }
}

use std::io::Write;

use mathema_core::{
    context::{mathema_parse, Context, Outcome},
    error::Diagnostic,
    symbol::Symbol
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

    pub fn show_error(&self, error: &dyn Diagnostic) {
        if let Some(src) = error.source_code() {
            println!("{}", src);
        }
        println!("{}", error.message());
    }

    pub fn show_answer(&self, answer: f64) {
        println!("{}", answer);
    }

    pub fn show_var_decl(&self, name: Symbol) {
        println!("declared {name}");
    }

    pub fn show_fn_decl(&self, name: Symbol) {
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
            Err(errs) => errs.iter().for_each(|e| prompt.show_error(e.as_ref()))
        }
    }
}

fn handle_outcome(state: &Prompt, outcome: Outcome) {
    match outcome {
        Outcome::Answer(ans) => state.show_answer(ans),
        Outcome::Var(ref n) => state.show_var_decl(*n),
        Outcome::Fn(ref n) => state.show_fn_decl(*n),
    }
}

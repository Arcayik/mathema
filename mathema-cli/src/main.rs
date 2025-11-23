use std::io::Write;

use mathema_core::{
    context::{mathema_parse, Context, Outcome},
    error::Diagnostic, value::MathemaValue,
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
        if let Some(mut spans) = error.spans() 
            && let Some(span) = spans.next() {
                let until = span.start;
                let len = span.end - span.start;
                print!("{}", " ".repeat(self.prefix.len() + until));
                println!("{}", "^".repeat(len));
        }
        println!("{}", error.message());
    }

    pub fn show_answer(&self, answer: MathemaValue) {
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

        let result = mathema_parse(&mut context, &input);
        match result {
            Ok(outcome) => handle_outcome(&prompt, outcome),
            Err(errs) => handle_errors(&prompt, errs),
        }
    }
}

fn handle_outcome(state: &Prompt, outcome: Outcome) {
    if let Outcome::Answer(ans) = outcome {
        state.show_answer(ans)
    }
}

fn handle_errors(state: &Prompt, errors: Vec<Box<dyn Diagnostic>>) {
    // errors.iter().for_each(|e| state.show_error(e.as_ref()))
    errors.first().map(|e| state.show_error(e.as_ref()));
}

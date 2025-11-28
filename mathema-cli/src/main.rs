use std::io::Write;

use mathema_core::{
    context::{mathema_parse, Context, Outcome},
    error::MathemaError,
    parsing::token::{Span, Spanned},
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

    pub fn show_outcome(&self, outcome: Outcome) {
        if let Outcome::Answer(ans) = outcome {
            println!("{}", ans);
        }
    }

    pub fn show_error(&self, context: &Context, error: &MathemaError) {
        match error {
            MathemaError::Lexer(errs) => {
                let e = errs.first().unwrap();
                self.span_line(e.span());
                println!("{}", e);
            },
            MathemaError::Parser(e) => {
                self.span_line(e.span());
                println!("{}", e);
            },
            MathemaError::Definition(e) => {
            },
            MathemaError::Eval(errs) => {
                let e = errs.first().unwrap();
                self.span_line(e.span);
                println!("{}", e);
            }
        }
    }

    fn span_line(&self, span: Span) {
        print!("{}", " ".repeat(span.start + self.prefix.len()));
        println!("{}", "^".repeat(span.end - span.start));
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
            Ok(outcome) => prompt.show_outcome(outcome),
            Err(error) => prompt.show_error(&context, &error),
        }
    }
}


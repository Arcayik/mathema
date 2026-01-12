use std::io::Write;

mod error;
use error::ErrorDisplay;

use mathema_core::{
    context::{mathema_parse, Context, Outcome},
    parsing::token::{Span, Spanned},
    error::MathemaError,
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
                let err = errs.first().unwrap();
                self.prompt_span_line(err.span());
                println!("{}", err.display(context));
            },
            MathemaError::Parser(e) => {
                self.prompt_span_line(e.span());
                println!("{}", e.display(context));
            },
            MathemaError::Definition(e) => {
                self.prompt_span_line(e.span);
                println!("{}", e.display(context));
            },
            MathemaError::Eval(errs) => {
                let e = errs.first().unwrap();
                println!("{}", e.display(context));
            }
        }
    }

    fn prompt_span_line(&self, span: Span) {
        self.span_line(span, self.prefix.len());
    }

    fn span_line(&self, span: Span, offset: usize) {
        print!("{}", " ".repeat(span.start + offset));
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


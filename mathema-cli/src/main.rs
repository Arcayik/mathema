use std::io::Write;

use mathema_core::{
    context::{mathema_parse, Context, DefErrorKind, FuncError, Outcome, VarError},
    parsing::token::{Span, Spanned},
    algebra::EvalErrorKind,
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

    pub fn show_error(&self, error: &MathemaError) {
        match error {
            MathemaError::Lexer(errs) => {
                let e = errs.first().unwrap();
                self.prompt_span_line(e.span());
                println!("{}", e);
            },
            MathemaError::Parser(e) => {
                self.prompt_span_line(e.span());
                println!("{}", e);
            },
            MathemaError::Definition(e) => {
                self.prompt_span_line(e.span);
                match e.kind {
                    DefErrorKind::ReservedVar(var) => {
                        println!("Reserved var: {var}");
                    },
                    DefErrorKind::ReservedFunc(func) => {
                        println!("Reserved func: {func}");
                    }
                }
            },
            MathemaError::Eval(errs) => {
                let e = errs.first().unwrap();
                // TODO
                // self.prompt_span_line(e.span);

                // since this is straight from an invocation,
                // we can ignore `e.source` since it is literally on the screen

                match &e.kind {
                    EvalErrorKind::BadVar(var_e) => match var_e {
                        VarError::Eval { name: _, errors } => {
                            let e2 = errors.first().unwrap();
                            println!("{:?}", e2.kind);
                        },
                        VarError::NotDefined(symbol) => {
                            println!("Undefined var: {symbol}");
                        }
                    },
                    EvalErrorKind::BadFnCall(func_e) => match func_e {
                        FuncError::Eval { name, errors: _ } => {
                            println!("[fn] {name}");
                        },
                        FuncError::BadArgs(_, _) => {
                            todo!();
                        },
                        FuncError::NotDefined(name) => {
                            println!("Undefined func: {name}",)
                        }
                    }
                }
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
            Err(error) => prompt.show_error(&error),
        }
    }
}


use std::io::Write;

use mathema_core::{
    parsing::{
        lexer::tokenize,
        parser::ParseBuffer, 
        ast::Stmt
    }
};

mod context;
mod diagnostic;

use context::{ScopeContext, Outcome, process_statement};
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
}

fn main() {
    let mut context = ScopeContext::default();
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
        return Err(Diagnostic::from_vec(errors))
    }

    let parser = ParseBuffer::new(tokens);
    parser.parse().map_err(|e| vec![e.into()])
}


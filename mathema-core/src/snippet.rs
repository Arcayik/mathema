use std::collections::{hash_map::{Values, ValuesMut}, HashMap};

use crate::{algebra::{AlgBinOp, AlgExpr, Value}, function::Function, parsing::{ast::Precedence, token::Span}, symbol::Symbol};

pub struct SnippetLine {
    pub(crate) source: String,
    pub(crate) span_map: HashMap<*const AlgExpr, Span>
}

impl SnippetLine {
    pub fn empty() -> Self {
        SnippetLine { 
            source: String::new(),
            span_map: HashMap::new()
        }
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn get_span(&self, key: &AlgExpr) -> Option<Span> {
        let key = key as *const AlgExpr;
        self.span_map.get(&key).copied()
    }

    pub fn spans(&self) -> Values<'_, *const AlgExpr, Span> {
        self.span_map.values()
    }

    pub fn spans_mut(&mut self) -> ValuesMut<'_, *const AlgExpr, Span> {
        self.span_map.values_mut()
    }
}

pub fn create_algebra_snippet(algebra: &AlgExpr) -> SnippetLine {
    fn recurse(
        algebra: &AlgExpr,
        string: &mut String,
        spans: &mut HashMap<*const AlgExpr, Span>,
        mut end: usize,
    ) {
        match algebra {
            AlgExpr::Value(val) => match val {
                Value::Num(num) => string.push_str(&num.to_string()),
                Value::Var(var) => string.push_str(var.as_str()),
            },
            AlgExpr::Unary(un) => {
                let op_str = &un.op.to_string();
                end += op_str.len();
                string.push_str(op_str);
                let mut sub_str = String::new();
                recurse(&un.expr, &mut sub_str, spans, end);
                if !matches!(*un.expr, AlgExpr::Value(_)) {
                    string.push_str(&format!("({})", sub_str));
                } else {
                    string.push_str(&sub_str)
                }
            },
            AlgExpr::Binary(bin) => {
                {
                    let mut left_str = String::new();
                    recurse(&bin.left, &mut left_str, spans, end);
                    end += left_str.len();

                    if Precedence::of_alg(&bin.left) < Precedence::of_alg_binop(&bin.op) {
                        string.push_str(&format!("({})", left_str));
                        end += 2;
                    } else {
                        string.push_str(&left_str);
                    }
                }

                if matches!(bin.op, AlgBinOp::Mul)
                    && matches!(*bin.left, AlgExpr::Value(Value::Num(_)))
                    && matches!(*bin.right, AlgExpr::Value(Value::Var(_)) | AlgExpr::FnCall(_)) {
                        // implicit multiplication, print nothing
                } else {
                    let op_str = &format!(" {} ", bin.op);
                    end += op_str.len();
                    string.push_str(op_str);
                }

                let mut right_str = String::new();
                recurse(&bin.right, &mut right_str, spans, end);
                end += right_str.len();

                if Precedence::of_alg(&bin.right) < Precedence::of_alg_binop(&bin.op) {
                    string.push_str(&format!("({})", right_str));
                    end += 2;
                } else {
                    string.push_str(&right_str)
                }
            },
            AlgExpr::FnCall(fc) => {
                string.push_str(fc.name.as_str());
                string.push('(');

                let mut arg_strings: Vec<String> = Vec::new();
                for arg in &fc.args {
                    let mut arg_str = String::new();
                    recurse(arg, &mut arg_str, spans, end);
                    arg_strings.push(arg_str);
                }
                string.push_str(&arg_strings.join(", "));
                string.push(')');
            }
        };

        let ptr: *const AlgExpr = algebra;
        let span = Span {
            start: end,
            end: end + string.len(),
        };
        spans.insert(ptr, span);
    }

    let mut string = String::new();
    let mut span_map = HashMap::new();

    recurse(algebra, &mut string, &mut span_map, 0);

    SnippetLine { source: string, span_map }
}

pub fn create_var_decl_snippet(name: Symbol, decl: &AlgExpr) -> SnippetLine {
    let mut string = format!("{} = ", name);
    let span_offset = name.as_str().len() + 3; // " = "

    let mut snip = create_algebra_snippet(decl);
    let body = snip.source();
    string.push_str(body);

    for span in snip.spans_mut() {
        span.start += span_offset;
        span.end += span_offset;
    }

    snip.source = string;
    snip
}

pub fn create_function_snippet(name: Symbol, function: &Function) -> SnippetLine {
    let mut string = format!("{} = ", name);
    let span_offset = name.as_str().len() + 3;

    let mut snip = create_algebra_snippet(&function.body);
    let body = snip.source();
    string.push_str(body);

    for span in snip.spans_mut() {
        span.start += span_offset;
        span.end += span_offset;
    }

    snip.source = string;
    snip
}

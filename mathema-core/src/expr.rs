use crate::{
    punctuated::Punctuated,
    stmt::parse_punctuated_group,
    token::{Delimiter, Paren, End, Ident, Literal, Parse, Span, Spanned, Token},
    lexer::LexToken,
    parser::{ParseStream, ParseError},
    context::Context,
};

#[derive(Debug)]
pub enum ExprError {
    UndefinedVar(Ident),
    UndefinedFunc(Ident),
}

pub enum Expr {
    Value(ExprValue),
    Binary(ExprBinary),
    Unary(ExprUnary),
    Group(ExprGroup),
    FnCall(ExprFnCall),
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(t) => write!(f, "{:?}", t),
            Self::Binary(t) => write!(f, "{:?}", t),
            Self::Unary(t) => write!(f, "{:?}", t),
            Self::Group(t) => write!(f, "{:?}", t),
            Self::FnCall(t) => write!(f, "{:?}", t),
        }
    }
}

impl From<ExprValue> for Expr {
    fn from(value: ExprValue) -> Self {
        Expr::Value(value)
    }
}

impl From<ExprBinary> for Expr {
    fn from(value: ExprBinary) -> Self {
        Expr::Binary(value)
    }
}

impl From<ExprUnary> for Expr {
    fn from(value: ExprUnary) -> Self {
        Expr::Unary(value)
    }
}

impl From<ExprGroup> for Expr {
    fn from(value: ExprGroup) -> Self {
        Expr::Group(value)
    }
}

impl From<ExprFnCall> for Expr {
    fn from(value: ExprFnCall) -> Self {
        Expr::FnCall(value)
    }
}

impl Spanned for Expr {
    fn span(&self) -> Span {
        match self {
            Self::Value(e) => e.span(),
            Self::Binary(e) => e.span(),
            Self::Unary(e) => e.span(),
            Self::Group(e) => e.span(),
            Self::FnCall(e) => e.span()
        }
    }
}

impl Parse for Expr {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let lhs = parsing::parse_expr_lhs(input)?;
        parsing::parse_expr(input, lhs, Precedence::MIN)
    }
}

impl Expr {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        match self {
            Self::Value(value) => value.eval(ctxt).map_err(|e| vec![e]),
            Self::Binary(expr) => expr.eval(ctxt),
            Self::Unary(expr) => expr.eval(ctxt),
            Self::Group(group) => group.eval(ctxt),
            Self::FnCall(call) => call.eval(ctxt),
        }
    }
}

pub enum ExprValue {
    Literal(Literal),
    Ident(Ident),
}

impl std::fmt::Debug for ExprValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{}", l.num),
            Self::Ident(i) => write!(f, "{}", i.repr)
        }
    }
}

impl Spanned for ExprValue {
    fn span(&self) -> Span {
        match self {
            Self::Literal(lit) => lit.span(),
            Self::Ident(id) => id.span(),
        }
    }
}

impl Parse for ExprValue {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if input.peek::<Literal>() {
            input.parse().map(ExprValue::Literal)
        } else if input.peek::<Ident>() {
            input.parse().map(ExprValue::Ident)
        } else {
            Err(input.error("Expected literal or ident"))
        }
    }
}

impl ExprValue {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, ExprError> {
        match self {
            Self::Ident(ident) => {
                ctxt.get_variable(&ident.repr)
                    .ok_or_else(|| ExprError::UndefinedVar(ident.clone()))
            }
            Self::Literal(literal) => Ok(literal.num),
        }
    }
}

pub struct ExprBinary {
    pub(super) lhs: Box<Expr>,
    pub(super) op: BinOp,
    pub(super) rhs: Box<Expr>,
}

impl std::fmt::Debug for ExprBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?} {:?})", self.lhs, self.op, self.rhs)
    }
}

impl Spanned for ExprBinary {
    fn span(&self) -> Span {
        let start = self.lhs.span().start;
        let end = self.rhs.span().end;
        Span { start, end }
    }
}

impl Parse for ExprBinary {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        Ok(ExprBinary {
            lhs: Box::new(input.parse()?),
            op: input.parse()?,
            rhs: Box::new(input.parse()?),
        })
    }
}

impl ExprBinary {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        let left = self.lhs.eval(ctxt);
        let right = self.rhs.eval(ctxt);

        match (left, right) {
            (Ok(l), Ok(r)) => match self.op {
                BinOp::Add(_) => Ok(l + r),
                BinOp::Sub(_) => Ok(l - r),
                BinOp::Mul(_) => Ok(l * r),
                BinOp::Div(_) => Ok(l / r),
            },

            (Err(e), Ok(_)) | (Ok(_), Err(e)) => Err(e),

            (Err(mut le), Err(mut re)) => {
                le.append(&mut re);
                Err(le)
            }
        }
    }
}

pub enum BinOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
}

impl std::fmt::Debug for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(_) => write!(f, "{}", <Token![+]>::display()),
            Self::Sub(_) => write!(f, "{}", <Token![-]>::display()),
            Self::Mul(_) => write!(f, "{}", <Token![*]>::display()),
            Self::Div(_) => write!(f, "{}", <Token![/]>::display()),
        }
    }
}

impl Spanned for BinOp {
    fn span(&self) -> Span {
        match self {
            Self::Add(t) => t.span(),
            Self::Sub(t) => t.span(),
            Self::Mul(t) => t.span(),
            Self::Div(t) => t.span(),
        }
    }
}

impl Parse for BinOp {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if input.peek::<Token![+]>() {
            input.parse().map(BinOp::Add)
        } else if input.peek::<Token![-]>() {
            input.parse().map(BinOp::Sub)
        } else if input.peek::<Token![*]>() {
            input.parse().map(BinOp::Mul)
        } else if input.peek::<Token![/]>() {
            input.parse().map(BinOp::Div)
        } else {
            Err(input.error("Expected binary operator"))
        }
    }
}

pub struct ExprUnary {
    pub(super) op: UnaryOp,
    pub(super) expr: Box<Expr>,
}

impl std::fmt::Debug for ExprUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?})", self.op, self.expr)
    }
}

impl Spanned for ExprUnary {
    fn span(&self) -> Span {
        let start = self.op.span().start;
        let end = self.expr.span().end;
        Span { start, end }
    }
}

impl Parse for ExprUnary {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        Ok(ExprUnary {
            op: input.parse()?,
            expr: Box::new(parsing::parse_expr_lhs(input)?),
        })
    }
}

impl ExprUnary {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        let right = self.expr.eval(ctxt);
        if let Ok(r) = right {
            match self.op {
                UnaryOp::Neg(_) => Ok(-r),
            }
        } else {
            right
        }
    }
}

pub enum UnaryOp {
    Neg(Token![-])
}

impl std::fmt::Debug for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Neg(_) => write!(f, "{}", <Token![-]>::display()),
        }
    }
}

impl Spanned for UnaryOp {
    fn span(&self) -> Span {
        match self {
            Self::Neg(t) => t.span(),
        }
    }
}

impl Parse for UnaryOp {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if input.peek::<Token![-]>() {
            input.parse().map(UnaryOp::Neg)
        } else {
            Err(input.error("Expected unary operator"))
        }
    }
}

pub struct ExprGroup {
    pub(super) delim: Delimiter,
    pub(super) expr: Box<Expr>,
}

impl std::fmt::Debug for ExprGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?})", self.delim, self.expr)
    }
}

impl Spanned for ExprGroup {
    fn span(&self) -> Span {
        self.expr.span()
    }
}

impl Parse for ExprGroup {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if let LexToken::Group(g, _) = input.next_token() {
            Ok(ExprGroup {
                delim: g.delim,
                expr: Box::new(input.parse()?)
            })
        } else {
            Err(input.error("Expected group"))
        }
    }
}

impl ExprGroup {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        self.expr.eval(ctxt)
    }
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence { Assign, Sum, Product, Unary, Unambiguous }

impl Precedence {
    pub(super) const MIN: Self = Precedence::Assign;

    pub fn of_binop(op: &BinOp) -> Self {
        match op {
            BinOp::Add(_) | BinOp::Sub(_) => Precedence::Sum,
            BinOp::Mul(_) | BinOp::Div(_) => Precedence::Product
        }
    }

    pub fn of(e: &Expr) -> Self {
        match e {
            Expr::Value(_) => Precedence::Unambiguous,
            Expr::Binary(b) => Precedence::of_binop(&b.op),
            Expr::Unary(_) => Precedence::Unary,
            Expr::Group(_) => Precedence::Unambiguous,
            Expr::FnCall(_) => Precedence::Unambiguous,
        }
    }
}

pub struct ExprFnCall {
    pub name: Ident,
    pub parens: Paren,
    pub inputs: Punctuated<Expr, Token![,]>
}

impl std::fmt::Debug for ExprFnCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({:?})", self.name, self.inputs)
    }
}

impl Spanned for ExprFnCall {
    fn span(&self) -> Span {
        let start = self.name.span().start;
        let end = self.parens.span().end;
        Span { start, end }
    }
}

impl Parse for ExprFnCall {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let name = input.parse()?;
        let parens = input.parse()?;
        Ok(ExprFnCall {
            name,
            parens,
            inputs: parse_punctuated_group(input)?,
        })
    }
}

impl ExprFnCall {
    pub fn eval(&self, ctxt: &Context) -> Result<f64, Vec<ExprError>> {
        let name = &self.name.repr;
        let func = ctxt.get_function(name)
            .ok_or(vec![ExprError::UndefinedFunc(self.name.clone())])?;

        let mut arg_exprs = Vec::new();
        for expr in self.inputs.iter() {
            arg_exprs.push(expr.eval(ctxt)?);
        };

        let call = func.call(&arg_exprs);

        Ok(call)
    }
}

mod parsing {
    use crate::token::Paren;

    use super::*;

    pub fn parse_expr_lhs(input: ParseStream) -> Result<Expr, ParseError> {
        if input.peek::<Token![-]>() {
            input.parse().map(Expr::Unary)
        } else if input.peek::<Ident>() && input.peek2::<Paren>() {
            input.parse().map(Expr::FnCall)
        } else if input.peek::<Literal>() || input.peek::<Ident>() {
            input.parse().map(Expr::Value)
        } else if input.peek::<Paren>() {
            input.parse().map(Expr::Group)
        } else {
            Err(input.error("Expected ident, literal, parens, unary operator"))
        }
    }

    pub fn parse_expr(
        input: ParseStream,
        mut left: Expr,
        base: Precedence
    ) -> Result<Expr, ParseError> {
        loop {
            // NOTE: point of interest
            if input.peek::<End>() {
                break;
            }
            if !peek_binop(input) {
                break
            }

            let begin = input.save_pos();

            let op = input.parse()?;
            let precedence = Precedence::of_binop(&op);

            if precedence < base {
                input.restore_pos(begin);
                break;
            } else {
                left = ExprBinary {
                    lhs: Box::new(left),
                    op,
                    rhs: parse_binop_rhs(input, precedence)?,
                }.into();
            }
        }
        Ok(left)
    }

    fn peek_binop(input: ParseStream) -> bool {
        input.peek::<Token![+]>() || input.peek::<Token![-]>() ||
            input.peek::<Token![*]>() || input.peek::<Token![/]>()
    }

    fn parse_binop_rhs(
        input: ParseStream,
        precedence: Precedence,
    ) -> Result<Box<Expr>, ParseError> {
        let mut rhs = parse_expr_lhs(input)?;
        loop {
            let begin = input.save_pos();
            let next = peek_precedence(input);

            if next > precedence {
                rhs = parse_expr(input, rhs, next)?;
            } else {
                input.restore_pos(begin);
                break;
            }
        }
        Ok(Box::new(rhs))
    }

    fn peek_precedence(input: ParseStream) -> Precedence {
        let begin = input.save_pos();

        let precedence = if let Ok(op) = input.parse() {
            Precedence::of_binop(&op)
        } else if input.peek::<Token![=]>() {
            Precedence::Assign
        } else {
            Precedence::MIN
        };

        input.restore_pos(begin);
        precedence
    }
}

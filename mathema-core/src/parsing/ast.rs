use super::{
    lexer::LexToken,
    parser::{ParseError, ParseStream},
    punctuated::Punctuated,
    token::{Delimiter, End, Ident, Literal, Paren, Parse, Span, Spanned, Token}
};

#[derive(Debug)]
pub enum AstStmt {
    Expr(AstExpr),
    VarDecl(VarDecl),
    FnDecl(FnDecl)
}

impl From<AstExpr> for AstStmt {
    fn from(value: AstExpr) -> Self {
        AstStmt::Expr(value)
    }
}

impl From<VarDecl> for AstStmt {
    fn from(value: VarDecl) -> Self {
        AstStmt::VarDecl(value)
    }
}

impl From<FnDecl> for AstStmt {
    fn from(value: FnDecl) -> Self {
        AstStmt::FnDecl(value)
    }
}

impl Spanned for AstStmt {
    fn span(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.span(),
            Self::VarDecl(vardecl) => vardecl.span(),
            Self::FnDecl(fndecl) => fndecl.span(),
        }
    }
}

impl Parse for AstStmt {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let stmt = if input.peek::<Ident>() && input.peek2::<Token![=]>() {
            input.parse().map(AstStmt::VarDecl)?
        } else if input.peek::<Ident>() && input.peek2::<Paren>() {
            // might be a declaration, might just be a call in an expr
            parse_ambiguous_fn(input)?
        } else {
            input.parse().map(AstStmt::Expr)?
        };

        if !input.peek::<End>() {
            return Err(input.error("Trailing token"))
        }

        Ok(stmt)
    }
}

pub enum AstExpr {
    Value(AstValue),
    Binary(AstBinary),
    Unary(AstUnary),
    Group(AstGroup),
    FnCall(AstFnCall),
}

impl std::fmt::Debug for AstExpr {
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

impl From<AstValue> for AstExpr {
    fn from(value: AstValue) -> Self {
        AstExpr::Value(value)
    }
}

impl From<AstBinary> for AstExpr {
    fn from(value: AstBinary) -> Self {
        AstExpr::Binary(value)
    }
}

impl From<AstUnary> for AstExpr {
    fn from(value: AstUnary) -> Self {
        AstExpr::Unary(value)
    }
}

impl From<AstGroup> for AstExpr {
    fn from(value: AstGroup) -> Self {
        AstExpr::Group(value)
    }
}

impl From<AstFnCall> for AstExpr {
    fn from(value: AstFnCall) -> Self {
        AstExpr::FnCall(value)
    }
}

impl Spanned for AstExpr {
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

impl Parse for AstExpr {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let lhs = parsing::parse_expr_lhs(input)?;
        parsing::parse_expr(input, lhs, Precedence::MIN)
    }
}

pub enum AstValue {
    Literal(Literal),
    Ident(Ident),
}

impl std::fmt::Debug for AstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(l) => write!(f, "{}", l.num),
            Self::Ident(i) => write!(f, "{}", i.symbol)
        }
    }
}

impl Spanned for AstValue {
    fn span(&self) -> Span {
        match self {
            Self::Literal(lit) => lit.span(),
            Self::Ident(id) => id.span(),
        }
    }
}

impl Parse for AstValue {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let mut lookahead = input.lookahead();
        if lookahead.peek::<Literal>() {
            input.parse().map(AstValue::Literal)
        } else if lookahead.peek::<Ident>() {
            input.parse().map(AstValue::Ident)
        } else {
            Err(lookahead.error())
        }
    }
}

pub struct AstBinary {
    pub(crate) lhs: Box<AstExpr>,
    pub(crate) op: BinOp,
    pub(crate) rhs: Box<AstExpr>,
}

impl std::fmt::Debug for AstBinary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?} {:?})", self.lhs, self.op, self.rhs)
    }
}

impl Spanned for AstBinary {
    fn span(&self) -> Span {
        let start = self.lhs.span().start;
        let end = self.rhs.span().end;
        Span { start, end }
    }
}

impl Parse for AstBinary {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        Ok(AstBinary {
            lhs: Box::new(input.parse()?),
            op: input.parse()?,
            rhs: Box::new(input.parse()?),
        })
    }
}

#[derive(Clone)]
pub enum BinOp {
    Add(Token![+]),
    Sub(Token![-]),
    Mul(Token![*]),
    Div(Token![/]),
    Exp(Token![^])
}

impl std::fmt::Debug for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(_) => write!(f, "{}", <Token![+]>::display()),
            Self::Sub(_) => write!(f, "{}", <Token![-]>::display()),
            Self::Mul(_) => write!(f, "{}", <Token![*]>::display()),
            Self::Div(_) => write!(f, "{}", <Token![/]>::display()),
            Self::Exp(_) => write!(f, "{}", <Token![^]>::display()),
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
            Self::Exp(t) => t.span(),
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
        } else if input.peek::<Token![^]>() {
            input.parse().map(BinOp::Exp)
        } else {
            // use custom message here instead of listing binary operators
            Err(input.error("Expected binary operator"))
        }
    }
}

pub struct AstUnary {
    pub(crate) op: UnaryOp,
    pub(crate) expr: Box<AstExpr>,
}

impl std::fmt::Debug for AstUnary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?})", self.op, self.expr)
    }
}

impl Spanned for AstUnary {
    fn span(&self) -> Span {
        let start = self.op.span().start;
        let end = self.expr.span().end;
        Span { start, end }
    }
}

impl Parse for AstUnary {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        Ok(AstUnary {
            op: input.parse()?,
            expr: Box::new(parsing::parse_expr_lhs(input)?),
        })
    }
}

#[derive(Clone)]
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
            // use custom message here instead of listing unary operators
            Err(input.error("Expected unary operator"))
        }
    }
}

pub struct AstGroup {
    pub(crate) delim: Delimiter,
    pub(crate) expr: Box<AstExpr>,
}

impl std::fmt::Debug for AstGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?} {:?})", self.delim, self.expr)
    }
}

impl Spanned for AstGroup {
    fn span(&self) -> Span {
        self.expr.span()
    }
}

impl Parse for AstGroup {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        if let LexToken::Group(g, _) = input.next_token() {
            let group = AstGroup {
                delim: g.delim,
                expr: Box::new(input.parse()?)
            };
            // already checked by lexer
            assert!(matches!(input.next_token(), LexToken::End(..))); // eat End
            Ok(group)
        } else {
            Err(input.error("Expected group"))
        }
    }
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence { Assign, Sum, Product, Unary, Exponent, Unambiguous }

impl Precedence {
    pub(crate) const MIN: Self = Precedence::Assign;

    pub fn of_binop(op: &BinOp) -> Self {
        match op {
            BinOp::Add(_) | BinOp::Sub(_) => Precedence::Sum,
            BinOp::Mul(_) | BinOp::Div(_) => Precedence::Product,
            BinOp::Exp(_) => Precedence::Exponent
        }
    }

    pub fn of(e: &AstExpr) -> Self {
        match e {
            AstExpr::Value(_) => Precedence::Unambiguous,
            AstExpr::Binary(b) => Precedence::of_binop(&b.op),
            AstExpr::Unary(_) => Precedence::Unary,
            AstExpr::Group(_) => Precedence::Unambiguous,
            AstExpr::FnCall(_) => Precedence::Unambiguous,
        }
    }
}

pub struct AstFnCall {
    pub fn_name: Ident,
    pub parens: Paren,
    pub inputs: Punctuated<AstExpr, Token![,]>
}

impl std::fmt::Debug for AstFnCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}({:?})", self.fn_name, self.inputs)
    }
}

impl Spanned for AstFnCall {
    fn span(&self) -> Span {
        let start = self.fn_name.span().start;
        let end = self.parens.span().end;
        Span { start, end }
    }
}

impl Parse for AstFnCall {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let name = input.parse()?;
        let parens = input.parse()?;
        Ok(AstFnCall {
            fn_name: name,
            parens,
            inputs: parse_punctuated_group(input)?,
        })
    }
}

#[derive(Debug)]
pub struct VarDecl {
    pub var_name: Ident,
    pub equals: Token![=],
    pub expr: AstExpr,
}

impl Spanned for VarDecl {
    fn span(&self) -> Span {
        Span {
            start: self.var_name.span().start,
            end: self.expr.span().end
        }
    }
}

impl Parse for VarDecl {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        Ok(VarDecl {
            var_name: input.parse()?,
            equals: input.parse()?,
            expr: input.parse()?,
        })
    }
}

#[derive(Debug)]
pub struct FnSig {
    pub fn_name: Ident,
    pub parens: Paren,
    pub inputs: Punctuated<Ident, Token![,]>
}

impl Spanned for FnSig {
    fn span(&self) -> Span {
        let end = self.inputs.try_span()
            .or_else(|| Some(self.parens.span()))
            .unwrap()
            .end;

        Span {
            start: self.fn_name.span().start,
            end
        }
    }
}

impl Parse for FnSig {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let fn_name = input.parse()?;
        let parens = input.parse()?;
        Ok(FnSig {
            fn_name,
            parens,
            inputs: parse_punctuated_group(input)?,
        })
    }
}

pub fn parse_punctuated_group<T: Parse, S: Parse>(input: ParseStream)
-> Result<Punctuated<T, S>, ParseError> {
    let mut punctuated = Punctuated::new();
    loop {
        if input.peek::<End>() {
            input.next_token();
            break
        }
        let ident = input.parse()?;
        punctuated.push_value(ident);

        if input.peek::<End>() {
            input.next_token();
            break
        }
        let comma = input.parse()?;
        punctuated.push_separator(comma);
    }
    Ok(punctuated)
}

#[derive(Debug)]
pub struct FnDecl {
    pub sig: FnSig,
    pub equals: Token![=],
    pub body: AstExpr,
}

impl Spanned for FnDecl {
    fn span(&self) -> Span {
        Span {
            start: self.sig.span().start,
            end: self.body.span().end
        }
    }
}

impl Parse for FnDecl {
    fn parse(input: ParseStream) -> Result<Self, ParseError> {
        let sig = input.parse()?;
        let equals: Token![=] = input.parse()?;
        let body = input.parse()?;
        Ok(FnDecl {
            sig,
            equals,
            body,
        })
    }
}

fn parse_ambiguous_fn(input: ParseStream) -> Result<AstStmt, ParseError> {
    let begin = input.save_pos();

    let name = input.parse()?;

    if input.peek_ignore_group::<Token![=]>() {
        let parens = input.parse()?;

        let inputs = parse_punctuated_group(input)?;
        let sig = FnSig {
            fn_name: name,
            parens,
            inputs
        };
        Ok(FnDecl{
            sig,
            equals: input.parse()?,
            body: input.parse()?,
        }.into())
    } else {
        input.restore_pos(begin);
        input.parse().map(AstStmt::Expr)
    }
}

pub trait AstVisit {
    type Result;

    fn visit_expr(&mut self, expr: &AstExpr) -> Self::Result;
    fn visit_value(&mut self, value: &AstValue) -> Self::Result;
    fn visit_binary(&mut self, binary: &AstBinary) -> Self::Result;
    fn visit_unary(&mut self, unary: &AstUnary) -> Self::Result;
    fn visit_fn_call(&mut self, fn_call: &AstFnCall) -> Self::Result;
    fn visit_group(&mut self, group: &AstGroup) -> Self::Result;
}

mod parsing {
    use crate::parsing::token::Paren;

    use super::*;

    pub fn parse_expr_lhs(input: ParseStream) -> Result<AstExpr, ParseError> {
        if input.peek::<Token![-]>() {
            input.parse().map(AstExpr::Unary)
        } else if input.peek::<Ident>() && input.peek2::<Paren>() {
            input.parse().map(AstExpr::FnCall)
        } else if input.peek::<Literal>() || input.peek::<Ident>() {
            input.parse().map(AstExpr::Value)
        } else if input.peek::<Paren>() {
            input.parse().map(AstExpr::Group)
        } else {
            Err(input.error("Expected ident, literal, parens, unary operator"))
        }
    }

    pub fn parse_expr(
        input: ParseStream,
        mut left: AstExpr,
        base: Precedence
    ) -> Result<AstExpr, ParseError> {
        loop {
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
                left = AstBinary {
                    lhs: Box::new(left),
                    op,
                    rhs: parse_binop_rhs(input, precedence)?,
                }.into();
            }
        }
        Ok(left)
    }

    fn peek_binop(input: ParseStream) -> bool {
        input.peek::<Token![+]>() ||
            input.peek::<Token![-]>() ||
            input.peek::<Token![*]>() ||
            input.peek::<Token![/]>() ||
            input.peek::<Token![^]>()
    }

    fn parse_binop_rhs(
        input: ParseStream,
        precedence: Precedence,
    ) -> Result<Box<AstExpr>, ParseError> {
        let mut rhs = parse_expr_lhs(input)?;
        loop {
            let begin = input.save_pos();
            let next = peek_precedence(input);

            // if right assoc: next can be of equal precedence
            if (peek_is_left_assoc(input) && next > precedence) ||
                (!peek_is_left_assoc(input) && next >= precedence)
            {
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

    fn peek_is_left_assoc(input: ParseStream) -> bool {
        let begin = input.save_pos();

        let assoc = if let Ok(op) = input.parse() {
            !matches!(op, BinOp::Exp(_))
        } else {
            true
        };

        input.restore_pos(begin);
        assoc
    }
}

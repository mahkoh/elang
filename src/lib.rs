#![allow(clippy::len_zero)]
#![allow(clippy::new_without_default)]

pub use crate::{
    diag::Diagnostic,
    types::{
        diagnostic::{Error, ErrorContext, ErrorType, TokenAlternative},
        result::Result,
        span::{Span, Spanned},
        store::StrId,
        token::TokenType,
        tree::{Expr, ExprId, ExprKind, ExprType, Fields, FnParam, FnType, BuiltInFn},
        value::Value,
    },
};
use crate::{lexer::Lexer, parser::Parser, types::store::Store};
use num_rational::BigRational;
use std::{convert::TryInto, rc::Rc};

mod diag;
mod eval;
mod funcs;
mod lexer;
mod parser;
mod types;
mod util;

pub struct Elang {
    store: Store,
    force_trace: Vec<ExprId>,
}

/// Core methods
impl Elang {
    /// Creates a new instance
    pub fn new() -> Self {
        Self {
            store: Store::new(),
            force_trace: vec![],
        }
    }

    /// Parses source code and returns the parsed expression
    ///
    /// `lo` is the base for all generated spans.
    ///
    /// Returns an error if `src` does not end
    /// in a `\n`. Returns an error if `lo + src.len() > u32::max_value() - 1`.
    pub fn parse(&mut self, lo: u32, src: &[u8]) -> Result<ExprId> {
        if src.last().copied() != Some(b'\n') {
            return self.err(ErrorType::MissingNewline);
        }
        let overflow = src
            .len()
            .try_into()
            .ok()
            .and_then(|len| lo.checked_add(len))
            .and_then(|hi| {
                if hi > u32::max_value() - 1 {
                    None
                } else {
                    Some(hi)
                }
            })
            .is_none();
        if overflow {
            return self.err(ErrorType::SpanOverflow);
        }
        let lexer = Lexer::new(lo, src, &mut self.store);
        let mut parser = Parser::new(lexer);
        parser.parse()
    }

    fn err<T>(&self, details: ErrorType) -> Result<T> {
        Err(Error {
            span: Span::built_in(),
            error: details,
            context: vec![],
        })
    }

    /// Evaluates the expression
    ///
    /// After this function returns successfully, `expr_id` has been modified such that
    /// its value is one of the following:
    ///
    /// * `Value::Number`
    /// * `Value::Ident`
    /// * `Value::Bool`
    /// * `Value::Null`
    /// * `Value::List`
    /// * `Value::Fn`
    /// * `Value::String`
    /// * `Value::Set` (non recursive)
    /// * `Value::Inherit`
    /// * `Value::Resolved` (where the value of the destination is one of these)
    ///
    /// After this function returns with an error, some subtrees of `expr_id` might have
    /// been modified as above.
    ///
    /// If any expression `e` that is part of the tree of `expr_id`
    ///
    /// * is evaluated by this function and
    /// * `e` is already borrowed when this function is called,
    ///
    /// this will be interpreted as infinite recursion and an error will be returned.
    pub fn eval(&mut self, expr_id: ExprId) -> Result {
        self.force(expr_id)
    }

    pub fn add_expr(&mut self, span: Span, value: ExprType) -> ExprId {
        self.store.add_expr(span, value)
    }

    pub fn get_expr(&self, expr_id: ExprId) -> Rc<Expr> {
        self.store.get_expr(expr_id)
    }

    pub fn intern(&mut self, val: Rc<[u8]>) -> StrId {
        self.store.add_str(val)
    }

    pub fn get_interned(&self, i: StrId) -> Rc<[u8]> {
        self.store.get_str(i)
    }
}

/// Utility methods
impl Elang {
    pub fn resolve(&mut self, expr_id: ExprId) -> Result<Rc<Expr>> {
        self.resolve_(expr_id)
    }

    pub fn span(&self, expr_id: ExprId) -> Span {
        self.span_(expr_id)
    }

    pub fn get_string(&mut self, expr_id: ExprId) -> Result<StrId> {
        self.get_string_(expr_id)
    }

    pub fn get_bool(&mut self, expr_id: ExprId) -> Result<bool> {
        self.get_bool_(expr_id)
    }

    pub fn get_int(&mut self, expr_id: ExprId) -> Result<Rc<BigRational>> {
        self.get_int_(expr_id)
    }

    pub fn get_list(&mut self, expr_id: ExprId) -> Result<Rc<[ExprId]>> {
        self.get_list_(expr_id)
    }

    pub fn get_field(&mut self, expr_id: ExprId, selector: ExprId) -> Result<ExprId> {
        self.get_field_int(expr_id, selector)
    }

    pub fn get_opt_field(
        &mut self,
        expr_id: ExprId,
        selector: ExprId,
    ) -> Result<Option<ExprId>> {
        self.get_opt_field_(expr_id, selector)
    }

    pub fn get_fields(&mut self, expr_id: ExprId) -> Result<Fields> {
        self.get_fields_(expr_id)
    }

    pub fn get_value(&mut self, expr_id: ExprId) -> Result<Value> {
        self.get_value_(expr_id)
    }
}

#![allow(clippy::len_zero)]
#![allow(clippy::new_without_default)]

use crate::types::store::Store;
pub use crate::{
    diag::Diagnostic,
    types::{
        error::{Error, ErrorContext, ErrorType, TokenAlternative},
        result::Result,
        span::{Span, Spanned},
        store::{Intern, StrId},
        token::TokenKind,
        tree::{Expr, ExprId, ExprKind, ExprType, FnParam, FnType, NativeFn},
        value::Value,
    },
};
use num_rational::BigRational;
use std::{borrow::Cow, collections::HashMap, convert::TryInto, rc::Rc};

mod diag;
mod eval;
mod funcs;
mod lexer;
mod parser;
mod types;
mod util;

/// The elang engine
pub struct Elang {
    store: Store,
    force_trace: Vec<ExprId>,
    std: Option<ExprId>,
}

/// Core methods
impl Elang {
    /// Creates a new instance
    pub fn new() -> Self {
        Self {
            store: Store::new(),
            force_trace: vec![],
            std: None,
        }
    }

    /// Parses source code and returns the parsed expression
    ///
    /// `lo` is the base for all generated spans.
    ///
    /// If `src` does not end in a `\n`, a `\n` is appended. This might influence the
    /// generated spans. The default diagnostic util behaves the same way and can
    /// therefore handle such spans.
    ///
    /// Returns an error if `lo + src.len() > u32::max_value() - 1`.
    pub fn parse(&mut self, lo: u32, src: &[u8]) -> Result<ExprId> {
        let mut src = Cow::Borrowed(src);
        if src.last().copied() != Some(b'\n') {
            let mut copy = src.to_vec();
            copy.push(b'\n');
            src = Cow::Owned(copy);
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
            return Err(Error {
                span: Span::built_in(),
                error: ErrorType::SpanOverflow,
                context: vec![],
            });
        }
        let tokens = lexer::lex(lo, &src, &mut self.store)?;
        parser::parse(&mut self.store, tokens)
    }

    /// Evaluates the expression
    ///
    /// After this function returns successfully, the expression has been modified such that
    /// its type is one of the following:
    ///
    /// * `Number`
    /// * `Bool`
    /// * `Null`
    /// * `List`
    /// * `Fn`
    /// * `String`
    /// * `Map` (non recursive)
    /// * `Resolved` (where the value of the destination is one of these)
    ///
    /// After this function returns with an error, some subtrees of the expression might have
    /// been modified as above.
    ///
    /// If any expression `e` that is part of the tree of the expression
    ///
    /// * is evaluated by this function and
    /// * `e` is already borrowed when this function is called,
    ///
    /// this will be interpreted as infinite recursion and an error will be returned.
    pub fn eval(&mut self, expr_id: ExprId) -> Result {
        self.force(expr_id)
    }

    /// Adds an expression to the engine
    pub fn add_expr(&mut self, span: Span, value: ExprType) -> ExprId {
        self.store.add_expr(span, value)
    }

    /// Retrieves an expression
    pub fn get_expr(&self, expr_id: ExprId) -> Rc<Expr> {
        self.store.get_expr(expr_id)
    }

    /// Adds a string to the engine
    pub fn intern<T: Intern>(&mut self, val: T) -> StrId {
        self.store.add_str(val)
    }

    /// Retrieves an interned string
    pub fn get_interned(&self, i: StrId) -> Rc<[u8]> {
        self.store.get_str(i)
    }
}

/// Utility methods
impl Elang {
    /// Evaluates an expression and walks the `Resolved` chain
    ///
    /// The type of the returned expression is one of the types produced by `eval` but not
    /// `Resolved`.
    pub fn resolve(&mut self, expr_id: ExprId) -> Result<Rc<Expr>> {
        self.resolve_(expr_id)
    }

    /// Returns the span of an expression
    pub fn span(&self, expr_id: ExprId) -> Span {
        self.span_(expr_id)
    }

    /// Evaluates an expression, asserts that it is a boolean, and returns it
    pub fn get_bool(&mut self, expr_id: ExprId) -> Result<bool> {
        self.get_bool_(expr_id)
    }

    /// Evaluates an expression, asserts that it is a function, and returns it
    pub fn get_fn(&mut self, expr_id: ExprId) -> Result<FnType> {
        self.get_fn_(expr_id)
    }

    /// Evaluates an expression, asserts that it is a list, and returns it
    pub fn get_list(&mut self, expr_id: ExprId) -> Result<Rc<[ExprId]>> {
        self.get_list_(expr_id)
    }

    /// Evaluates an expression, asserts that it is a map, and returns it
    pub fn get_map(
        &mut self,
        expr_id: ExprId,
    ) -> Result<Rc<HashMap<Spanned<StrId>, ExprId>>> {
        self.get_fields_(expr_id)
    }

    /// Evaluates an expression and asserts that it is null
    pub fn get_null(&mut self, expr_id: ExprId) -> Result {
        self.get_null_(expr_id)
    }

    /// Evaluates an expression, asserts that it is a number, and returns it
    pub fn get_number(&mut self, expr_id: ExprId) -> Result<Rc<BigRational>> {
        self.get_number_(expr_id)
    }

    /// Evaluates an expression, asserts that it is a string, and returns it
    pub fn get_string(&mut self, expr_id: ExprId) -> Result<StrId> {
        self.get_string_(expr_id)
    }

    /// Retrieves a field from a map or a list
    ///
    /// `selector` should evaluate to a string or a number depending on the type of `expr_id`.
    ///
    /// If the field is not present in `expr_id`, an error is returned.
    pub fn get_field(&mut self, expr_id: ExprId, selector: ExprId) -> Result<ExprId> {
        self.get_field_int(expr_id, selector)
    }

    /// Retrieves an optional field from a map or a list
    ///
    /// `selector` should evaluate to a string or a number depending on the type of `expr_id`.
    ///
    /// If the field is not present in `expr_id`, `None` is returned.
    pub fn get_opt_field(
        &mut self,
        expr_id: ExprId,
        selector: ExprId,
    ) -> Result<Option<ExprId>> {
        self.get_opt_field_(expr_id, selector)
    }

    /// Retrieves an expression as a value
    ///
    /// This function recursively resolves the expression, asserts that it doesn't resolve to a
    /// function, and converts it into a `Value`.
    pub fn get_value(&mut self, expr_id: ExprId) -> Result<Value> {
        self.get_value_(expr_id)
    }

    /// Adds a value as an expression
    pub fn add_value(&mut self, value: Value) -> ExprId {
        self.add_value_(value)
    }

    /// Creates a new error originating from an expression
    ///
    /// This function traverses `expr_id` as long as it is `Resolved`, adding each step as context
    /// to the error. The context is sorted inside-out. The span of the error is the span of the
    /// first non-`Resolved` expression.
    pub fn error(&self, expr_id: ExprId, error: ErrorType) -> Error {
        self.perror(expr_id, error)
    }
}

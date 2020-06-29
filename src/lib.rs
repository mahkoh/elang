#![allow(clippy::len_zero)]
#![allow(clippy::new_without_default)]

use crate::types::store::Store;
pub use crate::{
    diag::Diagnostic,
    types::{
        diagnostic::{Error, ErrorContext, ErrorType, TokenAlternative},
        result::Result,
        span::{Span, Spanned},
        store::{StrId, Intern},
        token::TokenKind,
        tree::{BuiltInFn, Expr, ExprId, ExprKind, ExprType, FnParam, FnType},
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
            return self.err(ErrorType::SpanOverflow);
        }
        let tokens = lexer::lex(lo, &src, &mut self.store)?;
        parser::parse(&mut self.store, tokens)
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

    pub fn intern<T: Intern>(&mut self, val: T) -> StrId {
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

    pub fn get_number(&mut self, expr_id: ExprId) -> Result<Rc<BigRational>> {
        self.get_number_(expr_id)
    }

    pub fn get_null(&mut self, expr_id: ExprId) -> Result {
        let res = self.resolve_(expr_id)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Null => Ok(()),
            _ => self.error2(
                expr_id,
                ErrorType::UnexpectedExprKind(&[ExprKind::Null], val.kind()),
            ),
        }
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

    pub fn get_fields(
        &mut self,
        expr_id: ExprId,
    ) -> Result<Rc<HashMap<Spanned<StrId>, ExprId>>> {
        self.get_fields_(expr_id)
    }

    pub fn get_value(&mut self, expr_id: ExprId) -> Result<Value> {
        self.get_value_(expr_id)
    }
}

#![allow(clippy::len_zero)]
#![allow(clippy::new_without_default)]

pub use crate::{
    lexer::Lexer,
    parser::Parser,
    types::{
        diagnostic::{Error, MsgDetails, MsgLevel},
        result::Result,
        span::Span,
        store::{Store, StrId},
        tree::{Expr, ExprId, Fields, Selector, Value},
    },
};
use std::{convert::TryInto, rc::Rc};

mod eval;
mod funcs;
mod lexer;
mod parser;
mod types;
pub mod util;

pub struct Elang {
    store: Store,
}

/// Core methods
impl Elang {
    /// Creates a new instance
    pub fn new() -> Self {
        Self {
            store: Store::new(),
        }
    }

    /// Parses source code and returns the parsed expression
    ///
    /// `lo` is the base for all generated spans.
    ///
    /// Returns an error if `src` does not end
    /// in a `\n`. Returns an error if `lo + src.len() > u32::max_value() - 1`.
    pub fn parse(&self, lo: u32, src: &[u8]) -> Result<ExprId> {
        if src.last().copied() != Some(b'\n') {
            return self.err(MsgDetails::MissingNewline);
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
            return self.err(MsgDetails::SpanOverflow);
        }
        let lexer = Lexer::new(lo, src, self.store.clone());
        let mut parser = Parser::new(lexer, self.store.clone());
        parser.parse()
    }

    fn err<T>(&self, details: MsgDetails) -> Result<T> {
        return Err(Error {
            span: Span::built_in(),
            level: MsgLevel::Error,
            details,
            children: vec![],
        });
    }

    fn eval_(&self) -> eval::Eval {
        eval::Eval::new(self.store.clone())
    }

    /// Evaluates the expression
    ///
    /// After this function returns successfully, `expr_id` has been modified such that
    /// its value is one of the following:
    ///
    /// * `Value::Integer`
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
    pub fn eval(&self, expr_id: ExprId) -> Result {
        self.eval_().force(expr_id)
    }

    pub fn add_expr(&self, span: Span, value: Value) -> ExprId {
        self.store.add_expr(span, value)
    }

    pub fn get_expr(&self, expr_id: ExprId) -> Rc<Expr> {
        self.store.get_expr(expr_id)
    }

    pub fn intern(&self, val: Rc<[u8]>) -> StrId {
        self.store.add_str(val)
    }

    pub fn get_interned(&self, i: StrId) -> Rc<[u8]> {
        self.store.get_str(i)
    }
}

/// Utility methods
impl Elang {
    pub fn resolve(&self, expr_id: ExprId) -> Result<Rc<Expr>> {
        self.eval_().resolve(expr_id)
    }

    pub fn span(&self, expr_id: ExprId) -> Span {
        self.eval_().span(expr_id)
    }

    pub fn get_string(&self, expr_id: ExprId) -> Result<StrId> {
        self.eval_().get_string(expr_id)
    }

    pub fn get_bool(&self, expr_id: ExprId) -> Result<bool> {
        self.eval_().get_bool(expr_id)
    }

    pub fn get_int(&self, expr_id: ExprId) -> Result<i64> {
        self.eval_().get_int(expr_id)
    }

    pub fn get_list(&self, expr_id: ExprId) -> Result<Rc<[ExprId]>> {
        self.eval_().get_list(expr_id)
    }

    pub fn get_field(&self, expr_id: ExprId, selector: Selector) -> Result<ExprId> {
        self.eval_().get_field(expr_id, &selector, None)
    }

    pub fn get_opt_field(
        &self,
        expr_id: ExprId,
        selector: Selector,
    ) -> Result<Option<ExprId>> {
        self.eval_().get_opt_field(expr_id, &selector, None)
    }

    pub fn get_fields(&self, expr_id: ExprId) -> Result<Fields> {
        self.eval_().get_fields(expr_id)
    }
}

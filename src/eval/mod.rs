use crate::{
    types::{
        result::Result,
        span::{Span, Spanned},
        store::Store,
        tree::{Expr, ExprId, FnArg, FnType, Selector, Value},
    },
    Error,
};
use std::{collections::HashMap, rc::Rc};
use crate::types::diagnostic::{ErrorType, ErrorContext};

mod force;
mod get;

pub struct Eval {
    store: Store,
    force_trace: Vec<ExprId>,
}

impl Eval {
    pub fn new(store: Store) -> Self {
        Eval {
            store,
            force_trace: vec![],
        }
    }

    fn equal_to(&mut self, left: ExprId, right: ExprId) -> Result<bool> {
        let l = self.resolve(left)?;
        let r = self.resolve(right)?;
        let l = l.val.borrow();
        let r = r.val.borrow();
        match (&*l, &*r) {
            (&Value::Integer(l), &Value::Integer(r)) => Ok(l == r),
            (&Value::String(l), &Value::String(r)) => Ok(l == r),
            (&Value::Null, &Value::Null) => Ok(true),
            _ => Ok(false),
        }
    }

    /// Forces the expression and returns the forced datatype expression.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// After forcing, this expression can be a reference to a datatype expression. This
    /// function recursively resolves these references and returns the datatype
    /// expression.
    pub fn resolve(&mut self, mut expr: ExprId) -> Result<Rc<Expr>> {
        self.force(expr)?;

        let mut e;
        loop {
            e = self.store.get_expr(expr);
            expr = match *e.val.borrow() {
                Value::Resolved(_, e) => e,
                _ => break,
            };
        }
        Ok(e)
    }

    pub fn span(&self, expr: ExprId) -> Span {
        self.store.get_expr(expr).span
    }

    pub fn deep_copy(&mut self, target: ExprId) -> ExprId {
        macro_rules! copy {
            ($e:expr) => {
                self.deep_copy($e)
            };
        }

        let val = self.store.get_expr(target);
        let target_span = val.span;
        let val = val.val.borrow();

        macro_rules! new {
            ($e:expr) => {{
                let e = $e;
                self.store.add_expr(target_span, e)
            }};
        }

        // Unary operators
        {
            macro_rules! un {
                ($f:expr, $a:expr) => {
                    Some(($f, $a))
                };
            }

            let un: Option<(fn(_) -> _, _)> = match *val {
                Value::Not(e) => un!(Value::Not, e),
                Value::Neg(e) => un!(Value::Neg, e),
                Value::Stringify(e) => un!(Value::Stringify, e),
                _ => None,
            };

            if let Some((f, n)) = un {
                return new!(f(copy!(n)));
            }
        }

        // Binary operators
        {
            macro_rules! bin {
                ($f:expr, $a:expr, $b:expr) => {
                    Some(($f, $a, $b))
                };
            }

            let bin: Option<(fn(_, _) -> _, _, _)> = match *val {
                Value::And(l, r) => bin!(Value::And, l, r),
                Value::Or(l, r) => bin!(Value::Or, l, r),
                Value::Add(l, r) => bin!(Value::Add, l, r),
                Value::Sub(l, r) => bin!(Value::Sub, l, r),
                Value::Mul(l, r) => bin!(Value::Mul, l, r),
                Value::Div(l, r) => bin!(Value::Div, l, r),
                Value::Mod(l, r) => bin!(Value::Mod, l, r),
                Value::Gt(l, r) => bin!(Value::Gt, l, r),
                Value::Lt(l, r) => bin!(Value::Lt, l, r),
                Value::Ge(l, r) => bin!(Value::Ge, l, r),
                Value::Le(l, r) => bin!(Value::Le, l, r),
                Value::Eq(l, r) => bin!(Value::Eq, l, r),
                Value::Ne(l, r) => bin!(Value::Ne, l, r),
                Value::Impl(l, r) => bin!(Value::Impl, l, r),
                Value::Overlay(l, r) => bin!(Value::Overlay, l, r),
                Value::Concat(l, r) => bin!(Value::Concat, l, r),
                Value::Apl(l, r) => bin!(Value::Apl, l, r),
                Value::Test(l, r) => bin!(Value::Test, l, r),
                _ => None,
            };

            if let Some((f, l, r)) = bin {
                return new!(f(copy!(l), copy!(r)));
            }
        }

        // Ternary operators
        {
            macro_rules! tern {
                ($f:expr, $a:expr, $b:expr, $c:expr) => {
                    Some(($f, $a, $b, $c))
                };
            }

            let tern: Option<(fn(_, _, _) -> _, _, _, _)> = match *val {
                Value::Cond(a, b, c) => tern!(Value::Cond, a, b, c),
                _ => None,
            };

            if let Some((f, a, b, c)) = tern {
                return new!(f(copy!(a), copy!(b), copy!(c)));
            }
        }

        // Everything else
        let rv = match *val {
            Value::Not(..)
            | Value::Neg(..)
            | Value::Stringify(..)
            | Value::And(..)
            | Value::Or(..)
            | Value::Add(..)
            | Value::Sub(..)
            | Value::Mul(..)
            | Value::Div(..)
            | Value::Mod(..)
            | Value::Gt(..)
            | Value::Lt(..)
            | Value::Ge(..)
            | Value::Le(..)
            | Value::Eq(..)
            | Value::Ne(..)
            | Value::Impl(..)
            | Value::Overlay(..)
            | Value::Concat(..)
            | Value::Apl(..)
            | Value::Test(..)
            | Value::Cond(..) => {
                // handled above
                unreachable!();
            }
            Value::Inherit => Value::Inherit,
            Value::String(s) => Value::String(s),
            Value::Integer(i) => Value::Integer(i),
            Value::Bool(b) => Value::Bool(b),
            Value::Null => Value::Null,
            Value::Resolved(id, dst) => Value::Resolved(id, dst),
            Value::Fn(FnType::BuiltIn(ref f)) => Value::Fn(FnType::BuiltIn(f.clone())),
            Value::Ident(id) => Value::Ident(id),
            Value::List(ref els) => {
                let mut nels = Vec::with_capacity(els.len());
                for &el in els.iter() {
                    nels.push(copy!(el));
                }
                Value::List(Rc::from(nels.into_boxed_slice()))
            }
            Value::Set(ref fields, rec) => {
                let mut nfields = HashMap::with_capacity(fields.len());
                for (&id, &(span, val)) in fields.iter() {
                    nfields.insert(id, (span, copy!(val)));
                }
                Value::Set(Rc::new(nfields), rec)
            }
            Value::Let(ref fields, body) => {
                let mut nfields = HashMap::with_capacity(fields.len());
                for (&id, &(span, val)) in fields.iter() {
                    nfields.insert(id, (span, copy!(val)));
                }
                let nbody = copy!(body);
                Value::Let(Rc::new(nfields), nbody)
            }
            Value::Path(ref segs) => {
                let mut nsegs = Vec::with_capacity(segs.len());
                for &seg in segs.iter() {
                    nsegs.push(copy!(seg));
                }
                Value::Path(Rc::from(nsegs.into_boxed_slice()))
            }
            Value::Selector(ref s) => {
                if let Selector::Expr(e) = *s {
                    Value::Selector(Selector::Expr(copy!(e)))
                } else {
                    Value::Selector(s.clone())
                }
            }
            Value::Select(target, segs, alt) => {
                let ntarget = copy!(target);
                let nsegs = copy!(segs);
                let nalt = match alt {
                    Some(alt) => Some(copy!(alt)),
                    _ => None,
                };
                Value::Select(ntarget, nsegs, nalt)
            }
            Value::Fn(FnType::Normal(
                Spanned {
                    span,
                    val: FnArg::Ident(id),
                },
                body,
            )) => {
                let nbody = copy!(body);
                Value::Fn(FnType::Normal(Spanned::new(span, FnArg::Ident(id)), nbody))
            }
            Value::Fn(FnType::Normal(
                Spanned {
                    span,
                    val: FnArg::Pat(id, ref fields, wild),
                },
                body,
            )) => {
                let mut nfields = HashMap::with_capacity(fields.len());
                for (&id, &(span, alt)) in fields.iter() {
                    nfields.insert(
                        id,
                        match alt {
                            Some(alt) => (span, Some(copy!(alt))),
                            _ => (span, None),
                        },
                    );
                }
                let nbody = copy!(body);
                let pat = FnArg::Pat(id, Rc::new(nfields), wild);
                Value::Fn(FnType::Normal(Spanned::new(span, pat), nbody))
            }
        };

        new!(rv)
    }

    fn error<T>(&mut self, eid: ExprId, error: ErrorType) -> Result<T> {
        Err(self.error_(eid, error))
    }

    fn error_(&mut self, mut eid: ExprId, error: ErrorType) -> Error {
        let mut ctx = vec!();
        let mut span;
        loop {
            let expr = self.store.get_expr(eid);
            span = expr.span;
            let expr = expr.val.borrow();
            match *expr {
                Value::Resolved(_, new) => {
                    ctx.push(ErrorContext::EvalResolved(eid));
                    eid = new;
                },
                _ => break,
            }
        }
        ctx.reverse();
        Error {
            span,
            error,
            context: ctx,
        }
    }
}

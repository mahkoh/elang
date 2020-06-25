use crate::{
    types::{
        diagnostic::{ErrorContext, ErrorType},
        result::Result,
        span::{Span, Spanned},
        tree::{Expr, ExprId, ExprType, FnArg, FnType},
        value::Value,
    },
    Elang, Error, ExprKind,
};
use std::{collections::HashMap, rc::Rc};

mod force;
mod get;

impl Elang {
    pub(crate) fn equal_to(&mut self, left: ExprId, right: ExprId) -> Result<bool> {
        let l = self.resolve_(left)?;
        let r = self.resolve_(right)?;
        let l = l.val.borrow();
        let r = r.val.borrow();
        match (&*l, &*r) {
            (&ExprType::Number(ref l), &ExprType::Number(ref r)) => Ok(l == r),
            (&ExprType::String(l), &ExprType::String(r)) => Ok(l == r),
            (&ExprType::Null, &ExprType::Null) => Ok(true),
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
    pub(crate) fn resolve_(&mut self, mut expr: ExprId) -> Result<Rc<Expr>> {
        self.force(expr)?;

        let mut e;
        loop {
            e = self.store.get_expr(expr);
            expr = match *e.val.borrow() {
                ExprType::Resolved(_, e) => e,
                _ => break,
            };
        }
        Ok(e)
    }

    pub(crate) fn span_(&self, expr: ExprId) -> Span {
        self.store.get_expr(expr).span
    }

    pub(crate) fn deep_copy(&mut self, target: ExprId) -> ExprId {
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
                ExprType::Not(e) => un!(ExprType::Not, e),
                ExprType::Neg(e) => un!(ExprType::Neg, e),
                ExprType::Stringify(e) => un!(ExprType::Stringify, e),
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
                ExprType::And(l, r) => bin!(ExprType::And, l, r),
                ExprType::Or(l, r) => bin!(ExprType::Or, l, r),
                ExprType::Add(l, r) => bin!(ExprType::Add, l, r),
                ExprType::Sub(l, r) => bin!(ExprType::Sub, l, r),
                ExprType::Mul(l, r) => bin!(ExprType::Mul, l, r),
                ExprType::Div(l, r) => bin!(ExprType::Div, l, r),
                ExprType::Mod(l, r) => bin!(ExprType::Mod, l, r),
                ExprType::Gt(l, r) => bin!(ExprType::Gt, l, r),
                ExprType::Lt(l, r) => bin!(ExprType::Lt, l, r),
                ExprType::Ge(l, r) => bin!(ExprType::Ge, l, r),
                ExprType::Le(l, r) => bin!(ExprType::Le, l, r),
                ExprType::Eq(l, r) => bin!(ExprType::Eq, l, r),
                ExprType::Ne(l, r) => bin!(ExprType::Ne, l, r),
                ExprType::Impl(l, r) => bin!(ExprType::Impl, l, r),
                ExprType::Overlay(l, r) => bin!(ExprType::Overlay, l, r),
                ExprType::Concat(l, r) => bin!(ExprType::Concat, l, r),
                ExprType::Apl(l, r) => bin!(ExprType::Apl, l, r),
                ExprType::Test(l, r) => bin!(ExprType::Test, l, r),
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
                ExprType::Cond(a, b, c) => tern!(ExprType::Cond, a, b, c),
                _ => None,
            };

            if let Some((f, a, b, c)) = tern {
                return new!(f(copy!(a), copy!(b), copy!(c)));
            }
        }

        // Everything else
        let rv = match *val {
            ExprType::Not(..)
            | ExprType::Neg(..)
            | ExprType::Stringify(..)
            | ExprType::And(..)
            | ExprType::Or(..)
            | ExprType::Add(..)
            | ExprType::Sub(..)
            | ExprType::Mul(..)
            | ExprType::Div(..)
            | ExprType::Mod(..)
            | ExprType::Gt(..)
            | ExprType::Lt(..)
            | ExprType::Ge(..)
            | ExprType::Le(..)
            | ExprType::Eq(..)
            | ExprType::Ne(..)
            | ExprType::Impl(..)
            | ExprType::Overlay(..)
            | ExprType::Concat(..)
            | ExprType::Apl(..)
            | ExprType::Test(..)
            | ExprType::Cond(..) => {
                // handled above
                unreachable!();
            }
            ExprType::Inherit => ExprType::Inherit,
            ExprType::String(s) => ExprType::String(s),
            ExprType::Number(ref i) => ExprType::Number(i.clone()),
            ExprType::Bool(b) => ExprType::Bool(b),
            ExprType::Null => ExprType::Null,
            ExprType::Resolved(id, dst) => ExprType::Resolved(id, dst),
            ExprType::Fn(FnType::BuiltIn(ref f)) => {
                ExprType::Fn(FnType::BuiltIn(f.clone()))
            }
            ExprType::Ident(id) => ExprType::Ident(id),
            ExprType::List(ref els) => {
                let mut nels = Vec::with_capacity(els.len());
                for &el in els.iter() {
                    nels.push(copy!(el));
                }
                ExprType::List(Rc::from(nels.into_boxed_slice()))
            }
            ExprType::Set(ref fields, rec) => {
                let mut nfields = HashMap::with_capacity(fields.len());
                for (&id, &(span, val)) in fields.iter() {
                    nfields.insert(id, (span, copy!(val)));
                }
                ExprType::Set(Rc::new(nfields), rec)
            }
            ExprType::Let(ref fields, body) => {
                let mut nfields = HashMap::with_capacity(fields.len());
                for (&id, &(span, val)) in fields.iter() {
                    nfields.insert(id, (span, copy!(val)));
                }
                let nbody = copy!(body);
                ExprType::Let(Rc::new(nfields), nbody)
            }
            ExprType::Path(ref segs) => {
                let mut nsegs = Vec::with_capacity(segs.len());
                for &seg in segs.iter() {
                    nsegs.push(copy!(seg));
                }
                ExprType::Path(Rc::from(nsegs.into_boxed_slice()))
            }
            ExprType::Select(target, segs, alt) => {
                let ntarget = copy!(target);
                let nsegs = copy!(segs);
                let nalt = match alt {
                    Some(alt) => Some(copy!(alt)),
                    _ => None,
                };
                ExprType::Select(ntarget, nsegs, nalt)
            }
            ExprType::Fn(FnType::Normal(
                Spanned {
                    span,
                    val: FnArg::Ident(id),
                },
                body,
            )) => {
                let nbody = copy!(body);
                ExprType::Fn(FnType::Normal(Spanned::new(span, FnArg::Ident(id)), nbody))
            }
            ExprType::Fn(FnType::Normal(
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
                ExprType::Fn(FnType::Normal(Spanned::new(span, pat), nbody))
            }
        };

        new!(rv)
    }

    fn error<T>(&mut self, eid: ExprId, error: ErrorType) -> Result<T> {
        Err(self.error_(eid, error))
    }

    fn error_(&mut self, mut eid: ExprId, error: ErrorType) -> Error {
        let mut ctx = vec![];
        let mut span;
        loop {
            let expr = self.store.get_expr(eid);
            span = expr.span;
            let expr = expr.val.borrow();
            match *expr {
                ExprType::Resolved(_, new) => {
                    ctx.push(ErrorContext::EvalResolved(eid));
                    eid = new;
                }
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

    pub(crate) fn get_value_(&mut self, eid: ExprId) -> Result<Value> {
        let expr = self.resolve_(eid)?;
        let expr = expr.val.borrow();
        let val = match *expr {
            ExprType::Number(ref num) => Value::Number((**num).clone()),
            ExprType::Bool(b) => Value::Bool(b),
            ExprType::Null => Value::Null,
            ExprType::String(id) => Value::String(self.store.get_str(id).to_vec().into_boxed_slice()),
            ExprType::List(ref el) => {
                let el = el.clone();
                drop(expr);
                let mut l = vec![];
                for e in el.iter() {
                    l.push(self.get_value(*e)?);
                }
                Value::List(l.into_boxed_slice())
            }
            ExprType::Set(ref f, _) => {
                let f = f.clone();
                drop(expr);
                let mut r = HashMap::new();
                for e in f.iter() {
                    let s = self.store.get_str(*e.0).to_vec().into_boxed_slice();
                    let val = self.get_value((e.1).1)?;
                    r.insert(s, val);
                }
                Value::Set(r)
            }
            _ => {
                return self.error(
                    eid,
                    ErrorType::UnexpectedExprType(
                        &[
                            ExprKind::Number,
                            ExprKind::Bool,
                            ExprKind::Null,
                            ExprKind::String,
                            ExprKind::List,
                            ExprKind::Set,
                        ],
                        expr.kind(),
                    ),
                );
            }
        };
        Ok(val)
    }
}

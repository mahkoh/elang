use crate::{
    types::{
        diagnostic::{ErrorContext, ErrorType},
        result::Result,
        span::{Span, Spanned},
        tree::{Expr, ExprId, ExprType, FnParam, FnType},
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
            (&ExprType::Number { val: ref lhs }, &ExprType::Number { val: ref rhs }) => {
                Ok(lhs == rhs)
            }
            (&ExprType::String { content: lhs }, &ExprType::String { content: rhs }) => {
                Ok(lhs == rhs)
            }
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
                ExprType::Resolved { dest, .. } => dest,
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
                ExprType::Not { val } => un!(|val| ExprType::Not { val }, val),
                ExprType::Neg { val } => un!(|val| ExprType::Neg { val }, val),
                ExprType::Stringify { val } => {
                    un!(|val| ExprType::Stringify { val }, val)
                }
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
                ExprType::And { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::And { lhs, rhs }, lhs, rhs)
                }
                ExprType::Or { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Or { lhs, rhs }, lhs, rhs)
                }
                ExprType::Add { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Add { lhs, rhs }, lhs, rhs)
                }
                ExprType::Sub { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Sub { lhs, rhs }, lhs, rhs)
                }
                ExprType::Mul { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Mul { lhs, rhs }, lhs, rhs)
                }
                ExprType::Div {
                    numer,
                    denom,
                    int: true,
                } => bin!(
                    |numer, denom| ExprType::Div {
                        numer,
                        denom,
                        int: true
                    },
                    numer,
                    denom
                ),
                ExprType::Div {
                    numer,
                    denom,
                    int: false,
                } => bin!(
                    |numer, denom| ExprType::Div {
                        numer,
                        denom,
                        int: false
                    },
                    numer,
                    denom
                ),
                ExprType::Mod {
                    numer,
                    denom,
                } => bin!(
                    |numer, denom| ExprType::Mod {
                        numer,
                        denom,
                    },
                    numer,
                    denom
                ),
                ExprType::Gt { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Gt { lhs, rhs }, lhs, rhs)
                }
                ExprType::Lt { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Lt { lhs, rhs }, lhs, rhs)
                }
                ExprType::Ge { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Ge { lhs, rhs }, lhs, rhs)
                }
                ExprType::Le { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Le { lhs, rhs }, lhs, rhs)
                }
                ExprType::Eq { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Eq { lhs, rhs }, lhs, rhs)
                }
                ExprType::Ne { lhs, rhs } => {
                    bin!(|lhs, rhs| ExprType::Ne { lhs, rhs }, lhs, rhs)
                }
                ExprType::Overlay { lower, upper } => bin!(
                    |lower, upper| ExprType::Overlay { lower, upper },
                    lower,
                    upper
                ),
                ExprType::Apl { func, arg } => {
                    bin!(|func, arg| ExprType::Apl { func, arg }, func, arg)
                }
                ExprType::Test { base, path } => {
                    bin!(|base, path| ExprType::Test { base, path }, base, path)
                }
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
                ExprType::Cond { cond, then, el } => tern!(
                    |cond, then, el| ExprType::Cond { cond, then, el },
                    cond,
                    then,
                    el
                ),
                _ => None,
            };

            if let Some((f, a, b, c)) = tern {
                return new!(f(copy!(a), copy!(b), copy!(c)));
            }
        }

        // Everything else
        let rv = match *val {
            ExprType::Not { .. }
            | ExprType::Neg { .. }
            | ExprType::Stringify { .. }
            | ExprType::And { .. }
            | ExprType::Or { .. }
            | ExprType::Add { .. }
            | ExprType::Sub { .. }
            | ExprType::Mul { .. }
            | ExprType::Div { .. }
            | ExprType::Mod { .. }
            | ExprType::Gt { .. }
            | ExprType::Lt { .. }
            | ExprType::Ge { .. }
            | ExprType::Le { .. }
            | ExprType::Eq { .. }
            | ExprType::Ne { .. }
            | ExprType::Overlay { .. }
            | ExprType::Apl { .. }
            | ExprType::Test { .. }
            | ExprType::Cond { .. } => {
                // handled above
                unreachable!();
            }
            ExprType::Inherit => ExprType::Inherit,
            ExprType::String { content } => ExprType::String { content },
            ExprType::Number { ref val } => ExprType::Number { val: val.clone() },
            ExprType::Bool { val } => ExprType::Bool { val },
            ExprType::Null => ExprType::Null,
            ExprType::Resolved { ident, dest } => ExprType::Resolved { ident, dest },
            ExprType::Fn {
                func: FnType::BuiltIn { ref func },
            } => ExprType::Fn {
                func: FnType::BuiltIn { func: func.clone() },
            },
            ExprType::Ident { name } => ExprType::Ident { name },
            ExprType::List { ref elements } => {
                let mut nels = Vec::with_capacity(elements.len());
                for &el in elements.iter() {
                    nels.push(copy!(el));
                }
                ExprType::List {
                    elements: Rc::from(nels.into_boxed_slice()),
                }
            }
            ExprType::Map {
                ref fields,
                recursive,
            } => {
                let mut nfields = HashMap::with_capacity(fields.len());
                for (&id, &val) in fields.iter() {
                    nfields.insert(id, copy!(val));
                }
                ExprType::Map {
                    fields: Rc::new(nfields),
                    recursive,
                }
            }
            ExprType::Let { ref fields, body } => {
                let mut nfields = HashMap::with_capacity(fields.len());
                for (&id, &val) in fields.iter() {
                    nfields.insert(id, copy!(val));
                }
                let nbody = copy!(body);
                ExprType::Let {
                    fields: Rc::new(nfields),
                    body: nbody,
                }
            }
            ExprType::Path { ref path } => {
                let mut nsegs = Vec::with_capacity(path.len());
                for &seg in path.iter() {
                    nsegs.push(copy!(seg));
                }
                ExprType::Path {
                    path: Rc::from(nsegs.into_boxed_slice()),
                }
            }
            ExprType::Select { base, path, alt } => {
                let ntarget = copy!(base);
                let nsegs = copy!(path);
                let nalt = match alt {
                    Some(alt) => Some(copy!(alt)),
                    _ => None,
                };
                ExprType::Select {
                    base: ntarget,
                    path: nsegs,
                    alt: nalt,
                }
            }
            ExprType::Fn {
                func:
                    FnType::Normal {
                        param:
                            Spanned {
                                span,
                                val: FnParam::Ident { param_name },
                            },
                        body,
                    },
            } => ExprType::Fn {
                func: FnType::Normal {
                    param: span.span(FnParam::Ident { param_name }),
                    body: copy!(body),
                },
            },
            ExprType::Fn {
                func:
                    FnType::Normal {
                        param:
                            Spanned {
                                span,
                                val:
                                    FnParam::Pat {
                                        param_name,
                                        ref fields,
                                        wild,
                                    },
                            },
                        body,
                    },
            } => {
                let mut nfields = HashMap::with_capacity(fields.len());
                for (&id, &alt) in fields.iter() {
                    nfields.insert(id, alt.map(|alt| copy!(alt)));
                }
                let pat = FnParam::Pat {
                    param_name,
                    fields: Rc::new(nfields),
                    wild,
                };
                ExprType::Fn {
                    func: FnType::Normal {
                        param: span.span(pat),
                        body: copy!(body),
                    },
                }
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
                ExprType::Resolved { dest, .. } => {
                    ctx.push(ErrorContext::EvalResolved(eid));
                    eid = dest;
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
            ExprType::Number { ref val } => Value::Number((**val).clone()),
            ExprType::Bool { val } => Value::Bool(val),
            ExprType::Null => Value::Null,
            ExprType::String { content } => {
                Value::String(self.store.get_str(content).to_vec().into_boxed_slice())
            }
            ExprType::List { ref elements } => {
                let el = elements.clone();
                drop(expr);
                let mut l = vec![];
                for e in el.iter() {
                    l.push(self.get_value(*e)?);
                }
                Value::List(l.into_boxed_slice())
            }
            ExprType::Map { ref fields, .. } => {
                let f = fields.clone();
                drop(expr);
                let mut r = HashMap::new();
                for e in f.iter() {
                    let s = self.store.get_str(**e.0).to_vec().into_boxed_slice();
                    let val = self.get_value(*e.1)?;
                    r.insert(s, val);
                }
                Value::Map(r)
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
                            ExprKind::Map,
                        ],
                        expr.kind(),
                    ),
                );
            }
        };
        Ok(val)
    }
}

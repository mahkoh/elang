#![allow(unused)]

use crate::{
    types::{
        span::Span,
        store::Store,
        tree::{BuiltInFn, Expr, ExprType, FnType},
    },
    Elang, Error, ErrorType, ExprKind,
};
use std::rc::Rc;

macro_rules! bi {
    ($f:expr) => {
        ExprType::Fn {
            func: FnType::BuiltIn { func: Rc::new($f) },
        }
    };
}

pub fn to_list() -> Rc<dyn BuiltInFn> {
    let f = move |eval: &mut Elang, e: Rc<Expr>| {
        let fields = eval.get_fields(e.id)?;
        let mut list = Vec::with_capacity(fields.len());
        for &val in fields.values() {
            list.push(val);
        }
        Ok(ExprType::List {
            elements: Rc::from(list.into_boxed_slice()),
        })
    };
    Rc::new(f)
}

pub fn assert() -> Rc<dyn BuiltInFn> {
    let f = move |eval: &mut Elang, cond: Rc<Expr>| {
        if eval.get_bool(cond.id)? {
            let f = move |eval: &mut Elang, tail: Rc<Expr>| {
                Ok(ExprType::Resolved {
                    ident: None,
                    dest: tail.id,
                })
            };
            Ok(bi!(f))
        } else {
            Err(Error {
                span: cond.span,
                error: ErrorType::AssertionFailed,
                context: vec![],
            })
        }
    };
    Rc::new(f)
}

pub fn contains() -> Rc<dyn BuiltInFn> {
    let f = move |eval: &mut Elang, list: Rc<Expr>| {
        let list = eval.get_list(list.id)?;
        let f = move |eval: &mut Elang, val: Rc<Expr>| {
            for &el in list.iter() {
                if eval.equal_to(el, val.id)? {
                    return Ok(ExprType::Bool { val: true });
                }
            }
            Ok(ExprType::Bool { val: false })
        };
        Ok(bi!(f))
    };
    Rc::new(f)
}

pub fn filter() -> Rc<dyn BuiltInFn> {
    let f = move |eval: &mut Elang, cond: Rc<Expr>| {
        let f = move |eval: &mut Elang, olist: Rc<Expr>| {
            let list = eval.get_list(olist.id)?;
            let mut nlist = Vec::with_capacity(list.len());
            for &el in list.iter() {
                let span = Span::new(olist.span.lo, cond.span.hi);
                let expr = eval.add_expr(
                    span,
                    ExprType::Apl {
                        func: cond.id,
                        arg: el,
                    },
                );
                if eval.get_bool(expr)? {
                    nlist.push(el);
                }
            }
            Ok(ExprType::List {
                elements: Rc::from(nlist.into_boxed_slice()),
            })
        };
        Ok(bi!(f))
    };
    Rc::new(f)
}

pub fn ty() -> Rc<dyn BuiltInFn> {
    let f = move |eval: &mut Elang, e: Rc<Expr>| {
        let val = eval.resolve(e.id)?;
        let ty = match *val.val.borrow() {
            ExprType::Number { .. } => "number",
            ExprType::String { .. } => "string",
            ExprType::Fn { .. } => "fn",
            ExprType::Map { .. } => "map",
            ExprType::List { .. } => "list",
            ExprType::Bool { .. } => "bool",
            ExprType::Null => "null",
            ref o => {
                return Err(eval.error(
                    e.id,
                    ErrorType::UnexpectedExprKind(
                        &[
                            ExprKind::Number,
                            ExprKind::String,
                            ExprKind::Fn,
                            ExprKind::Map,
                            ExprKind::List,
                            ExprKind::Bool,
                            ExprKind::Null,
                        ],
                        o.kind(),
                    ),
                ))
            }
        };
        let s = eval.intern(ty.as_bytes().to_vec().into_boxed_slice().into());
        Ok(ExprType::String { content: s })
    };
    Rc::new(f)
}

macro_rules! is {
    ($name:ident, $pat:pat) => {
        pub fn $name() -> Rc<dyn BuiltInFn> {
            let f = move |eval: &mut Elang, e: Rc<Expr>| {
                let val = eval.resolve(e.id)?;
                let val = match *val.val.borrow() {
                    $pat => true,
                    _ => false,
                };
                Ok(ExprType::Bool { val })
            };
            Rc::new(f)
        }
    }
}

is!(is_number, ExprType::Number { .. });
is!(is_string, ExprType::String { .. });
is!(is_fn, ExprType::Fn { .. });
is!(is_map, ExprType::Map { .. });
is!(is_list, ExprType::List { .. });
is!(is_bool, ExprType::Bool { .. });
is!(is_null, ExprType::Null);

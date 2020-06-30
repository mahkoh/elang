#![allow(unused)]

use crate::{
    types::{
        span::Span,
        store::Store,
        tree::{NativeFn, Expr, ExprType, FnType},
    },
    Elang, Error, ErrorType, ExprKind,
};
use std::rc::Rc;

macro_rules! bi {
    ($f:expr) => {
        ExprType::Fn {
            func: FnType::Native { func: Rc::new($f) },
        }
    };
}

pub fn to_list() -> Rc<dyn NativeFn> {
    let f = move |eval: &mut Elang, e: Rc<Expr>| {
        let fields = eval.get_map(e.id)?;
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

pub fn assert() -> Rc<dyn NativeFn> {
    let f = move |eval: &mut Elang, cond: Rc<Expr>| {
        if eval.get_bool(cond.id)? {
            let f = |_: &mut Elang, _msg: Rc<Expr>| {
                let f = |_: &mut Elang, expr: Rc<Expr>| {
                    Ok(ExprType::Resolved {
                        ident: None,
                        dest: expr.id,
                    })
                };
                Ok(bi!(f))
            };
            Ok(bi!(f))
        } else {
            let f = |eval: &mut Elang, msg: Rc<Expr>| {
                let str = eval.get_string(msg.id)?;
                Err(eval.error(msg.id, ErrorType::AssertionFailed { msg: str }))
            };
            Ok(bi!(f))
        }
    };
    Rc::new(f)
}

pub fn contains() -> Rc<dyn NativeFn> {
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

pub fn raise() -> Rc<dyn NativeFn> {
    let f = move |eval: &mut Elang, msg: Rc<Expr>| {
        let str = eval.get_string(msg.id)?;
        Err(eval.error(msg.id, ErrorType::Raised { msg: str }))
    };
    Rc::new(f)
}

pub fn filter() -> Rc<dyn NativeFn> {
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

pub fn ty() -> Rc<dyn NativeFn> {
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
                return Err(eval.perror(
                    e.id,
                    ErrorType::UnexpectedExprKind {
                        expected: &[
                            ExprKind::Number,
                            ExprKind::String,
                            ExprKind::Fn,
                            ExprKind::Map,
                            ExprKind::List,
                            ExprKind::Bool,
                            ExprKind::Null,
                        ],
                        encountered: o.kind(),
                    },
                ))
            }
        };
        let s = eval.intern(ty);
        Ok(ExprType::String { content: s })
    };
    Rc::new(f)
}

macro_rules! is {
    ($name:ident, $pat:pat) => {
        pub fn $name() -> Rc<dyn NativeFn> {
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
    };
}

is!(is_number, ExprType::Number { .. });
is!(is_string, ExprType::String { .. });
is!(is_fn, ExprType::Fn { .. });
is!(is_map, ExprType::Map { .. });
is!(is_list, ExprType::List { .. });
is!(is_bool, ExprType::Bool { .. });
is!(is_null, ExprType::Null);

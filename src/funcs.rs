#![allow(unused)]

use crate::{types::{
    span::Span,
    store::Store,
    tree::{BuiltInFn, Expr, FnType, Value},
}, Elang, Error, ErrorType};
use std::rc::Rc;

macro_rules! bi {
    ($f:expr) => {
        Value::Fn(FnType::BuiltIn(Rc::new($f)))
    };
}

pub fn to_list() -> Rc<dyn BuiltInFn> {
    let f = move |eval: &mut Elang, e: Rc<Expr>| {
        let fields = eval.get_fields(e.id)?;
        let mut list = Vec::with_capacity(fields.len());
        for &(_, val) in fields.values() {
            list.push(val);
        }
        Ok(Value::List(Rc::from(list.into_boxed_slice())))
    };
    Rc::new(f)
}

pub fn assert() -> Rc<dyn BuiltInFn> {
    let f = move |eval: &mut Elang, cond: Rc<Expr>| {
        if eval.get_bool(cond.id)? {
            let f = move |eval: &mut Elang, tail: Rc<Expr>| Ok(Value::Resolved(None, tail.id));
            Ok(bi!(f))
        } else {
            Err(Error {
                span: cond.span,
                error: ErrorType::AssertionFailed,
                context: vec!(),
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
                    return Ok(Value::Bool(true));
                }
            }
            Ok(Value::Bool(false))
        };
        Ok(bi!(f))
    };
    Rc::new(f)
}

pub fn filter() -> Rc<dyn BuiltInFn> {
    let f = move |eval: &mut Elang, olist: Rc<Expr>| {
        let list = eval.get_list(olist.id)?;
        let f = move |eval: &mut Elang, cond: Rc<Expr>| {
            let mut nlist = Vec::with_capacity(list.len());
            for &el in list.iter() {
                let span = Span::new(olist.span.lo, cond.span.hi);
                let expr = eval.add_expr(span, Value::Apl(cond.id, el));
                if eval.get_bool(expr)? {
                    nlist.push(el);
                }
            }
            Ok(Value::List(Rc::from(nlist.into_boxed_slice())))
        };
        Ok(bi!(f))
    };
    Rc::new(f)
}

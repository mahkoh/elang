#![allow(unused)]

use crate::{
    types::{
        diagnostic::MsgDetails,
        span::Span,
        store::Store,
        tree::{BuiltInFn, Expr, FnType, Value},
    },
    Elang, Error, MsgLevel,
};
use std::rc::Rc;

macro_rules! bi {
    ($f:expr) => {
        Value::Fn(FnType::BuiltIn(Rc::new($f)))
    };
}

pub fn to_list(eval: Rc<Elang>) -> Rc<dyn BuiltInFn> {
    let f = move |e: Rc<Expr>| {
        let fields = eval.get_fields(e.id)?;
        let mut list = Vec::with_capacity(fields.len());
        for &(_, val) in fields.values() {
            list.push(val);
        }
        Ok(Value::List(Rc::from(list.into_boxed_slice())))
    };
    Rc::new(f)
}

pub fn assert(eval: Rc<Elang>) -> Rc<dyn BuiltInFn> {
    let f = move |cond: Rc<Expr>| {
        let eval = eval.clone();
        let f = move |tail: Rc<Expr>| {
            if eval.get_bool(cond.id)? {
                Ok(Value::Resolved(None, tail.id))
            } else {
                Err(Error {
                    span: cond.span,
                    level: MsgLevel::Error,
                    details: MsgDetails::AssertionFailed,
                    children: vec![],
                })
            }
        };
        Ok(bi!(f))
    };
    Rc::new(f)
}

// pub fn contains(eval: Rc<Elang>) -> Rc<dyn BuiltInFn> {
//     let f = move |list: Rc<Expr>| {
//         let eval = eval.clone();
//         let f = move |val: Rc<Expr>| {
//             let list = eval.get_list(list.id)?;
//             for &el in list.iter() {
//                 if eval.equal_to(el, val.id)? {
//                     return Ok(Value::Bool(true));
//                 }
//             }
//             Ok(Value::Bool(false))
//         };
//         Ok(bi!(f))
//     };
//     Rc::new(f)
// }

pub fn filter(eval: Rc<Elang>, store: Store) -> Rc<dyn BuiltInFn> {
    let f = move |olist: Rc<Expr>| {
        let eval = eval.clone();
        let store = store.clone();
        let f = move |cond: Rc<Expr>| {
            let list = eval.get_list(olist.id)?;
            let mut nlist = Vec::with_capacity(list.len());
            for &el in list.iter() {
                let span = Span::new(olist.span.lo, cond.span.hi);
                let expr = store.add_expr(span, Value::Apl(cond.id, el));
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

use crate::types::{
    span::Span,
    tree::{Expr, ExprId, Value},
};
use std::{
    cell::{Cell, RefCell},
    collections::{hash_map::Entry, HashMap},
    convert::TryInto,
    fmt,
    fmt::{Debug, Formatter},
    rc::Rc,
};

/// A string interned in an interned.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct StrId(u32);

impl Debug for StrId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Interned({})", self.0)
    }
}

#[derive(Clone)]
pub struct Store {
    inner: Rc<Cell<Inner>>,
}

impl Store {
    pub fn new() -> Self {
        Self {
            inner: Rc::new(Cell::new(Inner {
                exprs: vec![],
                strs: vec![],
                str_to_id: Default::default(),
            })),
        }
    }

    #[allow(clippy::mut_from_ref)]
    fn inner(&self) -> &mut Inner {
        unsafe { &mut *self.inner.as_ptr() }
    }

    pub fn add_expr(&self, span: Span, expr: Value) -> ExprId {
        let inner = self.inner();
        let id = ExprId {
            id: inner.exprs.len().try_into().unwrap(),
        };
        inner.exprs.push(Rc::new(Expr {
            id,
            span,
            val: RefCell::new(expr),
        }));
        id
    }

    pub fn get_expr(&self, expr: ExprId) -> Rc<Expr> {
        let inner = self.inner();
        inner.exprs[expr.id as usize].clone()
    }

    pub fn add_str(&self, val: Rc<[u8]>) -> StrId {
        let inner = self.inner();
        let pos = {
            match inner.str_to_id.entry(val.clone()) {
                Entry::Vacant(v) => {
                    let pos = inner.strs.len();
                    inner.strs.push(val);
                    v.insert(pos);
                    pos
                }
                Entry::Occupied(o) => *o.get(),
            }
        };
        StrId(pos as u32)
    }

    pub fn get_str(&self, i: StrId) -> Rc<[u8]> {
        let inner = self.inner();
        let i = i.0 as usize;
        assert!(i < inner.strs.len());
        inner.strs[i].clone()
    }

    pub(crate) fn concat(&self, left: StrId, right: StrId) -> StrId {
        let tmp = self.get_str(left);
        let mut l = tmp.to_vec();
        let r = self.get_str(right);
        l.extend_from_slice(&r);
        self.add_str(l.into_boxed_slice().into())
    }
}

struct Inner {
    exprs: Vec<Rc<Expr>>,
    strs: Vec<Rc<[u8]>>,
    str_to_id: HashMap<Rc<[u8]>, usize>,
}

use crate::types::{
    span::Span,
    tree::{Expr, ExprId, ExprType},
};
use std::{
    cell::RefCell,
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

pub struct Store {
    exprs: Vec<Rc<Expr>>,
    strs: Vec<Rc<[u8]>>,
    str_to_id: HashMap<Rc<[u8]>, usize>,
}

impl Store {
    pub fn new() -> Self {
        Self {
            exprs: vec![],
            strs: vec![],
            str_to_id: Default::default(),
        }
    }

    pub fn add_expr(&mut self, span: Span, expr: ExprType) -> ExprId {
        let id = ExprId {
            id: self.exprs.len().try_into().unwrap(),
        };
        self.exprs.push(Rc::new(Expr {
            id,
            span,
            val: RefCell::new(expr),
        }));
        id
    }

    pub fn get_expr(&self, expr: ExprId) -> Rc<Expr> {
        self.exprs[expr.id as usize].clone()
    }

    pub fn add_str(&mut self, val: &[u8]) -> StrId {
        if let Some(&id) = self.str_to_id.get(val) {
            return StrId(id as u32);
        }
        let rc: Rc<[u8]> = val.to_vec().into_boxed_slice().into();
        let pos = self.strs.len();
        self.strs.push(rc.clone());
        self.str_to_id.insert(rc, pos);
        StrId(pos as u32)
    }

    pub fn add_string(&mut self, val: Rc<[u8]>) -> StrId {
        let pos = {
            match self.str_to_id.entry(val.clone()) {
                Entry::Vacant(v) => {
                    let pos = self.strs.len();
                    self.strs.push(val);
                    v.insert(pos);
                    pos
                }
                Entry::Occupied(o) => *o.get(),
            }
        };
        StrId(pos as u32)
    }

    pub fn get_str(&self, i: StrId) -> Rc<[u8]> {
        let i = i.0 as usize;
        assert!(i < self.strs.len());
        self.strs[i].clone()
    }

    pub(crate) fn concat(&mut self, left: StrId, right: StrId) -> StrId {
        let tmp = self.get_str(left);
        let mut l = tmp.to_vec();
        let r = self.get_str(right);
        l.extend_from_slice(&r);
        self.add_string(l.into_boxed_slice().into())
    }
}

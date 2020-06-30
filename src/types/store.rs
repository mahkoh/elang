use crate::types::{
    span::Span,
    tree::{Expr, ExprId, ExprType},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    convert::TryInto,
    fmt,
    fmt::{Debug, Formatter},
    rc::Rc,
};

/// Trait for types that can be interned
///
/// The `into_bytes` and `as_bytes` methods must return the same slice of bytes.
pub trait Intern {
    /// Returns the bytes to be interned
    fn as_bytes(&self) -> &[u8];

    /// Returns the bytes to be interned
    fn into_bytes(self) -> Rc<[u8]>;
}

/// The id of a string stored in an elang engine
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

    pub fn add_str<T: Intern>(&mut self, val: T) -> StrId {
        if let Some(&id) = self.str_to_id.get(val.as_bytes()) {
            return StrId(id as u32);
        }
        let rc: Rc<[u8]> = val.into_bytes();
        let pos = self.strs.len();
        self.strs.push(rc.clone());
        self.str_to_id.insert(rc, pos);
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
        self.add_str(l)
    }
}

impl<'a> Intern for &'a [u8] {
    fn as_bytes(&self) -> &[u8] {
        *self
    }

    fn into_bytes(self) -> Rc<[u8]> {
        self.to_vec().into_boxed_slice().into()
    }
}

impl Intern for Vec<u8> {
    fn as_bytes(&self) -> &[u8] {
        self
    }

    fn into_bytes(self) -> Rc<[u8]> {
        self.into_boxed_slice().into()
    }
}

impl Intern for Rc<[u8]> {
    fn as_bytes(&self) -> &[u8] {
        self
    }

    fn into_bytes(self) -> Rc<[u8]> {
        self
    }
}

impl Intern for Box<[u8]> {
    fn as_bytes(&self) -> &[u8] {
        self
    }

    fn into_bytes(self) -> Rc<[u8]> {
        self.into()
    }
}

impl<'a> Intern for &'a str {
    fn as_bytes(&self) -> &[u8] {
        str::as_bytes(*self)
    }

    fn into_bytes(self) -> Rc<[u8]> {
        self.as_bytes().to_vec().into_boxed_slice().into()
    }
}

impl Intern for String {
    fn as_bytes(&self) -> &[u8] {
        str::as_bytes(self)
    }

    fn into_bytes(self) -> Rc<[u8]> {
        self.into_bytes().into_boxed_slice().into()
    }
}

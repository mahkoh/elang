use crate::util::str::Utf8Lossy;
use num_rational::BigRational;
use std::{
    collections::HashMap,
    fmt,
    fmt::{Debug, Formatter},
};

#[derive(Clone, Eq, PartialEq)]
pub enum Value {
    Bool(bool),
    List(Box<[Value]>),
    Null,
    Number(BigRational),
    Set(HashMap<Box<[u8]>, Value>),
    String(Box<[u8]>),
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Value::Bool(b) => b.fmt(f),
            Value::List(ref l) => f.debug_list().entries(l.iter()).finish(),
            Value::Null => f.write_str("null"),
            Value::Number(ref n) => write!(f, "{}", n),
            Value::Set(ref el) => f
                .debug_map()
                .entries(el.iter().map(|(k, v)| (Utf8Lossy::from_bytes(k), v)))
                .finish(),
            Value::String(ref s) => Utf8Lossy::from_bytes(s).fmt(f),
        }
    }
}

use num_rational::BigRational;
use std::{collections::HashMap};

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    List(Box<[Value]>),
    Null,
    Number(BigRational),
    Set(HashMap<Box<[u8]>, Value>),
    String(Box<[u8]>),
}

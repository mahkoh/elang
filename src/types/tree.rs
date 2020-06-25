use crate::{
    types::{
        result::Result,
        span::{Span, Spanned},
        store::StrId,
    },
    Elang,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt,
    fmt::{Debug, Formatter},
    hash::Hash,
    rc::Rc,
};

/// An expression with an associated span.
pub type SExpr = Spanned<ExprId>;

/// A reference-counted expression with interior mutability.
///
/// = Remarks
///
/// Instead of copying `Expr_`s every time they are used, we add references to them. This
/// way we only have to evaluate each `Expr_` once.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct ExprId {
    pub(crate) id: u32,
}

#[non_exhaustive]
#[derive(Debug)]
pub struct Expr {
    pub(crate) id: ExprId,
    pub(crate) span: Span,
    pub(crate) val: RefCell<Value>,
}

impl Expr {
    /// Returns whether this is a inherit expression.
    pub(crate) fn is_inherit(&self) -> bool {
        match *self.val.borrow() {
            Value::Inherit => true,
            _ => false,
        }
    }

    pub fn id(&self) -> ExprId {
        self.id
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn value(&self) -> &RefCell<Value> {
        &self.val
    }
}

/// The value of an expression
///
/// In the field documentation, `e1`, `e2`, and `e3` denote expressions.
#[derive(Clone, Debug)]
pub enum Value {
    /// `e1 + e2`
    Add(ExprId, ExprId),
    /// `e1 && e2`
    And(ExprId, ExprId),
    /// `e1 e2`
    Apl(ExprId, ExprId),
    /// A boolean
    Bool(bool),
    /// `e1 ++ e2`
    Concat(ExprId, ExprId),
    /// `if e1 then e2 else e3`
    Cond(ExprId, ExprId, ExprId),
    /// `e1 / e2`
    Div(ExprId, ExprId),
    /// `e1 == e2`
    Eq(ExprId, ExprId),
    /// A function
    Fn(FnType),
    /// `e1 >= e2`
    Ge(ExprId, ExprId),
    /// `e1 > e2`
    Gt(ExprId, ExprId),
    /// An identifier
    Ident(StrId),
    /// `e1 -> e2`
    Impl(ExprId, ExprId),
    /// `inherit`
    ///
    /// This only appears in the fields of a set
    Inherit,
    /// An integer
    Integer(i64),
    /// `e1 <= e2`
    Le(ExprId, ExprId),
    /// `let fields in e1`
    Let(Fields, ExprId),
    /// `[e1, e2, e3]`
    List(Rc<[ExprId]>),
    /// `e1 < e2`
    Lt(ExprId, ExprId),
    /// `e1 % e2`
    Mod(ExprId, ExprId),
    /// `e1 * e2`
    Mul(ExprId, ExprId),
    /// `e1 != e2`
    Ne(ExprId, ExprId),
    /// `-e1`
    Neg(ExprId),
    /// `!e1`
    Not(ExprId),
    /// `null`
    Null,
    /// `e1 || e2`
    Or(ExprId, ExprId),
    /// `e1 \\ e2`
    Overlay(ExprId, ExprId),
    /// `e1.e2.e3`
    ///
    /// Each element is a `Value::Selector`.
    Path(Rc<[ExprId]>),
    /// A reference to another expression
    Resolved(Option<StrId>, ExprId),
    /// `e1.e2 or e3`
    ///
    /// `e2` is a `Value::Path`.
    Select(ExprId, ExprId, Option<ExprId>),
    /// A Selector of a field
    Selector(Selector),
    /// `rec { i = e1 }`
    ///
    /// The boolean is true iff the set is recursive
    Set(Fields, bool),
    /// `"\{e1}"`
    Stringify(ExprId),
    /// A string
    String(StrId),
    /// `e1 - e2`
    Sub(ExprId, ExprId),
    /// `e1 ? e2`
    ///
    /// `e2` is a `Value::Path`
    Test(ExprId, ExprId),
}

/// The type of a value of an expression
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ValueType {
    Add,
    And,
    Apl,
    Bool,
    Concat,
    Cond,
    Div,
    Eq,
    Fn,
    Ge,
    Gt,
    Ident,
    Impl,
    Inherit,
    Integer,
    Le,
    Let,
    List,
    Lt,
    Mod,
    Mul,
    Ne,
    Neg,
    Not,
    Null,
    Or,
    Overlay,
    Path,
    Resolved,
    Select,
    Selector,
    Set,
    Stringify,
    String,
    Sub,
    Test,
}

impl ValueType {
    pub fn as_str(self) -> &'static str {
        match self {
            ValueType::Inherit => "inherit",
            ValueType::String => "string",
            ValueType::Integer => "integer",
            ValueType::Ident => "ident",
            ValueType::Set => "set",
            ValueType::And => "and",
            ValueType::Or => "or",
            ValueType::Not => "not",
            ValueType::Add => "add",
            ValueType::Sub => "sub",
            ValueType::Mul => "mul",
            ValueType::Div => "div",
            ValueType::Mod => "mod",
            ValueType::Gt => "gt",
            ValueType::Lt => "lt",
            ValueType::Ge => "ge",
            ValueType::Le => "le",
            ValueType::Eq => "eq",
            ValueType::Ne => "ne",
            ValueType::Impl => "imlp",
            ValueType::Overlay => "overlay",
            ValueType::Concat => "concat",
            ValueType::Apl => "apl",
            ValueType::Neg => "neg",
            ValueType::Cond => "cond",
            ValueType::Bool => "bool",
            ValueType::Null => "null",
            ValueType::Test => "test",
            ValueType::Select => "select",
            ValueType::List => "list",
            ValueType::Let => "let",
            ValueType::Fn => "fn",
            ValueType::Stringify => "stringify",
            ValueType::Path => "path",
            ValueType::Selector => "selector",
            ValueType::Resolved => "resolved",
        }
    }
}

impl Value {
    pub fn ty(&self) -> ValueType {
        match *self {
            Value::Add(..) => ValueType::Add,
            Value::And(..) => ValueType::And,
            Value::Apl(..) => ValueType::Apl,
            Value::Bool(..) => ValueType::Bool,
            Value::Concat(..) => ValueType::Concat,
            Value::Cond(..) => ValueType::Cond,
            Value::Div(..) => ValueType::Div,
            Value::Eq(..) => ValueType::Eq,
            Value::Fn(..) => ValueType::Fn,
            Value::Ge(..) => ValueType::Ge,
            Value::Gt(..) => ValueType::Gt,
            Value::Ident(..) => ValueType::Ident,
            Value::Impl(..) => ValueType::Impl,
            Value::Inherit => ValueType::Inherit,
            Value::Integer(..) => ValueType::Integer,
            Value::Le(..) => ValueType::Le,
            Value::Let(..) => ValueType::Let,
            Value::List(..) => ValueType::List,
            Value::Lt(..) => ValueType::Lt,
            Value::Mod(..) => ValueType::Mod,
            Value::Mul(..) => ValueType::Mul,
            Value::Ne(..) => ValueType::Ne,
            Value::Neg(..) => ValueType::Neg,
            Value::Not(..) => ValueType::Not,
            Value::Null => ValueType::Null,
            Value::Or(..) => ValueType::Or,
            Value::Overlay(..) => ValueType::Overlay,
            Value::Path(..) => ValueType::Path,
            Value::Resolved(..) => ValueType::Resolved,
            Value::Select(..) => ValueType::Select,
            Value::Selector(..) => ValueType::Selector,
            Value::Set(..) => ValueType::Set,
            Value::Stringify(..) => ValueType::Stringify,
            Value::String(..) => ValueType::String,
            Value::Sub(..) => ValueType::Sub,
            Value::Test(..) => ValueType::Test,
        }
    }

    pub fn debug<'a>(&'a self, e: &'a Elang) -> ExprDebug<'a> {
        ExprDebug { expr: self, e }
    }
}

pub struct ExprDebug<'a> {
    expr: &'a Value,
    e: &'a Elang,
}

impl<'a> Debug for ExprDebug<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self.expr {
            Value::Inherit => "Inherit",
            Value::String(..) => "String",
            Value::Integer(..) => "Integer",
            Value::Ident(..) => "Ident",
            Value::Set(..) => "Set",
            Value::And(..) => "And",
            Value::Or(..) => "Or",
            Value::Not(..) => "Not",
            Value::Add(..) => "Add",
            Value::Sub(..) => "Sub",
            Value::Mul(..) => "Mul",
            Value::Div(..) => "Div",
            Value::Mod(..) => "Mod",
            Value::Gt(..) => "Gt",
            Value::Lt(..) => "Lt",
            Value::Ge(..) => "Ge",
            Value::Le(..) => "Le",
            Value::Eq(..) => "Eq",
            Value::Ne(..) => "Ne",
            Value::Impl(..) => "Imlp",
            Value::Overlay(..) => "Overlay",
            Value::Concat(..) => "Concat",
            Value::Apl(..) => "Apl",
            Value::Neg(..) => "Neg",
            Value::Cond(..) => "Cond",
            Value::Bool(..) => "Bool",
            Value::Null => "Null",
            Value::Test(..) => "Test",
            Value::Select(..) => "Select",
            Value::List(..) => "List",
            Value::Let(..) => "Let",
            Value::Fn(..) => "Fn",
            Value::Stringify(..) => "Stringify",
            Value::Path(..) => "Path",
            Value::Selector(..) => "Selector",
            Value::Resolved(_, e) => {
                return self.e.get_expr(*e).val.borrow().debug(self.e).fmt(f)
            }
        };
        write!(f, "{}", s)
    }
}

pub type Fields = Rc<HashMap<StrId, (Span, ExprId)>>;

#[derive(Clone)]
pub enum FnArg {
    Ident(StrId),
    Pat(
        Option<Spanned<StrId>>,
        Rc<HashMap<StrId, (Span, Option<ExprId>)>>,
        bool,
    ),
}

#[derive(Copy, Clone, Debug)]
pub enum Selector {
    Ident(StrId),
    Integer(usize),
    Expr(ExprId),
}

pub trait BuiltInFn {
    fn apply(&self, elang: &mut Elang, expr: Rc<Expr>) -> Result<Value>;
}

impl<T> BuiltInFn for T
where
    T: Fn(&mut Elang, Rc<Expr>) -> Result<Value>,
{
    fn apply(&self, elang: &mut Elang, expr: Rc<Expr>) -> Result<Value> {
        self(elang, expr)
    }
}

#[derive(Clone)]
pub enum FnType {
    BuiltIn(Rc<dyn BuiltInFn>),
    Normal(Spanned<FnArg>, ExprId),
}

impl Debug for FnType {
    fn fmt(&self, _: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

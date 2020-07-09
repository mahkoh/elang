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
use crate::types::num::Number;

/// An expression with an associated span.
pub type SExpr = Spanned<ExprId>;

/// The id of an expression stored in an elang engine
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct ExprId {
    pub(crate) id: u32,
}

/// An expression
#[non_exhaustive]
#[derive(Debug)]
pub struct Expr {
    pub(crate) id: ExprId,
    pub(crate) span: Span,
    pub(crate) val: RefCell<ExprType>,
}

impl Expr {
    /// Returns whether this is a inherit expression.
    pub(crate) fn is_inherit(&self) -> bool {
        match *self.val.borrow() {
            ExprType::Inherit => true,
            _ => false,
        }
    }

    /// Returns the id of this expression
    pub fn id(&self) -> ExprId {
        self.id
    }

    /// Returns the span of this expression
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns the type of this expression
    pub fn ty(&self) -> &RefCell<ExprType> {
        &self.val
    }
}

/// The type of an expression
#[derive(Clone, Debug)]
pub enum ExprType {
    /// `lhs + rhs`
    Add { lhs: ExprId, rhs: ExprId },
    /// `lhs && rhs`
    And { lhs: ExprId, rhs: ExprId },
    /// `func arg`
    Apl { func: ExprId, arg: ExprId },
    /// A boolean
    Bool { val: bool },
    /// `if e1 then e2 else e3`
    Cond {
        /// `e1`
        cond: ExprId,
        /// `e2`
        then: ExprId,
        /// `e3`
        el: ExprId,
    },
    /// `numer / denom`
    Div {
        numer: ExprId,
        denom: ExprId,
        int: bool,
    },
    /// `lhs == rhs`
    Eq { lhs: ExprId, rhs: ExprId },
    /// A function
    Fn { func: FnType },
    /// `lhs >= rhs`
    Ge { lhs: ExprId, rhs: ExprId },
    /// `lhs > rhs`
    Gt { lhs: ExprId, rhs: ExprId },
    /// An identifier
    Ident { name: StrId },
    /// `inherit`
    Inherit,
    /// `lhs <= rhs`
    Le { lhs: ExprId, rhs: ExprId },
    /// `let fields in body`
    Let {
        fields: Rc<HashMap<Spanned<StrId>, ExprId>>,
        body: ExprId,
    },
    /// `[e1, e2, e3]`
    List { elements: Rc<[ExprId]> },
    /// `lhs < rhs`
    Lt { lhs: ExprId, rhs: ExprId },
    /// `rec { fields }`
    Map {
        fields: Rc<HashMap<Spanned<StrId>, ExprId>>,
        recursive: bool,
    },
    /// `numer % denom`
    Mod { numer: ExprId, denom: ExprId },
    /// `lhs * rhs`
    Mul { lhs: ExprId, rhs: ExprId },
    /// `lhs != rhs`
    Ne { lhs: ExprId, rhs: ExprId },
    /// `-val`
    Neg { val: ExprId },
    /// `!val`
    Not { val: ExprId },
    /// `null`
    Null,
    /// A number
    Number { val: Rc<Number> },
    /// `lhs || rhs`
    Or { lhs: ExprId, rhs: ExprId },
    /// `lower \\ upper`
    Overlay { lower: ExprId, upper: ExprId },
    /// `e1.e2.e3`
    Path { path: Rc<[ExprId]> },
    /// A reference to another expression
    Resolved { ident: Option<StrId>, dest: ExprId },
    /// `base.path or alt`
    Select {
        base: ExprId,
        path: ExprId,
        alt: Option<ExprId>,
    },
    /// The standard library
    Std,
    /// A string
    String { content: StrId },
    /// `"\{val}"`
    Stringify { val: ExprId },
    /// `lhs - rhs`
    Sub { lhs: ExprId, rhs: ExprId },
    /// `base ? path`
    Test { base: ExprId, path: ExprId },
}

/// The kind of an expression
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ExprKind {
    /// `lhs + rhs`
    Add,
    /// `lhs && rhs`
    And,
    /// `func arg`
    Apl,
    /// A boolean
    Bool,
    /// `if e1 then e2 else e3`
    Cond,
    /// `numer / denom`
    Div,
    /// `lhs == rhs`
    Eq,
    /// A function
    Fn,
    /// `lhs >= rhs`
    Ge,
    /// `lhs > rhs`
    Gt,
    /// An identifier
    Ident,
    /// `inherit`
    Inherit,
    /// `lhs <= rhs`
    Le,
    /// `let fields in body`
    Let,
    /// `[e1, e2, e3]`
    List,
    /// `lhs < rhs`
    Lt,
    /// `rec { fields }`
    Map,
    /// `numer % denom`
    Mod,
    /// `lhs * rhs`
    Mul,
    /// `lhs != rhs`
    Ne,
    /// `-val`
    Neg,
    /// `!val`
    Not,
    /// `null`
    Null,
    /// A number
    Number,
    /// `lhs || rhs`
    Or,
    /// `lower \\ upper`
    Overlay,
    /// `e1.e2.e3`
    Path,
    /// A reference to another expression
    Resolved,
    /// `base.path or alt`
    Select,
    /// The standard library
    Std,
    /// A string
    String,
    /// `"\{val}"`
    Stringify,
    /// `lhs - rhs`
    Sub,
    /// `base ? path`
    Test,
}

impl ExprKind {
    pub fn as_str(self) -> &'static str {
        match self {
            ExprKind::Inherit => "inherit",
            ExprKind::String => "string",
            ExprKind::Number => "number",
            ExprKind::Ident => "ident",
            ExprKind::Map => "map",
            ExprKind::And => "and",
            ExprKind::Or => "or",
            ExprKind::Not => "not",
            ExprKind::Add => "add",
            ExprKind::Sub => "sub",
            ExprKind::Mul => "mul",
            ExprKind::Div => "div",
            ExprKind::Mod => "mod",
            ExprKind::Gt => "gt",
            ExprKind::Lt => "lt",
            ExprKind::Ge => "ge",
            ExprKind::Le => "le",
            ExprKind::Eq => "eq",
            ExprKind::Ne => "ne",
            ExprKind::Overlay => "overlay",
            ExprKind::Apl => "apl",
            ExprKind::Neg => "neg",
            ExprKind::Cond => "cond",
            ExprKind::Bool => "bool",
            ExprKind::Null => "null",
            ExprKind::Test => "test",
            ExprKind::Select => "select",
            ExprKind::List => "list",
            ExprKind::Let => "let",
            ExprKind::Fn => "fn",
            ExprKind::Stringify => "stringify",
            ExprKind::Path => "path",
            ExprKind::Resolved => "resolved",
            ExprKind::Std => "std",
        }
    }
}

impl ExprType {
    pub fn kind(&self) -> ExprKind {
        match *self {
            ExprType::Add { .. } => ExprKind::Add,
            ExprType::And { .. } => ExprKind::And,
            ExprType::Apl { .. } => ExprKind::Apl,
            ExprType::Bool { .. } => ExprKind::Bool,
            ExprType::Cond { .. } => ExprKind::Cond,
            ExprType::Div { .. } => ExprKind::Div,
            ExprType::Eq { .. } => ExprKind::Eq,
            ExprType::Fn { .. } => ExprKind::Fn,
            ExprType::Ge { .. } => ExprKind::Ge,
            ExprType::Gt { .. } => ExprKind::Gt,
            ExprType::Ident { .. } => ExprKind::Ident,
            ExprType::Inherit => ExprKind::Inherit,
            ExprType::Number { .. } => ExprKind::Number,
            ExprType::Le { .. } => ExprKind::Le,
            ExprType::Let { .. } => ExprKind::Let,
            ExprType::List { .. } => ExprKind::List,
            ExprType::Lt { .. } => ExprKind::Lt,
            ExprType::Mod { .. } => ExprKind::Mod,
            ExprType::Mul { .. } => ExprKind::Mul,
            ExprType::Ne { .. } => ExprKind::Ne,
            ExprType::Neg { .. } => ExprKind::Neg,
            ExprType::Not { .. } => ExprKind::Not,
            ExprType::Null => ExprKind::Null,
            ExprType::Or { .. } => ExprKind::Or,
            ExprType::Overlay { .. } => ExprKind::Overlay,
            ExprType::Path { .. } => ExprKind::Path,
            ExprType::Resolved { .. } => ExprKind::Resolved,
            ExprType::Select { .. } => ExprKind::Select,
            ExprType::Map { .. } => ExprKind::Map,
            ExprType::Std { .. } => ExprKind::Std,
            ExprType::Stringify { .. } => ExprKind::Stringify,
            ExprType::String { .. } => ExprKind::String,
            ExprType::Sub { .. } => ExprKind::Sub,
            ExprType::Test { .. } => ExprKind::Test,
        }
    }
}

/// The parameter of a normal function
#[derive(Clone)]
pub enum FnParam {
    /// `a: body`
    Ident { param_name: StrId },
    /// `a @ { b, c ? e }: body`
    Pat {
        param_name: Option<Spanned<StrId>>,
        fields: Rc<HashMap<Spanned<StrId>, Option<ExprId>>>,
    },
}

/// A function written in Rust
pub trait NativeFn {
    /// Applies the function to an expression
    fn apply(&self, elang: &mut Elang, expr: Rc<Expr>) -> Result<ExprType>;
}

impl<T> NativeFn for T
where
    T: Fn(&mut Elang, Rc<Expr>) -> Result<ExprType>,
{
    fn apply(&self, elang: &mut Elang, expr: Rc<Expr>) -> Result<ExprType> {
        self(elang, expr)
    }
}

/// The type of a function
#[derive(Clone)]
pub enum FnType {
    /// A function written in Rust
    Native { func: Rc<dyn NativeFn> },
    /// A function written in elang
    Normal {
        param: Spanned<FnParam>,
        body: ExprId,
    },
}

impl Debug for FnType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FnType::Native { .. } => write!(f, "native"),
            FnType::Normal { .. } => write!(f, "normal"),
        }
    }
}

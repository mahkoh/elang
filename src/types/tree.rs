use crate::{
    types::{
        result::Result,
        span::{Span, Spanned},
        store::StrId,
    },
    Elang,
};
use num_rational::BigRational;
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

    pub fn id(&self) -> ExprId {
        self.id
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn value(&self) -> &RefCell<ExprType> {
        &self.val
    }
}

/// The value of an expression
///
/// In the field documentation, `e1`, `e2`, and `e3` denote expressions.
#[derive(Clone, Debug)]
pub enum ExprType {
    /// `e1 + e2`
    Add {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `e1 && e2`
    And {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `e1 e2`
    Apl {
        func: ExprId,
        arg: ExprId,
    },
    /// A boolean
    Bool {
        val: bool,
    },
    /// `e1 ++ e2`
    Concat {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `if e1 then e2 else e3`
    Cond {
        cond: ExprId,
        then: ExprId,
        el: ExprId,
    },
    /// `e1 / e2`
    Div {
        numer: ExprId,
        denom: ExprId,
    },
    /// `e1 == e2`
    Eq {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// A function
    Fn {
        func: FnType,
    },
    /// `e1 >= e2`
    Ge {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `e1 > e2`
    Gt {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// An identifier
    Ident {
        name: StrId,
    },
    /// `inherit`
    ///
    /// This only appears in the fields of a set
    Inherit,
    /// `e1 <= e2`
    Le {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `let fields in e1`
    Let {
        fields: Fields,
        body: ExprId,
    },
    /// `[e1, e2, e3]`
    List {
        elements: Rc<[ExprId]>,
    },
    /// `e1 < e2`
    Lt {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `e1 % e2`
    Mod {
        numer: ExprId,
        denom: ExprId,
    },
    /// `e1 * e2`
    Mul {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `e1 != e2`
    Ne {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `-e1`
    Neg {
        val: ExprId,
    },
    /// `!e1`
    Not {
        val: ExprId,
    },
    /// `null`
    Null,
    /// A number
    Number {
        val: Rc<BigRational>,
    },
    /// `e1 || e2`
    Or {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `e1 \\ e2`
    Overlay {
        lower: ExprId,
        upper: ExprId,
    },
    /// `e1.e2.e3`
    Path {
        path: Rc<[ExprId]>,
    },
    /// A reference to another expression
    Resolved {
        ident: Option<StrId>,
        dest: ExprId,
    },
    /// `e1.e2 or e3`
    Select {
        base: ExprId,
        path: ExprId,
        alt: Option<ExprId>,
    },
    /// `rec { i = e1 }`
    ///
    /// The boolean is true iff the set is recursive
    Set {
        fields: Fields,
        recursive: bool,
    },
    /// A string
    String {
        content: StrId,
    },
    /// `"\{e1}"`
    Stringify {
        val: ExprId,
    },
    /// `e1 - e2`
    Sub {
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `e1 ? e2`
    Test {
        base: ExprId,
        path: ExprId,
    },
}

/// The type of a value of an expression
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ExprKind {
    /// `e1 + e2`
    Add,
    /// `e1 && e2`
    And,
    /// `e1 e2`
    Apl,
    /// A boolean
    Bool,
    /// `e1 ++ e2`
    Concat,
    /// `if e1 then e2 else e3`
    Cond,
    /// `e1 / e2`
    Div,
    /// `e1 == e2`
    Eq,
    /// A function
    Fn,
    /// `e1 >= e2`
    Ge,
    /// `e1 > e2`
    Gt,
    /// An identifier
    Ident,
    /// `e1 -> e2`
    Impl,
    /// `inherit`
    ///
    /// This only appears in the fields of a set
    Inherit,
    /// `e1 <= e2`
    Le,
    /// `let fields in e1`
    Let,
    /// `[e1, e2, e3]`
    List,
    /// `e1 < e2`
    Lt,
    /// `e1 % e2`
    Mod,
    /// `e1 * e2`
    Mul,
    /// `e1 != e2`
    Ne,
    /// `-e1`
    Neg,
    /// `!e1`
    Not,
    /// `null`
    Null,
    /// A number
    Number,
    /// `e1 || e2`
    Or,
    /// `e1 \\ e2`
    Overlay,
    /// `e1.e2.e3`
    Path,
    /// A reference to another expression
    Resolved,
    /// `e1.e2 or e3`
    ///
    /// `e2` is a `Value::Path`.
    Select,
    /// `rec { i = e1 }`
    ///
    /// The boolean is true iff the set is recursive
    Set,
    /// A string
    String,
    /// `"\{e1}"`
    Stringify,
    /// `e1 - e2`
    Sub,
    /// `e1 ? e2`
    ///
    /// `e2` is a `Value::Path`
    Test,
}

impl ExprKind {
    pub fn as_str(self) -> &'static str {
        match self {
            ExprKind::Inherit => "inherit",
            ExprKind::String => "string",
            ExprKind::Number => "number",
            ExprKind::Ident => "ident",
            ExprKind::Set => "set",
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
            ExprKind::Impl => "imlp",
            ExprKind::Overlay => "overlay",
            ExprKind::Concat => "concat",
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
            ExprType::Concat { .. } => ExprKind::Concat,
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
            ExprType::Set { .. } => ExprKind::Set,
            ExprType::Stringify { .. } => ExprKind::Stringify,
            ExprType::String { .. } => ExprKind::String,
            ExprType::Sub { .. } => ExprKind::Sub,
            ExprType::Test { .. } => ExprKind::Test,
        }
    }
}

pub type Fields = Rc<HashMap<Spanned<StrId>, ExprId>>;

#[derive(Clone)]
pub enum FnParam {
    Ident {
        param_name: StrId,
    },
    Pat {
        param_name: Option<Spanned<StrId>>,
        fields: Rc<HashMap<Spanned<StrId>, Option<ExprId>>>,
        wild: bool,
    },
}

pub trait BuiltInFn {
    fn apply(&self, elang: &mut Elang, expr: Rc<Expr>) -> Result<ExprType>;
}

impl<T> BuiltInFn for T
where
    T: Fn(&mut Elang, Rc<Expr>) -> Result<ExprType>,
{
    fn apply(&self, elang: &mut Elang, expr: Rc<Expr>) -> Result<ExprType> {
        self(elang, expr)
    }
}

#[derive(Clone)]
pub enum FnType {
    BuiltIn {
        func: Rc<dyn BuiltInFn>,
    },
    Normal {
        param: Spanned<FnParam>,
        body: ExprId,
    },
}

impl Debug for FnType {
    fn fmt(&self, _: &mut Formatter<'_>) -> fmt::Result {
        unimplemented!()
    }
}

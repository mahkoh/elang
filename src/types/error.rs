use crate::{
    types::{span::Span, store::StrId, token::TokenKind, tree::ExprKind},
    ExprId, Spanned,
};
use num_rational::BigRational;
use std::{fmt::Debug, mem, rc::Rc};

/// An error
#[derive(Debug)]
pub struct Error {
    pub(crate) span: Span,
    pub(crate) error: ErrorType,
    pub(crate) context: Vec<ErrorContext>,
}

impl Error {
    /// Creates a new error
    pub fn new(span: Span, ty: ErrorType) -> Self {
        Error {
            span,
            error: ty,
            context: vec![],
        }
    }

    /// Returns the span of the error
    pub fn span(&self) -> Span {
        self.span
    }

    /// Returns the type of the error
    pub fn ty(&self) -> &ErrorType {
        &self.error
    }

    /// Adds context to an error
    pub fn add_context(mut self, ctx: ErrorContext) -> Self {
        self.context.push(ctx);
        self
    }

    /// Retrieves the current context of the error
    pub fn get_context(&self) -> &[ErrorContext] {
        &self.context
    }

    /// Replaces the context of the error
    pub fn set_context(&mut self, context: Vec<ErrorContext>) -> Vec<ErrorContext> {
        mem::replace(&mut self.context, context)
    }
}

/// The context in which an error occurred
#[derive(Copy, Clone, Debug)]
#[non_exhaustive]
pub enum ErrorContext {
    /// The error occurred while parsing a string
    ParseString { start: u32 },
    /// The error occurred while parsing a unicode escape sequence
    ParseUnicodeEscape { start: u32 },
    /// The error occurred while parsing a test expression
    ParseTest { start: u32 },
    /// The error occurred while parsing a select expression
    ParseSelect { start: u32 },
    /// The error occurred while parsing a parenthesized expression
    ParseParenthesized { start: u32 },
    /// The error occurred while parsing a function header
    ParseFnHeader { start: u32 },
    /// The error occurred while parsing a function pattern
    ParseFnPattern { start: u32 },
    /// The error occurred while parsing a let expression
    ParseLet { start: u32 },
    /// The error occurred while parsing a map field
    ParseField { start: u32 },
    /// The error occurred while parsing a conditional expression
    ParseCond { start: u32 },
    /// The error occurred while parsing a list
    ParseList { start: u32 },
    /// The error occurred while parsing a map
    ParseMap { start: u32 },
    /// The error occurred while parsing an inherit map field
    ParseInherit { start: u32 },
    /// The error occurred while evaluating an expression that resolved to another
    /// expression
    EvalResolved { pointer: ExprId },
    /// The error occurred while evaluating an arithmetic expression
    EvalArithmetic { arithmetic_expr: ExprId },
    /// The error occurred while evaluating a boolean expression
    EvalBool { boolean_expr: ExprId },
    /// The error occurred while evaluating an overlay expression
    EvalOverlay { overlay_expr: ExprId },
    /// The error occurred while evaluating an add expression
    EvalAdd { add_expr: ExprId },
    /// The error occurred while evaluating a conditional expression
    EvalCond { cond_expr: ExprId },
    /// The error occurred while evaluating a string interpolation
    EvalStringify { stringify_expr: ExprId },
    /// The error occurred while evaluating a function application
    EvalApl { apl_expr: ExprId },
    /// The error occurred while evaluating a select expression
    EvalSelect { select_expr: ExprId },
    /// The error occurred because another expression evaluated to an incompatible type
    EvalOtherExprKind {
        other_expr: ExprId,
        other_expr_kind: ExprKind,
    },
    /// The error occurred while trying to match a function pattern
    EvalFnPat { fn_pat_span: Span },
}

/// The type of an error
#[derive(Clone, Debug)]
#[non_exhaustive]
pub enum ErrorType {
    /// During parsing, the end of the input was reached unexpectedly
    UnexpectedEndOfInput,
    /// During parsing, an unexpected token was encountered
    UnexpectedToken {
        expected: TokenAlternative,
        encountered: TokenKind,
    },
    /// During parsing, a literal was encountered that is out-of-bounds for its type
    OutOfBoundsLiteral,
    /// During parsing of a number literal, a character in the range 0..=9 | a..=z | A..=Z was
    /// encountered which is not valid for the base of the literal
    InvalidDigit { digit: u8 },
    /// During parsing of a number literal with an explicit base, no digits were encountered after
    /// the base specifier
    EmptyNumberLiteral,
    /// During lexing, a byte was encountered that cannot be the start of any token
    InvalidByteForTokenStart { byte: u8 },
    /// During parsing of a unicode escape, the provided code point is empty
    MissingCodePoint,
    /// During parsing of a unicode escape, the provided code point is not valid
    InvalidCodePoint { code_point: u32 },
    /// During parsing of a string, an invalid escape code was encountered
    UnknownEscapeSequence { escape_sequence: u8 },
    /// During parsing of a `let`, a `set`, or a function header, the same identifier was declared
    /// multiple times
    DuplicateIdentifier {
        previous_declaration: Spanned<StrId>,
    },
    /// During evaluation, an unexpected expression kind was encountered
    UnexpectedExprKind {
        expected: &'static [ExprKind],
        encountered: ExprKind,
    },
    /// During string interpolation, a number to be interpolated was not an integer
    CannotStringifyNonInteger,
    /// An attempt was made to select a field from a map but the map has not such field
    MissingMapField { field_name: StrId },
    /// An attempt was made to select index into a list but the index was out of bounds
    MissingListField { index: Rc<BigRational> },
    /// During evaluation, an expression was encountered that must be evaluated before itself
    InfiniteRecursion { expr_id: ExprId },
    /// During evaluation, an expression was encountered whose kind cannot be evaluated
    CannotEvaluateExpr { kind: ExprKind },
    /// During evaluation, a division by zero was attempted
    DivideByZero,
    /// During evaluation of a function application, the argument did not contain of the fields
    /// required by the function pattern
    MissingArgument { missing_parameter: Spanned<StrId> },
    /// The provided source code is too large to be parsed
    SpanOverflow,
    /// An assertion failed
    AssertionFailed { msg: StrId },
    /// A custom error generated by an embedder-defined native function
    Custom {
        error: Rc<dyn std::error::Error + 'static>,
    },
    /// During parsing, a token was encountered that can only appear as a pair (e.g. `{`, `}`) but
    /// it appeared unpaired
    UnmatchedToken { kind: TokenKind },
    /// During parsing, an unexpected byte was encountered
    UnexpectedByte {
        expected: &'static [u8],
        encountered: u8,
    },
    /// During evaluation, an error was raised via `std.raise`
    Raised { msg: StrId },
}

/// An alternative to a token
///
/// This type is used for diagnostic purposes when, during parsing, an invalid token is
/// encountered. The value of this type indicates which kinds of tokens were valid in the
/// position.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenAlternative {
    /// The end of the input
    EndOfInput,
    /// The start of an expression
    StartOfExpression,
    /// A list of tokens
    List { candidates: &'static [TokenKind] },
}

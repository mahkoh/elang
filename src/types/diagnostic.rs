use crate::types::{
    span::Span,
    store::StrId,
    token::{SToken, TokenType},
    tree::{Expr, ValueType},
};
use std::rc::Rc;

#[derive(Debug)]
pub enum Error {
    ParserError(ParserError),
}

impl Error {
    pub(crate) fn add_parser_context(self, ctx: ParserErrorContext) -> Self {
        match self {
            Error::ParserError(mut pe) => {
                pe.context.push(ctx);
                Error::ParserError(pe)
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub span: Span,
    pub error: ParserErrorType,
    pub context: Vec<ParserErrorContext>,
}

#[derive(Copy, Clone)]
pub enum ParserErrorContext {
    String(u32),
    UnicodeEscape(u32),
    Test(u32),
    Select(u32),
    Parenthesized(u32),
    FnHeader(u32),
    FnPattern(u32),
    Let(u32),
    Field(u32),
    Cond(u32),
    List(u32),
    Set(u32),
    Inherit(u32),
}

pub enum ParserErrorType {
    UnexpectedEndOfInput,
    UnexpectedToken(TokenAlternative, TokenType),
    OutOfBoundsLiteral,
    OutOfBoundsSelector(i64),
    UnexpectedIntegerSuffix(u8),
    UnexpectedByte(u8),
    MissingCodePoint,
    InvalidCodePoint(u32),
    UnknownEscapeSequence(u8),
    DuplicateIdentifier(StrId, Span),
}

pub enum TokenAlternative {
    EndOfInput,
    StartOfExpression,
    List(&'static [TokenType]),
}

#[non_exhaustive]
#[derive(Debug)]
pub enum MsgDetails {
    DivideByZero,
    FoundExpr(&'static str, Rc<Expr>),
    FoundToken(&'static str, SToken),
    FoundString(&'static str, &'static str),
    FoundChar(&'static str, char),
    CannotStringify(Rc<Expr>),
    InfiniteRecursion,
    OutOfBounds,
    OverflowingLiteral,
    Overflow,
    InvalidCodepoint,
    UnknownEscapeSequence(u8),

    UnexpectedValueType(&'static [ValueType], Rc<Expr>),
    CannotForceExpression(Rc<Expr>),
    MissingNewline,
    SpanOverflow,

    SetHasNoField(StrId),
    ListHasNoField(usize),
    FnPattern(StrId),
    FnMissingField(StrId),
    AssertionFailed,

    DupSetField(Span),
}

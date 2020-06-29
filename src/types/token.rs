use crate::types::{span::Spanned, store::StrId};
use std::{
    fmt,
    fmt::{Debug, Formatter},
};

/// A token with a span.
pub type SToken = Spanned<Token>;

/// A token produced by the lexer.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Token {
    StringStart,
    String(StrId),
    StringEnd,

    AmpersandAmpersand,
    At,
    BackslashBackslash,
    BarBar,
    Colon,
    Comma,
    Dot,
    Else,
    Equals,
    EqualsEquals,
    ExclamationMark,
    ExclamationMarkEquals,
    False,
    Ident(StrId),
    If,
    In,
    Inherit,
    IntSlash,
    LeftAngle,
    LeftAngleEquals,
    LeftBrace,
    LeftBracket,
    LeftParen,
    Let,
    Minus,
    Null,
    Number(StrId, u16, u16),
    Or,
    Percent,
    Plus,
    QuestionMark,
    Rec,
    RightAngle,
    RightAngleEquals,
    RightBrace,
    RightBracket,
    RightParen,
    Slash,
    Star,
    Std,
    Then,
    True,
}

/// The kind of a token
///
/// Identifiers, strings, and numbers carry associated data. The variants of this enum do
/// not carry that data.
///
/// This type is used for diagnostic purposes only.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum TokenKind {
    /// `&&`
    AmpersandAmpersand,
    /// `@`
    At,
    /// `\\`
    BackslashBackslash,
    /// `||`
    BarBar,
    /// `:`
    Colon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `else`
    Else,
    /// `=`
    Equals,
    /// `==`
    EqualsEquals,
    /// `!`
    ExclamationMark,
    /// `!=`
    ExclamationMarkEquals,
    /// `false`
    False,
    /// An identifier
    Ident,
    /// `if`
    If,
    /// `in`
    In,
    /// `inherit`
    Inherit,
    /// `int/`
    IntSlash,
    /// `<`
    LeftAngle,
    /// `<=`
    LeftAngleEquals,
    /// `{`
    LeftBrace,
    /// `[`
    LeftBracket,
    /// `(`
    LeftParen,
    /// `let`
    Let,
    /// `-`
    Minus,
    /// `null`
    Null,
    /// A number
    Number,
    /// `or`
    Or,
    /// `%`
    Percent,
    /// `+`
    Plus,
    /// `?`
    QuestionMark,
    /// `rec`
    Rec,
    /// `>`
    RightAngle,
    /// `>=`
    RightAngleEquals,
    /// `}`
    RightBrace,
    /// `]`
    RightBracket,
    /// `)`
    RightParen,
    /// `/`
    Slash,
    /// `*`
    Star,
    /// `std`
    Std,
    /// A string
    String,
    /// `then`
    Then,
    /// `true`
    True,
}

impl TokenKind {
    /// Returns the textual representation of the token kind
    ///
    /// For symbol-like tokens and keywords, this is simply their representation in source
    /// code. For example, `AmpersandAmpersand => "&&"`.
    ///
    /// There are three special cases:
    ///
    /// * `Ident => "identifier"`,
    /// * `Number => "number"`,
    /// * `String => "string"`,
    pub fn as_str(self) -> &'static str {
        match self {
            TokenKind::AmpersandAmpersand => "&&",
            TokenKind::At => "@",
            TokenKind::BackslashBackslash => r"\\",
            TokenKind::BarBar => "||",
            TokenKind::Colon => ":",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Else => "else",
            TokenKind::Equals => "=",
            TokenKind::EqualsEquals => "==",
            TokenKind::ExclamationMark => "!",
            TokenKind::ExclamationMarkEquals => "!=",
            TokenKind::False => "false",
            TokenKind::Ident => "identifier",
            TokenKind::If => "if",
            TokenKind::Inherit => "inherit",
            TokenKind::In => "in",
            TokenKind::IntSlash => "int/",
            TokenKind::LeftAngle => "<",
            TokenKind::LeftAngleEquals => "<=",
            TokenKind::LeftBrace => "{",
            TokenKind::LeftBracket => "[",
            TokenKind::LeftParen => "(",
            TokenKind::Let => "let",
            TokenKind::Minus => "-",
            TokenKind::Null => "null",
            TokenKind::Number => "number",
            TokenKind::Or => "or",
            TokenKind::Percent => "%",
            TokenKind::Plus => "+",
            TokenKind::QuestionMark => "?",
            TokenKind::Rec => "rec",
            TokenKind::RightAngle => ">",
            TokenKind::RightAngleEquals => ">=",
            TokenKind::RightBrace => "}",
            TokenKind::RightBracket => "]",
            TokenKind::RightParen => ")",
            TokenKind::Slash => "/",
            TokenKind::Star => "*",
            TokenKind::Std => "std",
            TokenKind::String => "string",
            TokenKind::Then => "then",
            TokenKind::True => "true",
        }
    }
}

impl Token {
    /// Returns whether this token starts an expression
    ///
    /// We use this to find out if an expression has just ended. If a token does not start
    /// an expression, then it combines a subsequent token with the current expression.
    /// E.g., in
    ///
    /// ```elang
    /// 1 + 1
    /// ```
    ///
    /// the `+` does not start a new expression. However, in
    ///
    /// ```elang
    /// f x + 1
    /// ```
    ///
    /// the `x` (an identifier) starts a new expression and this will be parsed as a
    /// function application: `f(x + 1)`.
    pub fn starts_expr(self) -> bool {
        match self {
            Token::StringStart => true,
            Token::Number(..) => true,
            Token::Ident(..) => true,
            Token::True => true,
            Token::False => true,
            Token::Null => true,
            Token::Rec => true,
            Token::Let => true,
            Token::If => true,
            Token::LeftBracket => true,
            Token::LeftBrace => true,
            Token::LeftParen => true,
            Token::Std => true,

            // Unary operators
            Token::Minus => true,
            Token::ExclamationMark => true,

            Token::String(..) => false,
            Token::StringEnd => false,
            Token::In => false,
            Token::Inherit => false,
            Token::Then => false,
            Token::Else => false,
            Token::Or => false,
            Token::RightBracket => false,
            Token::RightBrace => false,
            Token::RightParen => false,
            Token::Dot => false,
            Token::Equals => false,
            Token::Colon => false,
            Token::Comma => false,
            Token::QuestionMark => false,
            Token::At => false,
            Token::EqualsEquals => false,
            Token::ExclamationMarkEquals => false,
            Token::LeftAngle => false,
            Token::LeftAngleEquals => false,
            Token::RightAngle => false,
            Token::RightAngleEquals => false,
            Token::Plus => false,
            Token::Star => false,
            Token::Slash => false,
            Token::Percent => false,
            Token::BackslashBackslash => false,
            Token::AmpersandAmpersand => false,
            Token::BarBar => false,
            Token::IntSlash => false,
        }
    }

    pub fn kind(self) -> TokenKind {
        match self {
            Token::AmpersandAmpersand => TokenKind::AmpersandAmpersand,
            Token::At => TokenKind::At,
            Token::BackslashBackslash => TokenKind::BackslashBackslash,
            Token::BarBar => TokenKind::BarBar,
            Token::Colon => TokenKind::Colon,
            Token::Comma => TokenKind::Comma,
            Token::Dot => TokenKind::Dot,
            Token::Else => TokenKind::Else,
            Token::EqualsEquals => TokenKind::EqualsEquals,
            Token::Equals => TokenKind::Equals,
            Token::ExclamationMarkEquals => TokenKind::ExclamationMarkEquals,
            Token::ExclamationMark => TokenKind::ExclamationMark,
            Token::False => TokenKind::False,
            Token::Ident(..) => TokenKind::Ident,
            Token::If => TokenKind::If,
            Token::Inherit => TokenKind::Inherit,
            Token::In => TokenKind::In,
            Token::IntSlash => TokenKind::IntSlash,
            Token::LeftAngleEquals => TokenKind::LeftAngleEquals,
            Token::LeftAngle => TokenKind::LeftAngle,
            Token::LeftBrace => TokenKind::LeftBrace,
            Token::LeftBracket => TokenKind::LeftBracket,
            Token::LeftParen => TokenKind::LeftParen,
            Token::Let => TokenKind::Let,
            Token::Minus => TokenKind::Minus,
            Token::Null => TokenKind::Null,
            Token::Number(..) => TokenKind::Number,
            Token::Or => TokenKind::Or,
            Token::Percent => TokenKind::Percent,
            Token::Plus => TokenKind::Plus,
            Token::QuestionMark => TokenKind::QuestionMark,
            Token::Rec => TokenKind::Rec,
            Token::RightAngleEquals => TokenKind::RightAngleEquals,
            Token::RightAngle => TokenKind::RightAngle,
            Token::RightBrace => TokenKind::RightBrace,
            Token::RightBracket => TokenKind::RightBracket,
            Token::RightParen => TokenKind::RightParen,
            Token::Slash => TokenKind::Slash,
            Token::Star => TokenKind::Star,
            Token::Std => TokenKind::Std,
            Token::StringStart => TokenKind::String,
            Token::String(..) => TokenKind::String,
            Token::StringEnd => TokenKind::String,
            Token::Then => TokenKind::Then,
            Token::True => TokenKind::True,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind().as_str())
    }
}

impl Debug for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

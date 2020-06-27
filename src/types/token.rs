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
    /// A non-terminating part of a string that contains an expression interpolation.
    ///
    /// = Remarks
    ///
    /// This looks like this in the source code:
    ///
    /// ```txt
    /// "aaaaaaaaaaaa\{e1}bbbbbbbbb\{e2}cccccccc"
    /// \____   ____/     \__   __/     \__   __/
    ///      \ /             \ /           \ /
    ///   StringPart      StringPart      String
    /// ```
    ///
    /// When the parser gets a `StringPart` from the lexer, it parses a single expression
    /// (`e1` or `e2` above), eats a `}`, and then tells the lexer to continue parsing the
    /// string.
    StringPart(StrId),
    String(StrId),

    AmpersandAmpersand,
    At,
    BackslashBackslash,
    BarBar,
    Colon,
    Comma,
    Dot,
    DotDot,
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
    IntPercent,
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
    PlusPlus,
    QuestionMark,
    Rec,
    RightAngle,
    RightAngleEquals,
    RightBrace,
    RightBracket,
    RightParen,
    Slash,
    Star,
    Then,
    True,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum TokenType {
    AmpersandAmpersand,
    At,
    BackslashBackslash,
    BarBar,
    Colon,
    Comma,
    Dot,
    DotDot,
    Else,
    Equals,
    EqualsEquals,
    ExclamationMark,
    ExclamationMarkEquals,
    False,
    Ident,
    If,
    In,
    Inherit,
    IntPercent,
    IntSlash,
    LeftAngle,
    LeftAngleEquals,
    LeftBrace,
    LeftBracket,
    LeftParen,
    Let,
    Minus,
    Null,
    Number,
    Or,
    Percent,
    Plus,
    PlusPlus,
    QuestionMark,
    Rec,
    RightAngle,
    RightAngleEquals,
    RightBrace,
    RightBracket,
    RightParen,
    Slash,
    Star,
    String,
    Then,
    True,
}

impl TokenType {
    pub fn as_str(self) -> &'static str {
        match self {
            TokenType::AmpersandAmpersand => "&&",
            TokenType::At => "@",
            TokenType::BackslashBackslash => r"\\",
            TokenType::BarBar => "||",
            TokenType::Colon => ":",
            TokenType::Comma => ",",
            TokenType::Dot => ".",
            TokenType::DotDot => "..",
            TokenType::Else => "else",
            TokenType::Equals => "=",
            TokenType::EqualsEquals => "==",
            TokenType::ExclamationMark => "!",
            TokenType::ExclamationMarkEquals => "!=",
            TokenType::False => "false",
            TokenType::Ident => "identifier",
            TokenType::If => "if",
            TokenType::Inherit => "inherit",
            TokenType::In => "in",
            TokenType::IntPercent => "int%",
            TokenType::IntSlash => "int/",
            TokenType::LeftAngle => "<",
            TokenType::LeftAngleEquals => "<=",
            TokenType::LeftBrace => "{",
            TokenType::LeftBracket => "[",
            TokenType::LeftParen => "(",
            TokenType::Let => "let",
            TokenType::Minus => "-",
            TokenType::Null => "null",
            TokenType::Number => "number",
            TokenType::Or => "or",
            TokenType::Percent => "%",
            TokenType::Plus => "+",
            TokenType::PlusPlus => "++",
            TokenType::QuestionMark => "?",
            TokenType::Rec => "rec",
            TokenType::RightAngle => ">",
            TokenType::RightAngleEquals => ">=",
            TokenType::RightBrace => "}",
            TokenType::RightBracket => "]",
            TokenType::RightParen => ")",
            TokenType::Slash => "/",
            TokenType::Star => "*",
            TokenType::String => "string",
            TokenType::Then => "then",
            TokenType::True => "true",
        }
    }
}

impl Token {
    /// Returns whether this token starts an expression.
    ///
    /// = Remarks
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
    /// the `x` (an identifier) starts a new expressiond and this will be parsed as a
    /// function application: `f(x + 1)`.
    pub fn starts_expr(self) -> bool {
        match self {
            Token::StringPart(..) => true,
            Token::String(..) => true,
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

            // Unary operators
            Token::Minus => true,
            Token::ExclamationMark => true,

            Token::In => false,
            Token::Inherit => false,
            Token::Then => false,
            Token::Else => false,
            Token::Or => false,
            Token::RightBracket => false,
            Token::RightBrace => false,
            Token::RightParen => false,
            Token::Dot => false,
            Token::DotDot => false,
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
            Token::PlusPlus => false,
            Token::Plus => false,
            Token::Star => false,
            Token::Slash => false,
            Token::Percent => false,
            Token::BackslashBackslash => false,
            Token::AmpersandAmpersand => false,
            Token::BarBar => false,
            Token::IntSlash => false,
            Token::IntPercent => false,
        }
    }

    pub fn ty(self) -> TokenType {
        match self {
            Token::AmpersandAmpersand => TokenType::AmpersandAmpersand,
            Token::At => TokenType::At,
            Token::BackslashBackslash => TokenType::BackslashBackslash,
            Token::BarBar => TokenType::BarBar,
            Token::Colon => TokenType::Colon,
            Token::Comma => TokenType::Comma,
            Token::DotDot => TokenType::DotDot,
            Token::Dot => TokenType::Dot,
            Token::Else => TokenType::Else,
            Token::EqualsEquals => TokenType::EqualsEquals,
            Token::Equals => TokenType::Equals,
            Token::ExclamationMarkEquals => TokenType::ExclamationMarkEquals,
            Token::ExclamationMark => TokenType::ExclamationMark,
            Token::False => TokenType::False,
            Token::Ident(..) => TokenType::Ident,
            Token::If => TokenType::If,
            Token::Inherit => TokenType::Inherit,
            Token::In => TokenType::In,
            Token::IntPercent => TokenType::IntPercent,
            Token::IntSlash => TokenType::IntSlash,
            Token::LeftAngleEquals => TokenType::LeftAngleEquals,
            Token::LeftAngle => TokenType::LeftAngle,
            Token::LeftBrace => TokenType::LeftBrace,
            Token::LeftBracket => TokenType::LeftBracket,
            Token::LeftParen => TokenType::LeftParen,
            Token::Let => TokenType::Let,
            Token::Minus => TokenType::Minus,
            Token::Null => TokenType::Null,
            Token::Number(..) => TokenType::Number,
            Token::Or => TokenType::Or,
            Token::Percent => TokenType::Percent,
            Token::PlusPlus => TokenType::PlusPlus,
            Token::Plus => TokenType::Plus,
            Token::QuestionMark => TokenType::QuestionMark,
            Token::Rec => TokenType::Rec,
            Token::RightAngleEquals => TokenType::RightAngleEquals,
            Token::RightAngle => TokenType::RightAngle,
            Token::RightBrace => TokenType::RightBrace,
            Token::RightBracket => TokenType::RightBracket,
            Token::RightParen => TokenType::RightParen,
            Token::Slash => TokenType::Slash,
            Token::Star => TokenType::Star,
            Token::StringPart(..) => TokenType::String,
            Token::String(..) => TokenType::String,
            Token::Then => TokenType::Then,
            Token::True => TokenType::True,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty().as_str())
    }
}

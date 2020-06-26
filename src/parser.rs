use crate::{
    lexer::Lexer,
    types::{
        diagnostic::{Error, ErrorContext, ErrorType, TokenAlternative},
        op::Op,
        result::{Result, ResultUtil},
        span::{Span, Spanned},
        stack::Stack,
        store::StrId,
        token::{Token, TokenType},
        tree::{ExprId, ExprType, FnParam, FnType, SExpr},
    },
};
use num_bigint::BigInt;
use num_rational::BigRational;
use std::{
    collections::{hash_map::Entry, HashMap},
    rc::Rc,
};

/// An expression parser.
pub struct Parser<'a, 'b> {
    lexer: Lexer<'a, 'b>,
}

impl<'a, 'b> Parser<'a, 'b> {
    /// Creates a new parser.
    ///
    /// [argument, lexer]
    /// The lexer from which tokens will be read.
    ///
    /// [argument, diagnostic]
    /// The destination for diagnostic messages.
    pub fn new(lexer: Lexer<'a, 'b>) -> Self {
        Parser { lexer }
    }

    /// Parses the content of the parser.
    ///
    /// = Remarks
    ///
    /// This succeeds if the whole content of the lexer is a single valid expression.
    pub fn parse(&mut self) -> Result<ExprId> {
        let expr = self.parse_expr()?;
        if let Some(t) = self.lexer.try_next()? {
            return self.error(
                t.span,
                ErrorType::UnexpectedToken(TokenAlternative::EndOfInput, t.ty()),
            );
        }
        Ok(expr.val)
    }

    fn spanned(&mut self, span: Span, expr: ExprType) -> SExpr {
        Spanned::new(span, self.lexer.store.add_expr(span, expr))
    }

    /// Parses an expression.
    fn parse_expr(&mut self) -> Result<SExpr> {
        let mut stack = Stack::new();

        'outer: loop {
            // Step 1: Check for unary operators.
            loop {
                let cur = self.lexer.peek(0)?;
                if match cur.val {
                    Token::Not => {
                        stack.push_op(&mut self.lexer.store, Op::Not(cur.span.lo));
                        true
                    }
                    Token::Minus => {
                        stack.push_op(&mut self.lexer.store, Op::UnMin(cur.span.lo));
                        true
                    }
                    _ => false,
                } {
                    self.lexer.skip(1);
                } else {
                    break;
                }
            }

            // Step 2: Read an atomic expression.
            stack.push_expr(self.parse_atomic()?);

            // Step 3: Read an operator.
            //
            // This is a loop because of the two special cases below which are effectively
            // unary postfix operators. Those are followed by an operator or end the
            // expression.
            loop {
                if self.lexer.eof()? {
                    break 'outer;
                }

                let next = self.lexer.peek(0)?;
                let op = match next.val {
                    Token::Implies => Op::Impl,
                    Token::OrOr => Op::Or,
                    Token::AndAnd => Op::And,
                    Token::Le => Op::Le,
                    Token::Ge => Op::Ge,
                    Token::Lt => Op::Lt,
                    Token::Gt => Op::Gt,
                    Token::Equals => Op::Eq,
                    Token::Unequal => Op::Ne,
                    Token::Overlay => Op::Overlay,
                    Token::Plus => Op::Add,
                    Token::Minus => Op::Sub,
                    Token::Times => Op::Mul,
                    Token::Div => Op::Div,
                    Token::Mod => Op::Mod,
                    Token::Concat => Op::Concat,
                    Token::Questionmark => Op::Test,
                    Token::Dot => Op::Select,
                    t if t.starts_expr() => Op::Apl,
                    _ => break 'outer,
                };

                if op != Op::Apl {
                    // Apl is not a token.

                    self.lexer.skip(1);
                }

                if op == Op::Test {
                    // Test has the form
                    //
                    // ----
                    // e ? i1.i2.i3
                    // ----

                    stack.next_op(&mut self.lexer.store, op);
                    let expr = stack.pop_expr();
                    let path = self
                        .parse_attr_path()
                        .ctx(ErrorContext::ParseTest(next.span.lo))?;
                    let span = Span::new(expr.span.lo, path.span.hi);
                    let expr = self.spanned(span, ExprType::Test(expr.val, path.val));
                    stack.push_expr(expr);
                } else if op == Op::Select {
                    // Select has the form
                    //
                    // ----
                    // e.i1.i2.i3
                    // ----
                    //
                    // or
                    //
                    // ----
                    // e1.i1.i2.i3 or e2
                    // ----

                    stack.next_op(&mut self.lexer.store, op);
                    let expr = stack.pop_expr();
                    let path = self
                        .parse_attr_path()
                        .ctx(ErrorContext::ParseSelect(next.span.lo))?;
                    let (hi, alt) = match self.lexer.try_peek(0)? {
                        Some(Spanned { val: Token::Or, .. }) => {
                            self.lexer.skip(1);
                            let alt = self.parse_expr()?;
                            (alt.span.hi, Some(alt.val))
                        }
                        _ => (path.span.hi, None),
                    };
                    let span = Span::new(expr.span.lo, hi);
                    let expr = ExprType::Select(expr.val, path.val, alt);
                    let expr = self.spanned(span, expr);
                    stack.push_expr(expr);
                } else {
                    stack.push_op(&mut self.lexer.store, op);
                    break;
                }
            }
        }

        Ok(stack.clear(&mut self.lexer.store))
    }

    /// Parses an atomic expression.
    ///
    /// = Remarks
    ///
    /// An atomic expression is one that can be recognized with a statically known amount
    /// of lookahead. For example, an Integer can be recognized with 1 token lookahead.
    ///
    /// `e1 || e2` is not an atomic expression since `e1` can consist of arbitrarily many
    /// tokens. Neither are expressions starting with the unary `-` and `!` operators
    /// since they are subject to precedence considerations.
    ///
    /// == Atomic expressions
    ///
    /// * Function
    /// * Number
    /// * Ident
    /// * Bool
    /// * Null
    /// * (recursive) Set
    /// * List
    /// * Let binding
    /// * Conditional
    /// * `(expr)`
    fn parse_atomic(&mut self) -> Result<SExpr> {
        let token = self.lexer.peek(0)?;

        if !token.val.starts_expr() {
            return self.error(
                token.span,
                ErrorType::UnexpectedToken(
                    TokenAlternative::StartOfExpression,
                    token.ty(),
                ),
            );
        }

        // Check if this is a function definition.
        match token.val {
            Token::Ident(..) => {
                match self.lexer.try_peek(1)? {
                    // ident:
                    Some(Spanned {
                        val: Token::Colon, ..
                    }) => return self.parse_fn(),
                    // ident @
                    Some(Spanned { val: Token::At, .. }) => return self.parse_fn(),
                    _ => {}
                }
            }
            Token::LeftBrace => {
                if let Some(one) = self.lexer.try_peek(1)? {
                    if one.val == Token::DotDot {
                        // { ..            }
                        return self.parse_fn();
                    } else if let Token::Ident(..) = one.val {
                        if let Some(two) = self.lexer.try_peek(2)? {
                            if two.val == Token::Comma
                                || two.val == Token::Questionmark
                                || two.val == Token::RightBrace
                            {
                                // { ident,                }
                                // { ident ?               }
                                // { ident }
                                return self.parse_fn();
                            }
                        }
                    }
                }
            }
            _ => {}
        }

        match token.val {
            Token::Number(..)
            | Token::Ident(..)
            | Token::True
            | Token::False
            | Token::Null
            | Token::String(..) => self.parse_simple(),
            Token::StringPart(..) => self.parse_string_part(),
            Token::Rec => self.parse_set(),
            Token::Let => self.parse_let(),
            Token::If => self.parse_conditional(),
            Token::LeftBracket => self.parse_list(),
            Token::LeftBrace => self.parse_set(),
            Token::LeftParen => {
                let opening = self.lexer.next().unwrap();
                let expr = self.parse_expr()?;
                let closing = match self.lexer.next_right_paren() {
                    Ok(c) => c,
                    Err(e) => {
                        return Err(e.add_context(ErrorContext::ParseParenthesized(
                            token.span.lo,
                        )));
                    }
                };
                let expr_ = self.lexer.store.get_expr(expr.val).val.borrow().clone();
                let span = Span::new(opening.span.lo, closing.span.hi);
                Ok(self.spanned(span, expr_))
            }
            _ => unreachable!(),
        }
    }

    fn parse_string_part(&mut self) -> Result<SExpr> {
        let token = self.lexer.next()?;
        let ctx = ErrorContext::ParseString(token.span.lo);
        let mut expr = match token.val {
            Token::StringPart(s) => self.spanned(token.span, ExprType::String(s)),
            _ => unreachable!(),
        };
        let mut end = false;
        while !end {
            let tmp = self.parse_expr()?;
            let span = Span::new(expr.span.lo, tmp.span.hi);
            let sfy = self.spanned(tmp.span, ExprType::Stringify(tmp.val));
            expr = self.spanned(span, ExprType::Concat(expr.val, sfy.val));

            let tmp = self.lexer.string(true).ctx(ctx)?;
            let id = match tmp.val {
                Token::StringPart(id) => id,
                Token::String(id) => {
                    end = true;
                    id
                }
                _ => unreachable!(),
            };
            let tmp = self.spanned(tmp.span, ExprType::String(id));
            let span = Span::new(expr.span.lo, tmp.span.hi);
            expr = self.spanned(span, ExprType::Concat(expr.val, tmp.val));
        }
        Ok(expr)
    }

    fn parse_number(&mut self, val: StrId, base: u16, shift: u16) -> Rc<BigRational> {
        let val = self.lexer.store.get_str(val);
        let numer = BigInt::parse_bytes(&val, base as u32).unwrap();
        let denom = BigInt::from(base).pow(shift as u32);
        Rc::new(BigRational::new(numer, denom))
    }

    fn parse_selector(&mut self) -> Result<SExpr> {
        let next = self.lexer.next()?;
        let mut span = next.span;
        let sel = match next.val {
            Token::Ident(i) => ExprType::String(i),
            Token::Number(val, base, shift) => {
                let num = self.parse_number(val, base, shift);
                ExprType::Number(num)
            }
            Token::LeftParen => {
                let expr = self.parse_expr()?;
                let ctx = ErrorContext::ParseParenthesized(next.span.lo);
                span.hi = self.lexer.next_right_paren().ctx(ctx)?.span.hi;
                ExprType::Resolved(None, expr.val)
            }
            _ => {
                return self.error(
                    next.span,
                    ErrorType::UnexpectedToken(
                        TokenAlternative::List(&[
                            TokenType::Number,
                            TokenType::Ident,
                            TokenType::LeftParen,
                        ]),
                        next.ty(),
                    ),
                );
            }
        };
        Ok(self.spanned(span, sel))
    }

    /// Parses an attribute path.
    ///
    /// = Remarks
    ///
    /// An attribute path has the following form:
    ///
    /// ----
    /// i1.i2.i3
    /// ----
    fn parse_attr_path(&mut self) -> Result<SExpr> {
        let mut path = Vec::new();
        let first = self.parse_selector()?;
        let mut last = first;
        path.push(first.val);
        while let Some(next) = self.lexer.try_peek(0)? {
            match next.val {
                Token::Dot => self.lexer.next()?,
                _ => break,
            };
            last = self.parse_selector()?;
            path.push(last.val);
        }
        path.shrink_to_fit();
        let span = Span::new(first.span.lo, last.span.hi);
        let expr = ExprType::Path(Rc::from(path.into_boxed_slice()));
        Ok(self.spanned(span, expr))
    }

    /// Parses a function.
    ///
    /// = Remarks
    ///
    /// The lexer should be at the start of a function when this function is invoked.
    ///
    /// Functions have the following forms:
    ///
    /// ----
    /// i: e
    /// ----
    ///
    /// ----
    /// { i1 ? e1, i2 ? e2, }: e3
    /// ----
    ///
    /// Where the `? e` parts and the last comma are optional.
    ///
    /// ----
    /// i @ { }: e
    /// ----
    ///
    /// Where the body of the `{ }` is as above.
    fn parse_fn(&mut self) -> Result<SExpr> {
        let first = self.lexer.peek(0).unwrap();
        let ctx = ErrorContext::ParseFnHeader(first.span.lo);

        if let Token::Ident(param_name) = first.val {
            self.lexer.skip(1);
            let second = self.lexer.next().unreachable();

            // First case above.
            if second.val == Token::Colon {
                let body = self.parse_expr()?;
                let arg = FnParam::Ident { param_name };
                let span = Span::new(first.span.lo, body.span.hi);
                let expr =
                    ExprType::Fn(FnType::Normal { param: Spanned::new(first.span, arg), body: body.val });
                return Ok(self.spanned(span, expr));
            }

            // Third case above.
            if second.val == Token::At {
                let (pat_span, fields, wild) = self.parse_fn_pat().ctx(ctx)?;
                if let Some((&spanned, _)) = fields.get_key_value(&param_name) {
                    return self.error(
                        spanned.span,
                        ErrorType::DuplicateIdentifier(Spanned::new(first.span, param_name)),
                    );
                }
                self.lexer.next_colon().ctx(ctx)?;
                let body = self.parse_expr()?;
                let arg_span = Span::new(first.span.lo, pat_span.hi);
                let param_name = Some(Spanned::new(first.span, param_name));
                let arg = FnParam::Pat {
                    param_name,
                    fields,
                    wild,
                };
                let span = Span::new(first.span.lo, body.span.hi);
                let expr =
                    ExprType::Fn(FnType::Normal { param: Spanned::new(arg_span, arg), body: body.val });
                return Ok(self.spanned(span, expr));
            }

            unreachable!();
        }

        // Second case above.
        if first.val == Token::LeftBrace {
            let (pat_span, fields, wild) = self.parse_fn_pat().ctx(ctx)?;
            self.lexer.next_colon().ctx(ctx)?;
            let body = self.parse_expr()?;
            let arg = FnParam::Pat {
                param_name: None,
                fields,
                wild,
            };
            let span = Span::new(pat_span.lo, body.span.hi);
            let expr =
                ExprType::Fn(FnType::Normal { param: Spanned::new(pat_span, arg), body: body.val });
            return Ok(self.spanned(span, expr));
        }

        unreachable!();
    }

    /// Parses a function argument pattern.
    ///
    /// [return_value]
    /// Returns (the span of the pattern, the bound variables and optional alternatives,
    /// whether the pattern has a wildcard).
    ///
    /// = Remarks
    ///
    /// Patterns have the following form.
    ///
    /// ----
    /// { i1 ? e1, i2 ? e2, .. }
    /// ----
    #[allow(clippy::type_complexity)]
    fn parse_fn_pat(
        &mut self,
    ) -> Result<(Span, Rc<HashMap<Spanned<StrId>, Option<ExprId>>>, bool)> {
        let opening = self.lexer.next().unwrap();
        let ctx = ErrorContext::ParseFnPattern(opening.span.lo);
        let mut vars = HashMap::<_, _>::new();
        let mut wild = false;

        loop {
            let ident = self.lexer.peek(0).ctx(ctx)?;
            let (ident, span) = match ident.val {
                Token::Ident(i) => {
                    self.lexer.skip(1);
                    (i, ident.span)
                }
                Token::DotDot => {
                    self.lexer.skip(1);
                    wild = true;
                    break;
                }
                _ => {
                    return self
                        .error(
                            ident.span,
                            ErrorType::UnexpectedToken(
                                TokenAlternative::List(&[
                                    TokenType::Ident,
                                    TokenType::DotDot,
                                ]),
                                ident.ty(),
                            ),
                        )
                        .ctx(ctx);
                }
            };

            let next = self
                .lexer
                .peek(0)
                .ctx(ErrorContext::ParseField(span.lo))
                .ctx(ctx)?;
            let alt = match next.val {
                Token::Questionmark => {
                    self.lexer.skip(1);
                    Some(self.parse_expr()?.val)
                }
                _ => None,
            };

            match vars.entry(Spanned::new(span, ident)) {
                Entry::Occupied(e) => {
                    return self
                        .error(span, ErrorType::DuplicateIdentifier(*e.key()))
                        .ctx(ctx);
                }
                Entry::Vacant(e) => e.insert(alt),
            };

            let next = self.lexer.peek(0).ctx(ctx)?;
            match next.val {
                Token::Comma => self.lexer.skip(1),
                Token::RightBrace => break,
                _ => {
                    return self
                        .error(
                            next.span,
                            ErrorType::UnexpectedToken(
                                TokenAlternative::List(&[
                                    TokenType::Comma,
                                    TokenType::RightBrace,
                                ]),
                                next.ty(),
                            ),
                        )
                        .ctx(ctx);
                }
            };
        }

        vars.shrink_to_fit();
        let closing = self.lexer.next_right_brace().ctx(ctx)?;
        let span = Span::new(opening.span.lo, closing.span.hi);
        Ok((span, Rc::new(vars), wild))
    }

    /// Parses a simple expression.
    ///
    /// = Remarks
    ///
    /// Simple expressions are those that consist of a single token. The lexer should
    /// currently be at one of those tokens or the process will be aborted.
    ///
    /// Simple expressions are
    ///
    /// * Numbers
    /// * Identifiers
    /// * Booleans
    /// * Null
    fn parse_simple(&mut self) -> Result<SExpr> {
        let t = self.lexer.next().unwrap();
        let expr = match t.val {
            Token::Number(val, base, shift) => {
                ExprType::Number(self.parse_number(val, base, shift))
            }
            Token::Ident(i) => ExprType::Ident(i),
            Token::True => ExprType::Bool(true),
            Token::False => ExprType::Bool(false),
            Token::Null => ExprType::Null,
            Token::String(s) => ExprType::String(s),
            _ => unreachable!(),
        };
        Ok(self.spanned(t.span, expr))
    }

    /// Parses a let binding.
    ///
    /// = Remarks
    ///
    /// When this function is invoked, the next token in the lexer should be `let`.
    ///
    /// A let binding has the following form:
    ///
    /// ----
    /// let
    ///     i1 = e1,
    ///     i2 = e2,
    /// in
    ///     e3
    /// ----
    fn parse_let(&mut self) -> Result<SExpr> {
        let let_ = self.lexer.next().unwrap();
        let ctx = ErrorContext::ParseLet(let_.span.lo);
        let mut bindings = HashMap::<_, _>::new();
        loop {
            if self.lexer.peek(0).ctx(ctx)?.val == Token::In {
                self.lexer.skip(1);
                break;
            }
            let (span, name) = self.lexer.next_ident().ctx(ctx)?;
            let ictx = ErrorContext::ParseField(span.span.lo);
            self.lexer.next_assign().ctx(ictx)?;
            let expr = self.parse_expr()?;
            let next = self.lexer.peek(0).ctx(ctx)?;
            match next.val {
                Token::In => {}
                Token::Comma => self.lexer.skip(1),
                _ => {
                    return self
                        .error(
                            next.span,
                            ErrorType::UnexpectedToken(
                                TokenAlternative::List(&[
                                    TokenType::In,
                                    TokenType::Comma,
                                ]),
                                next.ty(),
                            ),
                        )
                        .ctx(ctx);
                }
            }
            match bindings.entry(Spanned::new(span.span, name)) {
                Entry::Occupied(e) => {
                    return self
                        .error(span.span, ErrorType::DuplicateIdentifier(*e.key()))
                        .ctx(ctx);
                }
                Entry::Vacant(e) => e.insert(expr.val),
            };
        }
        bindings.shrink_to_fit();
        let expr = self.parse_expr()?;
        let span = Span::new(let_.span.lo, expr.span.hi);
        let expr = ExprType::Let(Rc::new(bindings), expr.val);
        Ok(self.spanned(span, expr))
    }

    /// Parses a conditional.
    ///
    /// = Remarks
    ///
    /// When this function is invoked, the next token in the lexer should be `if`.
    ///
    /// A conditional has the following form:
    ///
    /// ----
    /// if
    ///     e1
    /// then
    ///     e2
    /// else
    ///     e3
    /// ----
    fn parse_conditional(&mut self) -> Result<SExpr> {
        let if_ = self.lexer.next().unwrap();
        let ctx = ErrorContext::ParseCond(if_.span.lo);
        let e1 = self.parse_expr()?;
        self.lexer.next_then().ctx(ctx)?;
        let e2 = self.parse_expr()?;
        self.lexer.next_else().ctx(ctx)?;
        let e3 = self.parse_expr()?;
        let span = Span::new(if_.span.lo, e3.span.hi);
        Ok(self.spanned(span, ExprType::Cond(e1.val, e2.val, e3.val)))
    }

    /// Parses a list.
    ///
    /// = Remarks
    ///
    /// When this function is invoked, the next token in the lexer should be `[`.
    ///
    /// A list has the following form:
    ///
    /// ----
    /// [
    ///     e1,
    ///     e2,
    ///     e3,
    /// ]
    /// ----
    ///
    /// The last comma is optional.
    fn parse_list(&mut self) -> Result<SExpr> {
        let start = self.lexer.next().unwrap();
        let ctx = ErrorContext::ParseList(start.span.lo);
        let mut els = Vec::new();
        loop {
            if self.lexer.peek(0).ctx(ctx)?.val == Token::RightBracket {
                break;
            }
            els.push(self.parse_expr()?.val);
            let next = self.lexer.peek(0).ctx(ctx)?;
            match next.val {
                Token::Comma => self.lexer.skip(1),
                Token::RightBracket => break,
                _ => {
                    return self
                        .error(
                            next.span,
                            ErrorType::UnexpectedToken(
                                TokenAlternative::List(&[
                                    TokenType::Comma,
                                    TokenType::RightBracket,
                                ]),
                                next.ty(),
                            ),
                        )
                        .ctx(ctx);
                }
            };
        }
        let end = self.lexer.next().unwrap();
        let span = Span::new(start.span.lo, end.span.hi);
        Ok(self.spanned(span, ExprType::List(Rc::from(els.into_boxed_slice()))))
    }

    /// Parses a set.
    ///
    /// = Remarks
    ///
    /// When this function is invoked, the next token in the lexer should be `rec` or `{`.
    ///
    /// A set has the following form:
    ///
    /// ----
    /// rec # optional
    /// {
    ///     i1 = e1,
    ///     i2 = e2,
    /// }
    /// ----
    ///
    /// The last comma is optional.
    fn parse_set(&mut self) -> Result<SExpr> {
        let opening = self.lexer.next().unwrap();
        let ctx = ErrorContext::ParseSet(opening.span.lo);
        let rec = match opening.val {
            Token::Rec => {
                self.lexer.next_left_brace().ctx(ctx)?;
                true
            }
            _ => false,
        };
        let mut fields = HashMap::new();
        loop {
            if self.lexer.peek(0).ctx(ctx)?.val == Token::RightBrace {
                break;
            }

            let next = self.lexer.next()?;
            match next.val {
                Token::Ident(ident) => {
                    self.lexer
                        .next_assign()
                        .ctx(ErrorContext::ParseField(next.span.lo))?;
                    let expr = self.parse_expr()?;
                    match fields.entry(Spanned::new(next.span, ident)) {
                        Entry::Occupied(e) => {
                            return self
                                .error(
                                    next.span,
                                    ErrorType::DuplicateIdentifier(*e.key()),
                                )
                                .ctx(ctx);
                        }
                        Entry::Vacant(e) => e.insert(expr.val),
                    };
                }
                Token::Inherit => loop {
                    let el = self
                        .lexer
                        .peek(0)
                        .ctx(ErrorContext::ParseInherit(next.span.lo))?;
                    match el.val {
                        Token::Comma | Token::RightBrace => break,
                        Token::Ident(ident) => {
                            self.lexer.skip(1);
                            match fields.entry(Spanned::new(el.span, ident)) {
                                Entry::Occupied(e) => {
                                    return self
                                        .error(
                                            el.span,
                                            ErrorType::DuplicateIdentifier(*e.key()),
                                        )
                                        .ctx(ctx);
                                }
                                Entry::Vacant(e) => {
                                    let ex = self.spanned(el.span, ExprType::Inherit);
                                    e.insert(ex.val);
                                }
                            };
                        }
                        _ => {
                            return self
                                .error(
                                    el.span,
                                    ErrorType::UnexpectedToken(
                                        TokenAlternative::List(&[
                                            TokenType::Ident,
                                            TokenType::Comma,
                                            TokenType::RightBrace,
                                        ]),
                                        el.ty(),
                                    ),
                                )
                                .ctx(ErrorContext::ParseInherit(next.span.lo));
                        }
                    }
                },
                _ => {
                    return self
                        .error(
                            next.span,
                            ErrorType::UnexpectedToken(
                                TokenAlternative::List(&[
                                    TokenType::Ident,
                                    TokenType::Inherit,
                                    TokenType::RightBrace,
                                ]),
                                next.ty(),
                            ),
                        )
                        .ctx(ctx);
                }
            }

            let next = self.lexer.peek(0).ctx(ctx)?;
            match next.val {
                Token::Comma => self.lexer.skip(1),
                Token::RightBrace => break,
                _ => {
                    return self
                        .error(
                            next.span,
                            ErrorType::UnexpectedToken(
                                TokenAlternative::List(&[
                                    TokenType::Comma,
                                    TokenType::RightBrace,
                                ]),
                                next.ty(),
                            ),
                        )
                        .ctx(ctx);
                }
            };
        }
        let closing = match self.lexer.next_right_brace() {
            Ok(c) => c,
            _ => unreachable!(),
        };
        let span = Span::new(opening.span.lo, closing.span.hi);
        fields.shrink_to_fit();
        Ok(self.spanned(span, ExprType::Set(Rc::new(fields), rec)))
    }

    fn error<T>(&self, span: Span, error: ErrorType) -> Result<T> {
        Err(Error {
            span,
            error,
            context: vec![],
        })
    }
}

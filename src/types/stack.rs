use crate::types::{
    op::Op,
    span::{Span, Spanned},
    store::Store,
    tree::{ExprId, SExpr, Value},
};

/// A stack for combining a stream of expressions and operators to a single expression
/// that respects the operator precedence.
///
/// = Remarks
///
/// E.g., `a + b * c + d == `(a + (b * c)) + d`.
///
/// Uses the shunting-yard algorithm.
pub struct Stack {
    expr: Vec<SExpr>,
    op: Vec<Op>,
    store: Store,
}

impl Stack {
    /// Creates a new stack.
    pub fn new(store: Store) -> Stack {
        Stack {
            expr: Vec::new(),
            op: Vec::new(),
            store,
        }
    }

    /// Pushes an expression onto the stack.
    pub fn push_expr(&mut self, expr: SExpr) {
        self.expr.push(expr);
    }

    /// Pops an expression from the stack.
    ///
    /// = Remarks
    ///
    /// Panics if there are no expressions on the stack.
    pub fn pop_expr(&mut self) -> SExpr {
        self.expr.pop().unwrap()
    }

    /// Pushes an operator onto the stack.
    pub fn push_op(&mut self, nop: Op) {
        self.next_op(nop);
        self.op.push(nop);
    }

    /// Announces the next operator.
    ///
    /// = Remarks
    ///
    /// This causes expressions and operators that are already on the stack to be combined
    /// to a single expression if this is what would happen if the operator were pushed
    /// onto the stack. This new expression can then be popped off of the stack.
    ///
    /// This function is necessary because we want to handle certain operators inside the
    /// parser instead of the stack. See the comment in `combine_unary` below.
    pub fn next_op(&mut self, nop: Op) {
        let nprec = nop.precedence();
        while !self.op.is_empty() {
            let op = *self.op.last().unwrap();
            let prec = op.precedence();
            if prec > nprec || (prec >= nprec && op.left_assoc()) {
                self.combine();
            } else {
                break;
            }
        }
    }

    /// Combines all expressions and operators that are on the stack into a single
    /// expression.
    pub fn clear(&mut self) -> SExpr {
        while !self.op.is_empty() {
            self.combine();
        }
        assert_eq!(self.expr.len(), 1);
        self.expr.pop().unwrap()
    }

    /// Pops an operator and one (in the case of a unary operator) or two (in the case of
    /// a binary operator) off of the stack to combine them.
    fn combine(&mut self) {
        let op = *self.op.last().unwrap();
        if op.unary() {
            self.combine_unary()
        } else {
            self.combine_binary()
        }
    }

    fn combine_binary(&mut self) {
        let op = self.op.pop().unwrap();
        let right = self.expr.pop().unwrap();
        let left = self.expr.pop().unwrap();
        let expr: fn(ExprId, ExprId) -> Value = match op {
            Op::Impl => Value::Impl,
            Op::Or => Value::Or,
            Op::And => Value::And,
            Op::Le => Value::Le,
            Op::Ge => Value::Ge,
            Op::Lt => Value::Lt,
            Op::Gt => Value::Gt,
            Op::Eq => Value::Eq,
            Op::Ne => Value::Ne,
            Op::Overlay => Value::Overlay,
            Op::Add => Value::Add,
            Op::Sub => Value::Sub,
            Op::Mul => Value::Mul,
            Op::Div => Value::Div,
            Op::Mod => Value::Mod,
            Op::Concat => Value::Concat,
            Op::Apl => Value::Apl,

            // these are not handled via the stack but directly in the parser
            Op::Select | Op::Test => unreachable!(),

            // these are handled in combine_unary
            Op::Not(..) | Op::UnMin(..) => unreachable!(),
        };
        let span = Span::new(left.span.lo, right.span.hi);
        let expr =
            Spanned::new(span, self.store.add_expr(span, expr(left.val, right.val)));
        self.expr.push(expr)
    }

    fn combine_unary(&mut self) {
        let op = self.op.pop().unwrap();
        let arg = self.expr.pop().unwrap();
        let (lo, expr): (_, fn(ExprId) -> Value) = match op {
            Op::Not(lo) => (lo, Value::Not),
            Op::UnMin(lo) => (lo, Value::Neg),

            // the rest is handled in combine_binary
            _ => unreachable!(),
        };
        let span = Span::new(lo, arg.span.hi);
        let expr = Spanned::new(span, self.store.add_expr(span, expr(arg.val)));
        self.expr.push(expr)
    }
}

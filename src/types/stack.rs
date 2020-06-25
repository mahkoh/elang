use crate::types::{
    op::Op,
    span::{Span, Spanned},
    store::Store,
    tree::{ExprId, ExprType, SExpr},
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
}

impl Stack {
    /// Creates a new stack.
    pub fn new() -> Stack {
        Stack {
            expr: Vec::new(),
            op: Vec::new(),
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
    pub fn push_op(&mut self, store: &mut Store, nop: Op) {
        self.next_op(store, nop);
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
    pub fn next_op(&mut self, store: &mut Store, nop: Op) {
        let nprec = nop.precedence();
        while !self.op.is_empty() {
            let op = *self.op.last().unwrap();
            let prec = op.precedence();
            if prec > nprec || (prec >= nprec && op.left_assoc()) {
                self.combine(store);
            } else {
                break;
            }
        }
    }

    /// Combines all expressions and operators that are on the stack into a single
    /// expression.
    pub fn clear(&mut self, store: &mut Store) -> SExpr {
        while !self.op.is_empty() {
            self.combine(store);
        }
        assert_eq!(self.expr.len(), 1);
        self.expr.pop().unwrap()
    }

    /// Pops an operator and one (in the case of a unary operator) or two (in the case of
    /// a binary operator) off of the stack to combine them.
    fn combine(&mut self, store: &mut Store) {
        let op = *self.op.last().unwrap();
        if op.unary() {
            self.combine_unary(store)
        } else {
            self.combine_binary(store)
        }
    }

    fn combine_binary(&mut self, store: &mut Store) {
        let op = self.op.pop().unwrap();
        let right = self.expr.pop().unwrap();
        let left = self.expr.pop().unwrap();
        let expr: fn(ExprId, ExprId) -> ExprType = match op {
            Op::Impl => ExprType::Impl,
            Op::Or => ExprType::Or,
            Op::And => ExprType::And,
            Op::Le => ExprType::Le,
            Op::Ge => ExprType::Ge,
            Op::Lt => ExprType::Lt,
            Op::Gt => ExprType::Gt,
            Op::Eq => ExprType::Eq,
            Op::Ne => ExprType::Ne,
            Op::Overlay => ExprType::Overlay,
            Op::Add => ExprType::Add,
            Op::Sub => ExprType::Sub,
            Op::Mul => ExprType::Mul,
            Op::Div => ExprType::Div,
            Op::Mod => ExprType::Mod,
            Op::Concat => ExprType::Concat,
            Op::Apl => ExprType::Apl,

            // these are not handled via the stack but directly in the parser
            Op::Select | Op::Test => unreachable!(),

            // these are handled in combine_unary
            Op::Not(..) | Op::UnMin(..) => unreachable!(),
        };
        let span = Span::new(left.span.lo, right.span.hi);
        let expr = Spanned::new(span, store.add_expr(span, expr(left.val, right.val)));
        self.expr.push(expr)
    }

    fn combine_unary(&mut self, store: &mut Store) {
        let op = self.op.pop().unwrap();
        let arg = self.expr.pop().unwrap();
        let (lo, expr): (_, fn(ExprId) -> ExprType) = match op {
            Op::Not(lo) => (lo, ExprType::Not),
            Op::UnMin(lo) => (lo, ExprType::Neg),

            // the rest is handled in combine_binary
            _ => unreachable!(),
        };
        let span = Span::new(lo, arg.span.hi);
        let expr = Spanned::new(span, store.add_expr(span, expr(arg.val)));
        self.expr.push(expr)
    }
}

use crate::types::{
    op::Op,
    span::Span,
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
            Op::Or => |lhs, rhs| ExprType::Or { lhs, rhs },
            Op::And => |lhs, rhs| ExprType::And { lhs, rhs },
            Op::Le => |lhs, rhs| ExprType::Le { lhs, rhs },
            Op::Ge => |lhs, rhs| ExprType::Ge { lhs, rhs },
            Op::Lt => |lhs, rhs| ExprType::Lt { lhs, rhs },
            Op::Gt => |lhs, rhs| ExprType::Gt { lhs, rhs },
            Op::Eq => |lhs, rhs| ExprType::Eq { lhs, rhs },
            Op::Ne => |lhs, rhs| ExprType::Ne { lhs, rhs },
            Op::Overlay => |lower, upper| ExprType::Overlay { lower, upper },
            Op::Add => |lhs, rhs| ExprType::Add { lhs, rhs },
            Op::Sub => |lhs, rhs| ExprType::Sub { lhs, rhs },
            Op::Mul => |lhs, rhs| ExprType::Mul { lhs, rhs },
            Op::Div(true) => |numer, denom| ExprType::Div {
                numer,
                denom,
                int: true,
            },
            Op::Mod(true) => |numer, denom| ExprType::Mod {
                numer,
                denom,
                int: true,
            },
            Op::Div(false) => |numer, denom| ExprType::Div {
                numer,
                denom,
                int: false,
            },
            Op::Mod(false) => |numer, denom| ExprType::Mod {
                numer,
                denom,
                int: false,
            },
            Op::Concat => |lhs, rhs| ExprType::Concat { lhs, rhs },
            Op::Apl => |func, arg| ExprType::Apl { func, arg },

            // these are not handled via the stack but directly in the parser
            Op::Select | Op::Test => unreachable!(),

            // these are handled in combine_unary
            Op::Not(..) | Op::UnMin(..) => unreachable!(),
        };
        let span = Span::new(left.span.lo, right.span.hi);
        let expr = span.span(store.add_expr(span, expr(left.val, right.val)));
        self.expr.push(expr)
    }

    fn combine_unary(&mut self, store: &mut Store) {
        let op = self.op.pop().unwrap();
        let arg = self.expr.pop().unwrap();
        let (lo, expr): (_, fn(ExprId) -> ExprType) = match op {
            Op::Not(lo) => (lo, |val| ExprType::Not { val }),
            Op::UnMin(lo) => (lo, |val| ExprType::Neg { val }),

            // the rest is handled in combine_binary
            _ => unreachable!(),
        };
        let span = Span::new(lo, arg.span.hi);
        let expr = span.span(store.add_expr(span, expr(arg.val)));
        self.expr.push(expr)
    }
}

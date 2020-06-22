use crate::{
    eval::Eval,
    types::{
        diagnostic::{ErrorContext, ErrorType},
        result::{Result, ResultUtil},
        scope::Scope,
        span::Span,
        tree::{Expr, ExprId, FnArg, FnType, Selector, Value, ValueType},
    },
    Error,
};
use std::rc::Rc;

impl Eval {
    /// Forces (evaluates) the expression.
    ///
    /// = Remarks
    ///
    /// `val` should not be borrowed when this function is invoked. Otherwise this will be
    /// interpreted as illegal infinite recursion, an error will be printed, and an error
    /// will be returned.
    ///
    /// This function modifies existing expressions.
    ///
    /// After this function returns successfully, the expression has been replaced by a
    /// datatype expression or a reference to a datatype expression. If this expression is
    /// already a datatype expression, no operation is performed.
    ///
    /// Datatype expressions are:
    ///
    /// * Integer
    /// * Ident
    /// * Set
    /// * Bool
    /// * Null
    /// * List
    /// * Fn
    ///
    /// This evaluation occurs top-to-bottom and stops once we have created datatype
    /// expression. This means that the following expression will be evaluated to itself
    /// (is a no-op):
    ///
    /// ```elang
    /// rec { a = b, b = a }
    /// ```
    ///
    /// even though the fields themselves could not be evaluated. Similarly, the following
    /// will evaluate to `0`:
    ///
    /// ```elang
    /// rec { a = b, b = a, c = 0 }.c
    /// ```
    pub fn force(&mut self, eid: ExprId) -> Result {
        let expr = self.store.get_expr(eid);
        if expr.val.try_borrow_mut().is_err() {
            let context = self
                .force_trace
                .iter()
                .copied()
                .map(ErrorContext::EvalResolved)
                .rev()
                .collect();
            return Err(Error {
                span: expr.span,
                error: ErrorType::InfiniteRecursion(eid),
                context,
            });
        }
        self.force_trace.push(eid);
        let res = self.force_(expr);
        self.force_trace.pop();
        res
    }

    fn force_(&mut self, expr: Rc<Expr>) -> Result {
        let borrow = expr.val.borrow();

        match *borrow {
            Value::Integer(..)
            | Value::Ident(..)
            | Value::Bool(..)
            | Value::Null
            | Value::List(..)
            | Value::Fn(..)
            | Value::String(..)
            | Value::Set(_, false)
            | Value::Inherit => {
                // Nothing to do
                Ok(())
            }
            Value::Resolved(_, dst) => self.force(dst),
            Value::Add(..)
            | Value::Sub(..)
            | Value::Mul(..)
            | Value::Div(..)
            | Value::Mod(..)
            | Value::Neg(..) => {
                drop(borrow);
                self.force_int(&expr)
            }
            Value::And(..)
            | Value::Or(..)
            | Value::Not(..)
            | Value::Gt(..)
            | Value::Lt(..)
            | Value::Ge(..)
            | Value::Le(..)
            | Value::Test(..)
            | Value::Eq(..)
            | Value::Ne(..)
            | Value::Impl(..) => {
                drop(borrow);
                self.force_bool(&expr)
            }
            Value::Concat(..) => {
                drop(borrow);
                self.force_concat(&expr)
            }
            Value::Overlay(..) => {
                drop(borrow);
                self.force_overlay(&expr)
            }
            Value::Select(..) => {
                drop(borrow);
                self.force_select(&expr)
            }
            Value::Apl(..) => {
                drop(borrow);
                self.force_apl(&expr)
            }
            Value::Let(..) | Value::Set(_, true) => {
                drop(borrow);
                self.force_bind(&expr, &mut Scope::new(), false);
                self.force(expr.id)
            }
            Value::Cond(..) => {
                drop(borrow);
                self.force_cond(&expr)
            }
            Value::Stringify(..) => {
                drop(borrow);
                self.force_stringify(&expr)
            }
            Value::Path(..) | Value::Selector(..) => {
                self.error(expr.id, ErrorType::CannotForceExpr(borrow.ty()))
            }
        }
    }

    fn force_int(&mut self, expr: &Expr) -> Result {
        let ctx = ErrorContext::EvalArithmetic(expr.id);

        macro_rules! c {
            ($v:expr) => {
                match $v {
                    Some(v) => v,
                    _ => {
                        return self.error(expr.id, ErrorType::Overflow);
                    }
                }
            };
        }

        let int = |slf: &mut Self, v| match slf.get_int(v) {
            Ok(v) => Ok(v),
            Err(mut e) => {
                e.context.push(ctx);
                Err(e)
            }
        };

        let new = match *expr.val.borrow() {
            Value::Add(l, r) => {
                Value::Integer(c!(int(self, l)?.checked_add(int(self, r)?)))
            }
            Value::Sub(l, r) => {
                Value::Integer(c!(int(self, l)?.checked_sub(int(self, r)?)))
            }
            Value::Mul(l, r) => {
                Value::Integer(c!(int(self, l)?.checked_mul(int(self, r)?)))
            }
            Value::Div(l, r) => {
                let l = int(self, l)?;
                let rn = int(self, r)?;
                if rn == 0 {
                    return self.error(r, ErrorType::DivideByZero).ctx(ctx);
                }
                Value::Integer(l / rn)
            }
            Value::Mod(l, r) => {
                let l = int(self, l)?;
                let rn = int(self, r)?;
                if rn == 0 {
                    return self.error(r, ErrorType::DivideByZero).ctx(ctx);
                }
                Value::Integer(l % rn)
            }
            Value::Neg(e) => Value::Integer(c!(0i64.checked_sub(int(self, e)?))),
            _ => unreachable!(),
        };

        *expr.val.borrow_mut() = new;

        Ok(())
    }

    fn force_bool(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalBool(expr.id);

        fn get<U>(u: Result<U>, ctx: ErrorContext) -> Result<U> {
            match u {
                Ok(v) => Ok(v),
                Err(mut e) => {
                    e.context.push(ctx);
                    Err(e)
                }
            }
        }
        let int = |slf: &mut Self, v| get(slf.get_int(v), ctx);
        let bol = |slf: &mut Self, v| get(slf.get_bool(v), ctx);

        let new = match *val {
            Value::Gt(l, r) => Value::Bool(int(self, l)? > int(self, r)?),
            Value::Lt(l, r) => Value::Bool(int(self, l)? < int(self, r)?),
            Value::Ge(l, r) => Value::Bool(int(self, l)? >= int(self, r)?),
            Value::Le(l, r) => Value::Bool(int(self, l)? <= int(self, r)?),
            Value::Eq(l, r) => Value::Bool(self.equal_to(r, l)?),
            Value::Ne(l, r) => Value::Bool(!self.equal_to(r, l)?),
            Value::Impl(l, r) => Value::Bool(!bol(self, l)? || bol(self, r)?),
            Value::And(l, r) => Value::Bool(bol(self, l)? && bol(self, r)?),
            Value::Or(l, r) => Value::Bool(bol(self, l)? || bol(self, r)?),
            Value::Not(e) => Value::Bool(!bol(self, e)?),
            Value::Test(s, path) => {
                let mut set = s;
                let mut path = &self.get_path(path).ctx(ctx)?[..];
                while !path.is_empty() {
                    let selector = self.get_selector(path[0]).ctx(ctx)?;
                    set = match self.get_opt_field(set, &selector, None).ctx(ctx)? {
                        Some(f) => f,
                        None => break,
                    };
                    path = &path[1..];
                }
                Value::Bool(path.is_empty())
            }
            _ => unreachable!(),
        };

        *val = new;

        Ok(())
    }

    fn force_overlay(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalOverlay(expr.id);

        let new = if let Value::Overlay(bottom, top) = *val {
            let bottom = self.get_fields(bottom).ctx(ctx)?;
            let top = self.get_fields(top).ctx(ctx)?;

            let mut new = (*bottom).clone();

            for (&id, &(span, val)) in top.iter() {
                new.insert(id, (span, val));
            }

            new.shrink_to_fit();

            Value::Set(Rc::new(new), false)
        } else {
            unreachable!();
        };

        *val = new;

        Ok(())
    }

    fn force_concat(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalConcat(expr.id);

        let new = if let Value::Concat(l, r) = *val {
            let left = self.resolve(l)?;
            let left = left.val.borrow();
            match *left {
                Value::String(left) => {
                    let right = self.get_string(r).ctx(ctx)?;
                    let new = self.store.concat(left, right);
                    Value::String(new)
                }
                Value::List(ref left) => {
                    let right = self.get_list(r).ctx(ctx)?;
                    if right.len() == 0 {
                        Value::List(left.clone())
                    } else {
                        let mut left = (**left).to_vec();
                        left.reserve(right.len());
                        for &el in right.iter() {
                            left.push(el);
                        }
                        Value::List(Rc::from(left.into_boxed_slice()))
                    }
                }
                _ => {
                    return self
                        .error(
                            l,
                            ErrorType::UnexpectedExpr(
                                &[ValueType::String, ValueType::List],
                                left.ty(),
                            ),
                        )
                        .ctx(ctx);
                }
            }
        } else {
            unreachable!();
        };

        *val = new;

        Ok(())
    }

    fn force_bind(&mut self, expr: &Expr, scope: &mut Scope<ExprId>, in_fn_body: bool) {
        macro_rules! resolve {
            ($a:expr) => {{
                self.force_bind(&self.store.get_expr($a), scope, in_fn_body);
            }};
        }

        let mut val = expr.val.borrow_mut();

        // Identifiers
        {
            if let Value::Ident(id) = *val {
                if let Some(e) = scope.get(id) {
                    *val = Value::Resolved(Some(id), e);
                }
                return;
            }
        }

        // Lets
        {
            let mut new_val = None;
            if let Value::Let(ref fields, body) = *val {
                for (&id, &(_, val)) in fields.iter() {
                    if in_fn_body {
                        scope.hide(id);
                    } else {
                        scope.bind(id, val);
                    }
                }
                for (_, val) in fields.values() {
                    resolve!(*val);
                }
                resolve!(body);
                for &id in fields.keys() {
                    scope.pop(id);
                }
                if !in_fn_body {
                    new_val = Some(Value::Resolved(None, body));
                }
            }
            if let Value::Let(..) = *val {
                if let Some(new_val) = new_val {
                    *val = new_val;
                }
                return;
            }
        }

        // Recursive sets
        {
            let mut new_val = None;
            if let Value::Set(ref fields, true) = *val {
                let mut added_to_scope = vec![];
                for (&id, &(_, val)) in fields.iter() {
                    let val = self.store.get_expr(val);
                    if !val.is_inherit() {
                        if in_fn_body {
                            scope.hide(id);
                        } else {
                            scope.bind(id, val.id);
                        }
                        added_to_scope.push(id);
                    }
                }
                for (&id, &(_, val)) in fields.iter() {
                    let val = self.store.get_expr(val);
                    if val.is_inherit() {
                        if let Some(e) = scope.get(id) {
                            *val.val.borrow_mut() = Value::Resolved(Some(id), e);
                        }
                    } else {
                        resolve!(val.id);
                    }
                }
                for id in added_to_scope {
                    scope.pop(id);
                }
                if !in_fn_body {
                    new_val = Some(Value::Set(fields.clone(), false));
                }
            }
            if let Value::Set(_, true) = *val {
                if let Some(new_val) = new_val {
                    *val = new_val;
                }
                return;
            }
        }

        // Functions
        {
            if let Value::Fn(FnType::Normal(ref pat, body)) = *val {
                match pat.val {
                    FnArg::Ident(id) => scope.hide(id),
                    FnArg::Pat(id, ref fields, _) => {
                        for (_, field_alt) in fields.values() {
                            if let Some(field_alt) = *field_alt {
                                resolve!(field_alt);
                            }
                        }
                        for &field_name in fields.keys() {
                            scope.hide(field_name);
                        }
                        if let Some(id) = id {
                            scope.hide(id.val);
                        }
                    }
                }
                self.force_bind(&self.store.get_expr(body), scope, true);
                match pat.val {
                    FnArg::Ident(id) => scope.pop(id),
                    FnArg::Pat(id, ref fields, _) => {
                        for &field_name in fields.keys() {
                            scope.pop(field_name);
                        }
                        if let Some(id) = id {
                            scope.pop(id.val);
                        }
                    }
                }
                return;
            }
        }

        // The rest
        match *val {
            Value::Ident(..)
            | Value::Let(..)
            | Value::Set(_, true)
            | Value::Fn(FnType::Normal(..)) => {
                // handled above
                unreachable!();
            }
            Value::Null
            | Value::String(..)
            | Value::Integer(..)
            | Value::Resolved(..)
            | Value::Fn(FnType::BuiltIn(..))
            | Value::Bool(..)
            | Value::Inherit => {
                // nothing to do
            }
            Value::Not(e) | Value::Neg(e) | Value::Stringify(e) => {
                resolve!(e);
            }
            Value::And(l, r)
            | Value::Or(l, r)
            | Value::Add(l, r)
            | Value::Sub(l, r)
            | Value::Mul(l, r)
            | Value::Div(l, r)
            | Value::Mod(l, r)
            | Value::Gt(l, r)
            | Value::Lt(l, r)
            | Value::Ge(l, r)
            | Value::Le(l, r)
            | Value::Eq(l, r)
            | Value::Ne(l, r)
            | Value::Impl(l, r)
            | Value::Overlay(l, r)
            | Value::Concat(l, r)
            | Value::Apl(l, r) => {
                resolve!(l);
                resolve!(r);
            }
            Value::Cond(cond, then, el) => {
                resolve!(cond);
                resolve!(then);
                resolve!(el);
            }
            Value::Set(ref fields, false) => {
                for (&name, &(_, val)) in fields.iter() {
                    let val = self.store.get_expr(val);
                    if val.is_inherit() {
                        if let Some(e) = scope.get(name) {
                            *val.val.borrow_mut() = Value::Resolved(Some(name), e);
                        }
                    } else {
                        resolve!(val.id);
                    }
                }
            }
            Value::List(ref fields) => {
                for &field in fields.iter() {
                    resolve!(field);
                }
            }
            Value::Test(target, path) => {
                resolve!(target);
                resolve!(path);
            }
            Value::Select(target, path, ref alt) => {
                resolve!(target);
                resolve!(path);
                if let Some(alt) = *alt {
                    resolve!(alt);
                }
            }
            Value::Path(ref segs) => {
                for &seg in segs.iter() {
                    resolve!(seg);
                }
            }
            Value::Selector(ref ty) => {
                if let Selector::Expr(e) = *ty {
                    resolve!(e);
                }
            }
        }
    }

    fn force_cond(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalCond(expr.id);

        let new = if let Value::Cond(cond, then, el) = *val {
            if self.get_bool(cond).ctx(ctx)? {
                self.force(then)?;
                then
            } else {
                self.force(el)?;
                el
            }
        } else {
            unreachable!()
        };

        *val = Value::Resolved(None, new);

        Ok(())
    }

    fn force_stringify(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let e = match *val {
            Value::Stringify(e) => e,
            _ => unreachable!(),
        };
        let dst = self.resolve(e)?;
        let dst = dst.val.borrow();
        match *dst {
            Value::String(..) => *val = Value::Resolved(None, e),
            Value::Integer(v) => {
                let s = format!("{}", v);
                let id = self.store.add_str(s.into_bytes().into_boxed_slice().into());
                *val = Value::String(id);
            }
            _ => {
                drop(val);
                return self
                    .error(
                        expr.id,
                        ErrorType::UnexpectedExpr(
                            &[ValueType::String, ValueType::Integer],
                            dst.ty(),
                        ),
                    )
                    .ctx(ErrorContext::EvalStringify(expr.id));
            }
        }
        Ok(())
    }

    fn force_apl(&mut self, apl: &Expr) -> Result {
        match self.force_apl_(apl) {
            Ok(()) => Ok(()),
            Err(mut e) => {
                e.context.push(ErrorContext::EvalApl(apl.id));
                Err(e)
            }
        }
    }

    fn force_apl_(&mut self, apl: &Expr) -> Result {
        let (func, arg) = match *apl.val.borrow() {
            Value::Apl(func, arg) => (func, arg),
            _ => unreachable!(),
        };

        let (pat, body) = match self.get_func(func)? {
            FnType::Normal(pat, body) => (pat, body),
            FnType::BuiltIn(func) => {
                let arg = self.store.get_expr(arg);
                let res = func.apply(arg)?;
                *apl.val.borrow_mut() = res;
                return Ok(());
            }
        };

        let mut scope = Scope::new();

        match pat.val {
            FnArg::Ident(i) => {
                scope.bind(i, arg);
            }
            FnArg::Pat(at, fields, wild) => {
                let arg_fields = self.get_fields(arg)?;
                if !wild {
                    for &id in arg_fields.keys() {
                        if fields.get(&id).is_none() {
                            return self.error(arg, ErrorType::ExtraArgument(id));
                        }
                    }
                }
                for (&id, &(_, alt)) in fields.iter() {
                    if let Some(&(_, val)) = arg_fields.get(&id) {
                        scope.bind(id, val);
                    } else if let Some(alt) = alt {
                        scope.bind(id, alt);
                    } else {
                        return self.error(arg, ErrorType::MissingArgument(id));
                    }
                }
                if let Some(at) = at {
                    scope.bind(at.val, arg);
                }
            }
        }

        let new_body = self.deep_copy(body);
        self.force_bind(&self.store.get_expr(new_body), &mut scope, false);
        self.force(new_body)?;

        *apl.val.borrow_mut() = Value::Resolved(None, new_body);

        Ok(())
    }

    /// Evaluates a selection expression.
    ///
    /// = Remarks
    ///
    /// A selection expression is an expression of the form
    ///
    /// ----
    /// a.b or c
    /// ----
    ///
    /// where the `or c` part is optional and `b` can consist of multiple identifiers.
    ///
    /// If this is not a selection expression, the process is aborted.
    ///
    /// After this function returns successfully, the expression has been replaced by a
    /// reference to the selected expression. If the path does not exist and an
    /// alternative expression has been provided (`or c`), a reference to that expression
    /// is stored. If the path does not exist and no alternative has been provided, an
    /// error is printed and an error is returned.
    ///
    /// Note that all expressions which are selected on have to be sets or an error is
    /// printed, even if an alternative is provided.
    ///
    /// ----
    /// { a = 0 }.a               # evaluates to 0
    /// { a = { b = 0 } }.a.b     # evaluates to 0
    /// { }.a or 0                # evaluates to 0
    /// 1.a or 0                  # prints an error
    /// ----
    fn force_select(&mut self, expr: &Expr) -> Result {
        let res = {
            let val = expr.val.borrow();
            let ctx = ErrorContext::EvalSelect(expr.id);

            let (mut set, path, alt) = match *val {
                Value::Select(s, p, a) => (s, p, a),
                _ => unreachable!(),
            };

            let mut path = &self.get_path(path).ctx(ctx)?[..];
            let mut bad_path = Selector::Integer(0);
            let mut last_ok = set;

            while !path.is_empty() {
                let selector = self.get_selector(path[0]).ctx(ctx)?;
                set = match self
                    .get_opt_field(set, &selector, Some(&mut bad_path))
                    .ctx(ctx)?
                {
                    Some(f) => f,
                    _ => break,
                };
                last_ok = path[0];
                path = &path[1..];
            }

            if !path.is_empty() {
                let err_span = Span::new(self.span(set).lo, self.span(last_ok).hi);
                if let Some(alt) = alt {
                    self.force(alt)?;
                    alt
                } else {
                    let et = match bad_path {
                        Selector::Ident(i) => ErrorType::MissingSetField(i),
                        Selector::Integer(i) => ErrorType::MissingListField(i),
                        _ => unreachable!(),
                    };
                    let mut e = self.error_(set, et);
                    e.span = err_span;
                    return Err(e);
                }
            } else {
                self.force(set)?;
                set
            }
        };

        *expr.val.borrow_mut() = Value::Resolved(None, res);

        Ok(())
    }
}

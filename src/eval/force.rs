use crate::{
    types::{
        diagnostic::{ErrorContext, ErrorType},
        result::{Result, ResultUtil},
        scope::Scope,
        span::Span,
        tree::{Expr, ExprId, ExprKind, ExprType, FnParam, FnType},
    },
    Elang, Error,
};
use num_rational::BigRational;
use num_traits::identities::Zero;
use std::{ops::Neg, rc::Rc};

impl Elang {
    pub(crate) fn force(&mut self, eid: ExprId) -> Result {
        let expr = self.store.get_expr(eid);
        if expr.val.try_borrow_mut().is_err() {
            let mut context = vec![];
            for ex in self.force_trace.iter().rev() {
                match context.last() {
                    Some(ErrorContext::EvalResolved(e)) if e == ex => {}
                    _ => context.push(ErrorContext::EvalResolved(*ex)),
                }
            }
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
            ExprType::Number(..)
            | ExprType::Ident(..)
            | ExprType::Bool(..)
            | ExprType::Null
            | ExprType::List(..)
            | ExprType::Fn(..)
            | ExprType::String(..)
            | ExprType::Set(_, false)
            | ExprType::Inherit => {
                // Nothing to do
                Ok(())
            }
            ExprType::Resolved(_, dst) => self.force(dst),
            ExprType::Add(..)
            | ExprType::Sub(..)
            | ExprType::Mul(..)
            | ExprType::Div(..)
            | ExprType::Mod(..)
            | ExprType::Neg(..) => {
                drop(borrow);
                self.force_int(&expr)
            }
            ExprType::And(..)
            | ExprType::Or(..)
            | ExprType::Not(..)
            | ExprType::Gt(..)
            | ExprType::Lt(..)
            | ExprType::Ge(..)
            | ExprType::Le(..)
            | ExprType::Test(..)
            | ExprType::Eq(..)
            | ExprType::Ne(..)
            | ExprType::Impl(..) => {
                drop(borrow);
                self.force_bool(&expr)
            }
            ExprType::Concat(..) => {
                drop(borrow);
                self.force_concat(&expr)
            }
            ExprType::Overlay(..) => {
                drop(borrow);
                self.force_overlay(&expr)
            }
            ExprType::Select(..) => {
                drop(borrow);
                self.force_select(&expr)
            }
            ExprType::Apl(..) => {
                drop(borrow);
                self.force_apl(&expr)
            }
            ExprType::Let(..) | ExprType::Set(_, true) => {
                drop(borrow);
                self.force_bind(&expr, &mut Scope::new(), false);
                self.force(expr.id)
            }
            ExprType::Cond(..) => {
                drop(borrow);
                self.force_cond(&expr)
            }
            ExprType::Stringify(..) => {
                drop(borrow);
                self.force_stringify(&expr)
            }
            ExprType::Path(..) => {
                self.error(expr.id, ErrorType::CannotForceExpr(borrow.kind()))
            }
        }
    }

    fn force_int(&mut self, expr: &Expr) -> Result {
        let ctx = ErrorContext::EvalArithmetic(expr.id);

        let int = |slf: &mut Self, v| match slf.get_int_(v) {
            Ok(v) => Ok(v.clone()),
            Err(mut e) => {
                e.context.push(ctx);
                Err(e)
            }
        };

        let new = match *expr.val.borrow() {
            ExprType::Add(l, r) => {
                ExprType::Number(Rc::new(&*int(self, l)? + &*int(self, r)?))
            }
            ExprType::Sub(l, r) => {
                ExprType::Number(Rc::new(&*int(self, l)? - &*int(self, r)?))
            }
            ExprType::Mul(l, r) => {
                ExprType::Number(Rc::new(&*int(self, l)? * &*int(self, r)?))
            }
            ExprType::Div(l, r) => {
                let l = int(self, l)?;
                let rn = int(self, r)?;
                if *rn == BigRational::zero() {
                    return self.error(r, ErrorType::DivideByZero).ctx(ctx);
                }
                ExprType::Number(Rc::new(&*l / &*rn))
            }
            ExprType::Mod(l, r) => {
                let l = int(self, l)?;
                let rn = int(self, r)?;
                if *rn == BigRational::zero() {
                    return self.error(r, ErrorType::DivideByZero).ctx(ctx);
                }
                ExprType::Number(Rc::new(&*l % &*rn))
            }
            ExprType::Neg(e) => ExprType::Number(Rc::new((*int(self, e)?).clone().neg())),
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
        let int = |slf: &mut Self, v| get(slf.get_int_(v), ctx);
        let bol = |slf: &mut Self, v| get(slf.get_bool_(v), ctx);

        let new = match *val {
            ExprType::Gt(l, r) => ExprType::Bool(int(self, l)? > int(self, r)?),
            ExprType::Lt(l, r) => ExprType::Bool(int(self, l)? < int(self, r)?),
            ExprType::Ge(l, r) => ExprType::Bool(int(self, l)? >= int(self, r)?),
            ExprType::Le(l, r) => ExprType::Bool(int(self, l)? <= int(self, r)?),
            ExprType::Eq(l, r) => ExprType::Bool(self.equal_to(r, l)?),
            ExprType::Ne(l, r) => ExprType::Bool(!self.equal_to(r, l)?),
            ExprType::Impl(l, r) => ExprType::Bool(!bol(self, l)? || bol(self, r)?),
            ExprType::And(l, r) => ExprType::Bool(bol(self, l)? && bol(self, r)?),
            ExprType::Or(l, r) => ExprType::Bool(bol(self, l)? || bol(self, r)?),
            ExprType::Not(e) => ExprType::Bool(!bol(self, e)?),
            ExprType::Test(s, path) => {
                let mut set = s;
                let mut path = &self.get_path(path).ctx(ctx)?[..];
                while !path.is_empty() {
                    set = match self.get_opt_field_(set, path[0]).ctx(ctx)? {
                        Some(f) => f,
                        None => break,
                    };
                    path = &path[1..];
                }
                ExprType::Bool(path.is_empty())
            }
            _ => unreachable!(),
        };

        *val = new;

        Ok(())
    }

    fn force_overlay(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalOverlay(expr.id);

        let new = if let ExprType::Overlay(bottom, top) = *val {
            let bottom = self.get_fields_(bottom).ctx(ctx)?;
            let top = self.get_fields_(top).ctx(ctx)?;

            let mut new = (*bottom).clone();

            for (&id, &val) in top.iter() {
                new.insert(id, val);
            }

            new.shrink_to_fit();

            ExprType::Set(Rc::new(new), false)
        } else {
            unreachable!();
        };

        *val = new;

        Ok(())
    }

    fn force_concat(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalConcat(expr.id);

        let new = if let ExprType::Concat(l, r) = *val {
            let left = self.resolve_(l)?;
            let left = left.val.borrow();
            match *left {
                ExprType::String(left) => {
                    let ctx2 = ErrorContext::EvalOtherExprType(l, ExprKind::String);
                    let right = self.get_string_(r).ctx(ctx2).ctx(ctx)?;
                    let new = self.store.concat(left, right);
                    ExprType::String(new)
                }
                ExprType::List(ref left) => {
                    let ctx2 = ErrorContext::EvalOtherExprType(l, ExprKind::List);
                    let right = self.get_list_(r).ctx(ctx2).ctx(ctx)?;
                    if right.len() == 0 {
                        ExprType::List(left.clone())
                    } else {
                        let mut left = (**left).to_vec();
                        left.reserve(right.len());
                        for &el in right.iter() {
                            left.push(el);
                        }
                        ExprType::List(Rc::from(left.into_boxed_slice()))
                    }
                }
                _ => {
                    return self
                        .error(
                            l,
                            ErrorType::UnexpectedExprType(
                                &[ExprKind::String, ExprKind::List],
                                left.kind(),
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
        macro_rules! bind {
            ($a:expr) => {{
                self.force_bind(&self.store.get_expr($a), scope, in_fn_body);
            }};
        }

        let mut val = expr.val.borrow_mut();

        // Identifiers
        {
            if let ExprType::Ident(id) = *val {
                if let Some(e) = scope.get(id) {
                    *val = ExprType::Resolved(Some(id), e);
                }
                return;
            }
        }

        // Lets
        {
            let mut new_val = None;
            if let ExprType::Let(ref fields, body) = *val {
                for (&id, &val) in fields.iter() {
                    if in_fn_body {
                        scope.hide(*id);
                    } else {
                        scope.bind(*id, val);
                    }
                }
                for &val in fields.values() {
                    bind!(val);
                }
                bind!(body);
                for &id in fields.keys() {
                    scope.pop(*id);
                }
                if !in_fn_body {
                    new_val = Some(ExprType::Resolved(None, body));
                }
            }
            if let ExprType::Let(..) = *val {
                if let Some(new_val) = new_val {
                    *val = new_val;
                }
                return;
            }
        }

        // Recursive sets
        {
            let mut new_val = None;
            if let ExprType::Set(ref fields, true) = *val {
                let mut added_to_scope = vec![];
                for (&id, &val) in fields.iter() {
                    let val = self.store.get_expr(val);
                    if !val.is_inherit() {
                        if in_fn_body {
                            scope.hide(*id);
                        } else {
                            scope.bind(*id, val.id);
                        }
                        added_to_scope.push(*id);
                    }
                }
                for (&id, &val) in fields.iter() {
                    let val = self.store.get_expr(val);
                    if val.is_inherit() {
                        if let Some(e) = scope.get(*id) {
                            *val.val.borrow_mut() = ExprType::Resolved(Some(*id), e);
                        }
                    } else {
                        bind!(val.id);
                    }
                }
                for id in added_to_scope {
                    scope.pop(id);
                }
                if !in_fn_body {
                    new_val = Some(ExprType::Set(fields.clone(), false));
                }
            }
            if let ExprType::Set(_, true) = *val {
                if let Some(new_val) = new_val {
                    *val = new_val;
                }
                return;
            }
        }

        // Functions
        {
            if let ExprType::Fn(FnType::Normal { ref param, body }) = *val {
                match param.val {
                    FnParam::Ident { param_name } => scope.hide(param_name),
                    FnParam::Pat {
                        param_name,
                        ref fields,
                        ..
                    } => {
                        for field_alt in fields.values() {
                            if let Some(field_alt) = *field_alt {
                                bind!(field_alt);
                            }
                        }
                        for &field_name in fields.keys() {
                            scope.hide(*field_name);
                        }
                        if let Some(param_name) = param_name {
                            scope.hide(*param_name);
                        }
                    }
                }
                self.force_bind(&self.store.get_expr(body), scope, true);
                match param.val {
                    FnParam::Ident { param_name } => scope.pop(param_name),
                    FnParam::Pat {
                        param_name,
                        ref fields,
                        ..
                    } => {
                        for &field_name in fields.keys() {
                            scope.pop(*field_name);
                        }
                        if let Some(param_name) = param_name {
                            scope.pop(*param_name);
                        }
                    }
                }
                return;
            }
        }

        // The rest
        match *val {
            ExprType::Ident(..)
            | ExprType::Let(..)
            | ExprType::Set(_, true)
            | ExprType::Fn(FnType::Normal { .. }) => {
                // handled above
                unreachable!();
            }
            ExprType::Null
            | ExprType::String(..)
            | ExprType::Number(..)
            | ExprType::Resolved(..)
            | ExprType::Fn(FnType::BuiltIn { .. })
            | ExprType::Bool(..)
            | ExprType::Inherit => {
                // nothing to do
            }
            ExprType::Not(e) | ExprType::Neg(e) | ExprType::Stringify(e) => {
                bind!(e);
            }
            ExprType::And(l, r)
            | ExprType::Or(l, r)
            | ExprType::Add(l, r)
            | ExprType::Sub(l, r)
            | ExprType::Mul(l, r)
            | ExprType::Div(l, r)
            | ExprType::Mod(l, r)
            | ExprType::Gt(l, r)
            | ExprType::Lt(l, r)
            | ExprType::Ge(l, r)
            | ExprType::Le(l, r)
            | ExprType::Eq(l, r)
            | ExprType::Ne(l, r)
            | ExprType::Impl(l, r)
            | ExprType::Overlay(l, r)
            | ExprType::Concat(l, r)
            | ExprType::Apl(l, r) => {
                bind!(l);
                bind!(r);
            }
            ExprType::Cond(cond, then, el) => {
                bind!(cond);
                bind!(then);
                bind!(el);
            }
            ExprType::Set(ref fields, false) => {
                for (&name, &val) in fields.iter() {
                    let val = self.store.get_expr(val);
                    if val.is_inherit() {
                        if let Some(e) = scope.get(*name) {
                            *val.val.borrow_mut() = ExprType::Resolved(Some(*name), e);
                        }
                    } else {
                        bind!(val.id);
                    }
                }
            }
            ExprType::List(ref fields) => {
                for &field in fields.iter() {
                    bind!(field);
                }
            }
            ExprType::Test(target, path) => {
                bind!(target);
                bind!(path);
            }
            ExprType::Select(target, path, ref alt) => {
                bind!(target);
                bind!(path);
                if let Some(alt) = *alt {
                    bind!(alt);
                }
            }
            ExprType::Path(ref segs) => {
                for &seg in segs.iter() {
                    bind!(seg);
                }
            }
        }
    }

    fn force_cond(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalCond(expr.id);

        let new = if let ExprType::Cond(cond, then, el) = *val {
            if self.get_bool_(cond).ctx(ctx)? {
                self.force(then)?;
                then
            } else {
                self.force(el)?;
                el
            }
        } else {
            unreachable!()
        };

        *val = ExprType::Resolved(None, new);

        Ok(())
    }

    fn force_stringify(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let e = match *val {
            ExprType::Stringify(e) => e,
            _ => unreachable!(),
        };
        let dst = self.resolve_(e)?;
        let dst = dst.val.borrow();
        match *dst {
            ExprType::String(..) => *val = ExprType::Resolved(None, e),
            ExprType::Number(ref v) => {
                if !v.is_integer() {
                    return self
                        .error(expr.id, ErrorType::CannotStringifyNonInteger)
                        .ctx(ErrorContext::EvalStringify(expr.id));
                }
                let s = format!("{}", v);
                let id = self.store.add_str(s.into_bytes().into_boxed_slice().into());
                *val = ExprType::String(id);
            }
            _ => {
                drop(val);
                return self
                    .error(
                        expr.id,
                        ErrorType::UnexpectedExprType(
                            &[ExprKind::String, ExprKind::Number],
                            dst.kind(),
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
            ExprType::Apl(func, arg) => (func, arg),
            _ => unreachable!(),
        };

        let (pat, body) = match self.get_func(func)? {
            FnType::Normal { param, body } => (param, body),
            FnType::BuiltIn { func } => {
                let arg = self.store.get_expr(arg);
                let res = func.apply(self, arg)?;
                *apl.val.borrow_mut() = res;
                return Ok(());
            }
        };

        let mut scope = Scope::new();

        match pat.val {
            FnParam::Ident { param_name } => {
                scope.bind(param_name, arg);
            }
            FnParam::Pat {
                param_name,
                fields,
                wild,
            } => {
                let arg_fields = self.get_fields_(arg)?;
                if !wild {
                    for &id in arg_fields.keys() {
                        if fields.get(&id).is_none() {
                            return self
                                .error(arg, ErrorType::ExtraArgument(*id, pat.span));
                        }
                    }
                }
                for (&id, &alt) in fields.iter() {
                    if let Some(&val) = arg_fields.get(&id) {
                        scope.bind(*id, val);
                    } else if let Some(alt) = alt {
                        scope.bind(*id, alt);
                    } else {
                        return self.error(arg, ErrorType::MissingArgument(id));
                    }
                }
                if let Some(param_name) = param_name {
                    scope.bind(param_name.val, arg);
                }
            }
        }

        let new_body = self.deep_copy(body);
        self.force_bind(&self.store.get_expr(new_body), &mut scope, false);
        self.force(new_body)?;

        *apl.val.borrow_mut() = ExprType::Resolved(None, new_body);

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
                ExprType::Select(s, p, a) => (s, p, a),
                _ => unreachable!(),
            };

            let mut path = &self.get_path(path).ctx(ctx)?[..];
            let mut last_ok = set;

            while !path.is_empty() {
                set = match self.get_opt_field_(set, path[0]).ctx(ctx)? {
                    Some(f) => f,
                    _ => break,
                };
                last_ok = path[0];
                path = &path[1..];
            }

            if !path.is_empty() {
                let err_span = Span::new(self.span_(set).lo, self.span_(last_ok).hi);
                if let Some(alt) = alt {
                    self.force(alt)?;
                    alt
                } else {
                    let bad_path = self.resolve_(path[0]).unreachable();
                    let bad_path = bad_path.val.borrow();
                    let et = match *bad_path {
                        ExprType::String(i) => ErrorType::MissingSetField(i),
                        ExprType::Number(ref i) => ErrorType::MissingListField(i.clone()),
                        _ => unreachable!(),
                    };
                    let mut e = self.error_(set, et);
                    e.span = err_span;
                    e.context.push(ctx);
                    return Err(e);
                }
            } else {
                self.force(set)?;
                set
            }
        };

        *expr.val.borrow_mut() = ExprType::Resolved(None, res);

        Ok(())
    }
}

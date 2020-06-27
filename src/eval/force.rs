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
            ExprType::Number { .. }
            | ExprType::Ident { .. }
            | ExprType::Bool { .. }
            | ExprType::Null
            | ExprType::List { .. }
            | ExprType::Fn { .. }
            | ExprType::String { .. }
            | ExprType::Set {
                recursive: false, ..
            }
            | ExprType::Inherit => {
                // Nothing to do
                Ok(())
            }
            ExprType::Resolved { dest, .. } => self.force(dest),
            ExprType::Add { .. }
            | ExprType::Sub { .. }
            | ExprType::Mul { .. }
            | ExprType::Div { .. }
            | ExprType::Mod { .. }
            | ExprType::Neg { .. } => {
                drop(borrow);
                self.force_int(&expr)
            }
            ExprType::And { .. }
            | ExprType::Or { .. }
            | ExprType::Not { .. }
            | ExprType::Gt { .. }
            | ExprType::Lt { .. }
            | ExprType::Ge { .. }
            | ExprType::Le { .. }
            | ExprType::Test { .. }
            | ExprType::Eq { .. }
            | ExprType::Ne { .. } => {
                drop(borrow);
                self.force_bool(&expr)
            }
            ExprType::Concat { .. } => {
                drop(borrow);
                self.force_concat(&expr)
            }
            ExprType::Overlay { .. } => {
                drop(borrow);
                self.force_overlay(&expr)
            }
            ExprType::Select { .. } => {
                drop(borrow);
                self.force_select(&expr)
            }
            ExprType::Apl { .. } => {
                drop(borrow);
                self.force_apl(&expr)
            }
            ExprType::Let { .. }
            | ExprType::Set {
                recursive: true, ..
            } => {
                drop(borrow);
                self.force_bind(&expr, &mut Scope::new(), false);
                self.force(expr.id)
            }
            ExprType::Cond { .. } => {
                drop(borrow);
                self.force_cond(&expr)
            }
            ExprType::Stringify { .. } => {
                drop(borrow);
                self.force_stringify(&expr)
            }
            ExprType::Path { .. } => {
                self.error(expr.id, ErrorType::CannotForceExpr(borrow.kind()))
            }
        }
    }

    fn force_int(&mut self, expr: &Expr) -> Result {
        let ctx = ErrorContext::EvalArithmetic(expr.id);

        let num = |slf: &mut Self, v| match slf.get_int_(v) {
            Ok(v) => Ok(v),
            Err(mut e) => {
                e.context.push(ctx);
                Err(e)
            }
        };

        let new = match *expr.val.borrow() {
            ExprType::Add { lhs, rhs } => &*num(self, lhs)? + &*num(self, rhs)?,
            ExprType::Sub { lhs, rhs } => &*num(self, lhs)? - &*num(self, rhs)?,
            ExprType::Mul { lhs, rhs } => &*num(self, lhs)? * &*num(self, rhs)?,
            ExprType::Div { numer, denom, int } => {
                let numer = num(self, numer)?;
                let denom_ = num(self, denom)?;
                if *denom_ == BigRational::zero() {
                    return self.error(denom, ErrorType::DivideByZero).ctx(ctx);
                }
                let res = &*numer / &*denom_;
                if int {
                    res.trunc()
                } else {
                    res
                }
            }
            ExprType::Mod { numer, denom, int } => {
                let numer = num(self, numer)?;
                let denom_ = num(self, denom)?;
                if *denom_ == BigRational::zero() {
                    return self.error(denom, ErrorType::DivideByZero).ctx(ctx);
                }
                let res = &*numer % &*denom_;
                if int {
                    res.trunc()
                } else {
                    res
                }
            }
            ExprType::Neg { val } => (*num(self, val)?).clone().neg(),
            _ => unreachable!(),
        };

        *expr.val.borrow_mut() = ExprType::Number { val: Rc::new(new) };

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
            ExprType::Gt { lhs, rhs } => int(self, lhs)? > int(self, rhs)?,
            ExprType::Lt { lhs, rhs } => int(self, lhs)? < int(self, rhs)?,
            ExprType::Ge { lhs, rhs } => int(self, lhs)? >= int(self, rhs)?,
            ExprType::Le { lhs, rhs } => int(self, lhs)? <= int(self, rhs)?,
            ExprType::Eq { lhs, rhs } => self.equal_to(rhs, lhs)?,
            ExprType::Ne { lhs, rhs } => !self.equal_to(rhs, lhs)?,
            ExprType::And { lhs, rhs } => bol(self, lhs)? && bol(self, rhs)?,
            ExprType::Or { lhs, rhs } => bol(self, lhs)? || bol(self, rhs)?,
            ExprType::Not { val } => !bol(self, val)?,
            ExprType::Test { base, path } => {
                let mut set = base;
                let mut path = &self.get_path(path).ctx(ctx)?[..];
                while !path.is_empty() {
                    set = match self.get_opt_field_(set, path[0]).ctx(ctx)? {
                        Some(f) => f,
                        None => break,
                    };
                    path = &path[1..];
                }
                path.is_empty()
            }
            _ => unreachable!(),
        };

        *val = ExprType::Bool { val: new };

        Ok(())
    }

    fn force_overlay(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalOverlay(expr.id);

        let new = if let ExprType::Overlay { lower, upper } = *val {
            let bottom = self.get_fields_(lower).ctx(ctx)?;
            let top = self.get_fields_(upper).ctx(ctx)?;

            let mut new = (*bottom).clone();

            for (&id, &val) in top.iter() {
                new.insert(id, val);
            }

            new.shrink_to_fit();

            ExprType::Set {
                fields: Rc::new(new),
                recursive: false,
            }
        } else {
            unreachable!();
        };

        *val = new;

        Ok(())
    }

    fn force_concat(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalConcat(expr.id);

        let new = if let ExprType::Concat { lhs, rhs } = *val {
            let left = self.resolve_(lhs)?;
            let left = left.val.borrow();
            match *left {
                ExprType::String { content: left } => {
                    let ctx2 = ErrorContext::EvalOtherExprType(lhs, ExprKind::String);
                    let right = self.get_string_(rhs).ctx(ctx2).ctx(ctx)?;
                    let new = self.store.concat(left, right);
                    ExprType::String { content: new }
                }
                ExprType::List { elements: ref left } => {
                    let ctx2 = ErrorContext::EvalOtherExprType(lhs, ExprKind::List);
                    let right = self.get_list_(rhs).ctx(ctx2).ctx(ctx)?;
                    if right.len() == 0 {
                        ExprType::List {
                            elements: left.clone(),
                        }
                    } else {
                        let mut left = (**left).to_vec();
                        left.reserve(right.len());
                        for &el in right.iter() {
                            left.push(el);
                        }
                        ExprType::List {
                            elements: Rc::from(left.into_boxed_slice()),
                        }
                    }
                }
                _ => {
                    return self
                        .error(
                            lhs,
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
            if let ExprType::Ident { name } = *val {
                if let Some(dest) = scope.get(name) {
                    *val = ExprType::Resolved {
                        ident: Some(name),
                        dest,
                    };
                }
                return;
            }
        }

        // Lets
        {
            let mut new_val = None;
            if let ExprType::Let { ref fields, body } = *val {
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
                    new_val = Some(ExprType::Resolved {
                        ident: None,
                        dest: body,
                    });
                }
            }
            if let ExprType::Let { .. } = *val {
                if let Some(new_val) = new_val {
                    *val = new_val;
                }
                return;
            }
        }

        // Recursive sets
        {
            let mut new_val = None;
            if let ExprType::Set {
                ref fields,
                recursive: true,
            } = *val
            {
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
                            *val.val.borrow_mut() = ExprType::Resolved {
                                ident: Some(*id),
                                dest: e,
                            };
                        }
                    } else {
                        bind!(val.id);
                    }
                }
                for id in added_to_scope {
                    scope.pop(id);
                }
                if !in_fn_body {
                    new_val = Some(ExprType::Set {
                        fields: fields.clone(),
                        recursive: false,
                    });
                }
            }
            if let ExprType::Set {
                recursive: true, ..
            } = *val
            {
                if let Some(new_val) = new_val {
                    *val = new_val;
                }
                return;
            }
        }

        // Functions
        {
            if let ExprType::Fn {
                func: FnType::Normal { ref param, body },
            } = *val
            {
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
            ExprType::Ident { .. }
            | ExprType::Let { .. }
            | ExprType::Set {
                recursive: true, ..
            }
            | ExprType::Fn {
                func: FnType::Normal { .. },
            } => {
                // handled above
                unreachable!();
            }
            ExprType::Null
            | ExprType::String { .. }
            | ExprType::Number { .. }
            | ExprType::Resolved { .. }
            | ExprType::Fn {
                func: FnType::BuiltIn { .. },
            }
            | ExprType::Bool { .. }
            | ExprType::Inherit => {
                // nothing to do
            }
            ExprType::Not { val }
            | ExprType::Neg { val }
            | ExprType::Stringify { val } => {
                bind!(val);
            }
            ExprType::And { lhs, rhs }
            | ExprType::Or { lhs, rhs }
            | ExprType::Add { lhs, rhs }
            | ExprType::Sub { lhs, rhs }
            | ExprType::Mul { lhs, rhs }
            | ExprType::Div {
                numer: lhs,
                denom: rhs,
                int: _,
            }
            | ExprType::Mod {
                numer: lhs,
                denom: rhs,
                int: _,
            }
            | ExprType::Gt { lhs, rhs }
            | ExprType::Lt { lhs, rhs }
            | ExprType::Ge { lhs, rhs }
            | ExprType::Le { lhs, rhs }
            | ExprType::Eq { lhs, rhs }
            | ExprType::Ne { lhs, rhs }
            | ExprType::Overlay {
                lower: lhs,
                upper: rhs,
            }
            | ExprType::Concat { lhs, rhs }
            | ExprType::Apl {
                func: lhs,
                arg: rhs,
            } => {
                bind!(lhs);
                bind!(rhs);
            }
            ExprType::Cond { cond, then, el } => {
                bind!(cond);
                bind!(then);
                bind!(el);
            }
            ExprType::Set {
                ref fields,
                recursive: false,
            } => {
                for (&name, &val) in fields.iter() {
                    let val = self.store.get_expr(val);
                    if val.is_inherit() {
                        if let Some(e) = scope.get(*name) {
                            *val.val.borrow_mut() = ExprType::Resolved {
                                ident: Some(*name),
                                dest: e,
                            };
                        }
                    } else {
                        bind!(val.id);
                    }
                }
            }
            ExprType::List { ref elements } => {
                for &element in elements.iter() {
                    bind!(element);
                }
            }
            ExprType::Test { base, path } => {
                bind!(base);
                bind!(path);
            }
            ExprType::Select {
                base,
                path,
                ref alt,
            } => {
                bind!(base);
                bind!(path);
                if let Some(alt) = *alt {
                    bind!(alt);
                }
            }
            ExprType::Path { ref path } => {
                for &seg in path.iter() {
                    bind!(seg);
                }
            }
        }
    }

    fn force_cond(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalCond(expr.id);

        let dest = if let ExprType::Cond { cond, then, el } = *val {
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

        *val = ExprType::Resolved { ident: None, dest };

        Ok(())
    }

    fn force_stringify(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let e = match *val {
            ExprType::Stringify { val } => val,
            _ => unreachable!(),
        };
        let dst = self.resolve_(e)?;
        let dst = dst.val.borrow();
        match *dst {
            ExprType::String { .. } => {
                *val = ExprType::Resolved {
                    ident: None,
                    dest: e,
                }
            }
            ExprType::Number { val: ref v } => {
                if !v.is_integer() {
                    return self
                        .error(expr.id, ErrorType::CannotStringifyNonInteger)
                        .ctx(ErrorContext::EvalStringify(expr.id));
                }
                let s = format!("{}", v);
                let content =
                    self.store.add_str(s.into_bytes().into_boxed_slice().into());
                *val = ExprType::String { content };
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
            ExprType::Apl { func, arg } => (func, arg),
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
                let ctx = ErrorContext::EvalFnPat(pat.span);
                let arg_fields = self.get_fields_(arg).ctx(ctx)?;
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

        *apl.val.borrow_mut() = ExprType::Resolved {
            ident: None,
            dest: new_body,
        };

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

            let (mut base, path, alt) = match *val {
                ExprType::Select { base, path, alt } => (base, path, alt),
                _ => unreachable!(),
            };

            let mut path = &self.get_path(path).ctx(ctx)?[..];
            let mut last_ok = base;

            while !path.is_empty() {
                base = match self.get_opt_field_(base, path[0]).ctx(ctx)? {
                    Some(f) => f,
                    _ => break,
                };
                last_ok = path[0];
                path = &path[1..];
            }

            if !path.is_empty() {
                let err_span = Span::new(self.span_(base).lo, self.span_(last_ok).hi);
                if let Some(alt) = alt {
                    self.force(alt)?;
                    alt
                } else {
                    let bad_path = self.resolve_(path[0]).unreachable();
                    let bad_path = bad_path.val.borrow();
                    let et = match *bad_path {
                        ExprType::String { content } => {
                            ErrorType::MissingSetField(content)
                        }
                        ExprType::Number { ref val } => {
                            ErrorType::MissingListField(val.clone())
                        }
                        _ => unreachable!(),
                    };
                    let mut e = self.error_(base, et);
                    e.span = err_span;
                    e.context.push(ctx);
                    return Err(e);
                }
            } else {
                self.force(base)?;
                base
            }
        };

        *expr.val.borrow_mut() = ExprType::Resolved {
            ident: None,
            dest: res,
        };

        Ok(())
    }
}

use crate::{
    types::{
        error::{ErrorContext, ErrorType},
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
                    Some(ErrorContext::EvalResolved { pointer }) if pointer == ex => {}
                    _ => context.push(ErrorContext::EvalResolved { pointer: *ex }),
                }
            }
            return Err(Error {
                span: expr.span,
                error: ErrorType::InfiniteRecursion { expr_id: eid },
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
            | ExprType::Bool { .. }
            | ExprType::Null
            | ExprType::List { .. }
            | ExprType::Fn { .. }
            | ExprType::String { .. }
            | ExprType::Map {
                recursive: false, ..
            } => {
                // Nothing to do
                Ok(())
            }
            ExprType::Std => {
                drop(borrow);
                self.force_std(&expr)
            }
            ExprType::Resolved { dest, .. } => self.force(dest),
            ExprType::Add { .. } => {
                drop(borrow);
                self.force_add(&expr)
            }
            ExprType::Sub { .. }
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
            | ExprType::Map {
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
            ExprType::Ident { .. } | ExprType::Inherit | ExprType::Path { .. } => self
                .eerror(
                    expr.id,
                    ErrorType::CannotEvaluateExpr {
                        kind: borrow.kind(),
                    },
                ),
        }
    }

    fn force_int(&mut self, expr: &Expr) -> Result {
        let ctx = ErrorContext::EvalArithmetic {
            arithmetic_expr: expr.id,
        };

        let num = |slf: &mut Self, v| match slf.get_number_(v) {
            Ok(v) => Ok(v),
            Err(mut e) => {
                e.context.push(ctx);
                Err(e)
            }
        };

        let new = match *expr.val.borrow() {
            ExprType::Sub { lhs, rhs } => &*num(self, lhs)? - &*num(self, rhs)?,
            ExprType::Mul { lhs, rhs } => &*num(self, lhs)? * &*num(self, rhs)?,
            ExprType::Div { numer, denom, int } => {
                let numer = num(self, numer)?;
                let denom_ = num(self, denom)?;
                if *denom_ == 0 {
                    return self.eerror(denom, ErrorType::DivideByZero).ctx(ctx);
                }
                let res = &*numer / &*denom_;
                if int {
                    res.trunc()
                } else {
                    res
                }
            }
            ExprType::Mod { numer, denom } => {
                let numer = num(self, numer)?;
                let denom_ = num(self, denom)?;
                if *denom_ == 0 {
                    return self.eerror(denom, ErrorType::DivideByZero).ctx(ctx);
                }
                &*numer % &*denom_
            }
            ExprType::Neg { val } => (*num(self, val)?).clone().neg(),
            _ => unreachable!(),
        };

        *expr.val.borrow_mut() = ExprType::Number { val: Rc::new(new) };

        Ok(())
    }

    fn force_bool(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalBool {
            boolean_expr: expr.id,
        };

        fn get<U>(u: Result<U>, ctx: ErrorContext) -> Result<U> {
            match u {
                Ok(v) => Ok(v),
                Err(mut e) => {
                    e.context.push(ctx);
                    Err(e)
                }
            }
        }
        let int = |slf: &mut Self, v| get(slf.get_number_(v), ctx);
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
            ExprType::Test { mut base, path } => {
                let mut path = &self.get_path(path).ctx(ctx)?[..];
                while !path.is_empty() {
                    base = match self.get_opt_field_(base, path[0]).ctx(ctx)? {
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
        let ctx = ErrorContext::EvalOverlay {
            overlay_expr: expr.id,
        };

        let new = if let ExprType::Overlay { lower, upper } = *val {
            let bottom = self.get_fields_(lower).ctx(ctx)?;
            let top = self.get_fields_(upper).ctx(ctx)?;

            let mut new = (*bottom).clone();

            for (&id, &val) in top.iter() {
                new.insert(id, val);
            }

            new.shrink_to_fit();

            ExprType::Map {
                fields: Rc::new(new),
                recursive: false,
            }
        } else {
            unreachable!();
        };

        *val = new;

        Ok(())
    }

    fn force_std(&mut self, expr: &Expr) -> Result {
        let std = match self.std {
            Some(std) => std,
            _ => {
                let std = self.create_std();
                self.std = Some(std);
                std
            }
        };
        *expr.val.borrow_mut() = ExprType::Resolved {
            ident: Some(self.store.add_str("std".as_bytes())),
            dest: std,
        };
        Ok(())
    }

    fn force_add(&mut self, expr: &Expr) -> Result {
        let mut val = expr.val.borrow_mut();
        let ctx = ErrorContext::EvalAdd { add_expr: expr.id };

        let new = if let ExprType::Add { lhs, rhs } = *val {
            let left = self.resolve_(lhs)?;
            self.force(rhs)?;
            let leftb = left.val.borrow();
            let ctx2 = |k| ErrorContext::EvalOtherExprKind {
                other_expr: lhs,
                other_expr_kind: k,
            };
            match *leftb {
                ExprType::Number { val: ref left } => {
                    let left = left.clone();
                    drop(leftb);
                    let ctx2 = ctx2(ExprKind::Number);
                    let right = self.get_number_(rhs).ctx(ctx2).ctx(ctx)?;
                    let new = &*left + &*right;
                    ExprType::Number { val: Rc::new(new) }
                }
                ExprType::String { content: left } => {
                    drop(leftb);
                    let ctx2 = ctx2(ExprKind::String);
                    let right = self.get_string_(rhs).ctx(ctx2).ctx(ctx)?;
                    let new = self.store.concat(left, right);
                    ExprType::String { content: new }
                }
                ExprType::List { elements: ref left } => {
                    let left = left.clone();
                    drop(leftb);
                    let ctx2 = ctx2(ExprKind::List);
                    let right = self.get_list_(rhs).ctx(ctx2).ctx(ctx)?;
                    if right.len() == 0 {
                        ExprType::List {
                            elements: left.clone(),
                        }
                    } else {
                        let mut left = (*left).to_vec();
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
                        .eerror(
                            lhs,
                            ErrorType::UnexpectedExprKind {
                                expected: &[ExprKind::String, ExprKind::List],
                                encountered: leftb.kind(),
                            },
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

        // Recursive maps
        {
            let mut new_val = None;
            if let ExprType::Map {
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
                    new_val = Some(ExprType::Map {
                        fields: fields.clone(),
                        recursive: false,
                    });
                }
            }
            if let ExprType::Map {
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
            | ExprType::Map {
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
                func: FnType::Native { .. },
            }
            | ExprType::Bool { .. }
            | ExprType::Std
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
            ExprType::Map {
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

        let dest = if let ExprType::Cond { cond, then, el } = *val {
            let ctx = ErrorContext::EvalCond { cond_expr: cond };
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
                        .eerror(expr.id, ErrorType::CannotStringifyNonInteger)
                        .ctx(ErrorContext::EvalStringify {
                            stringify_expr: expr.id,
                        });
                }
                let s = format!("{}", BigRational::from((**v).clone()));
                let content = self.store.add_str(s);
                *val = ExprType::String { content };
            }
            _ => {
                drop(val);
                return self
                    .eerror(
                        expr.id,
                        ErrorType::UnexpectedExprKind {
                            expected: &[ExprKind::String, ExprKind::Number],
                            encountered: dst.kind(),
                        },
                    )
                    .ctx(ErrorContext::EvalStringify {
                        stringify_expr: expr.id,
                    });
            }
        }
        Ok(())
    }

    fn force_apl(&mut self, apl: &Expr) -> Result {
        match self.force_apl_(apl) {
            Ok(()) => Ok(()),
            Err(mut e) => {
                e.context.push(ErrorContext::EvalApl { apl_expr: apl.id });
                Err(e)
            }
        }
    }

    fn force_apl_(&mut self, apl: &Expr) -> Result {
        let (func, arg) = match *apl.val.borrow() {
            ExprType::Apl { func, arg } => (func, arg),
            _ => unreachable!(),
        };

        let (pat, body) = match self.get_fn_(func)? {
            FnType::Normal { param, body } => (param, body),
            FnType::Native { func } => {
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
            FnParam::Pat { param_name, fields } => {
                let ctx = ErrorContext::EvalFnPat {
                    fn_pat_span: pat.span,
                };
                let arg_fields = self.get_fields_(arg).ctx(ctx)?;
                for (&id, &alt) in fields.iter() {
                    if let Some(&val) = arg_fields.get(&id) {
                        scope.bind(*id, val);
                    } else if let Some(alt) = alt {
                        scope.bind(*id, alt);
                    } else {
                        return self.eerror(
                            arg,
                            ErrorType::MissingArgument {
                                missing_parameter: id,
                            },
                        );
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
    /// Note that all expressions which are selected on have to be maps or an error is
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
            let ctx = ErrorContext::EvalSelect {
                select_expr: expr.id,
            };

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
                        ExprType::String { content } => ErrorType::MissingMapField {
                            field_name: content,
                        },
                        ExprType::Number { ref val } => {
                            ErrorType::MissingListField { index: val.clone() }
                        }
                        _ => unreachable!(),
                    };
                    let mut e = self.perror(base, et);
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

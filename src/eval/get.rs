use crate::types::{
    result::Result,
    store::StrId,
    tree::{ExprId, ExprKind, ExprType, FnType},
};
use std::rc::Rc;

use crate::{types::{diagnostic::ErrorType, result::ResultUtil}, Elang, ErrorContext, Spanned};
use num_rational::BigRational;
use num_traits::ToPrimitive;
use std::collections::HashMap;

impl Elang {
    pub(crate) fn get_bool_(&mut self, expr: ExprId) -> Result<bool> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Bool { val } => Ok(val),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExprType(&[ExprKind::Bool], val.kind()),
            ),
        }
    }

    pub(crate) fn get_string_(&mut self, expr: ExprId) -> Result<StrId> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::String { content } => Ok(content),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExprType(&[ExprKind::String], val.kind()),
            ),
        }
    }

    pub(crate) fn get_int_(&mut self, expr: ExprId) -> Result<Rc<BigRational>> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Number { ref val } => Ok(val.clone()),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExprType(&[ExprKind::Number], val.kind()),
            ),
        }
    }

    pub(crate) fn get_list_(&mut self, expr: ExprId) -> Result<Rc<[ExprId]>> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::List { ref elements } => Ok(elements.clone()),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExprType(&[ExprKind::List], val.kind()),
            ),
        }
    }

    pub(crate) fn get_field_int(
        &mut self,
        expr: ExprId,
        selector: ExprId,
    ) -> Result<ExprId> {
        let field = self.get_opt_field_(expr, selector)?;

        if let Some(f) = field {
            return Ok(f);
        }

        let sel = self.resolve_(selector).unwrap();
        let sel = sel.val.borrow();

        self.error(
            expr,
            match *sel {
                ExprType::Ident { name } => ErrorType::MissingSetField(name),
                ExprType::Number { ref val } => ErrorType::MissingListField(val.clone()),
                _ => unreachable!(),
            },
        )
    }

    pub(crate) fn get_opt_field_(
        &mut self,
        base_: ExprId,
        sel_: ExprId,
    ) -> Result<Option<ExprId>> {
        let base = self.resolve_(base_)?;
        let sel = self.resolve_(sel_)?;
        let base_val = base.val.borrow();
        let sel_val = sel.val.borrow();

        match (&*base_val, &*sel_val) {
            (ExprType::Set { ref fields, .. }, ExprType::String { content }) => {
                if let Some(&val) = fields.get(content) {
                    return Ok(Some(val));
                }
                Ok(None)
            }
            (ExprType::List { ref elements }, ExprType::Number { ref val }) => {
                if val.is_integer() {
                    if let Some(val) = val.to_integer().to_usize() {
                        if val < elements.len() {
                            return Ok(Some(elements[val]));
                        }
                    }
                }
                Ok(None)
            }
            _ => {
                match *sel_val {
                    ExprType::String { .. } | ExprType::Number { .. } => {}
                    _ => {
                        return self.error(
                            sel_,
                            ErrorType::UnexpectedExprType(
                                &[ExprKind::String, ExprKind::Number],
                                sel_val.kind(),
                            ),
                        );
                    }
                }
                let (et, ot) = match *base_val {
                    ExprType::Set { .. } => (&[ExprKind::String], ExprKind::Set),
                    ExprType::List { .. } => (&[ExprKind::Number], ExprKind::List),
                    _ => {
                        return self.error(
                            base_,
                            ErrorType::UnexpectedExprType(
                                &[ExprKind::Set, ExprKind::List],
                                base_val.kind(),
                            ),
                        )
                    }
                };
                self.error(sel_, ErrorType::UnexpectedExprType(et, sel_val.kind()))
                    .ctx(ErrorContext::EvalOtherExprType(base_, ot))
            }
        }
    }

    pub(crate) fn get_fields_(&mut self, expr: ExprId) -> Result<Rc<HashMap<Spanned<StrId>, ExprId>>> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Set { ref fields, .. } => Ok(fields.clone()),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExprType(
                    &[ExprKind::Set],
                    val.kind(),
                ),
            ),
        }
    }

    pub(crate) fn get_func(&mut self, expr: ExprId) -> Result<FnType> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Fn { ref func } => Ok(func.clone()),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExprType(&[ExprKind::Fn], val.kind()),
            ),
        }
    }

    pub(crate) fn get_path(&mut self, expr: ExprId) -> Result<Rc<[ExprId]>> {
        let e = self.store.get_expr(expr);
        let val = e.val.borrow();
        match *val {
            ExprType::Path { ref path } => Ok(path.clone()),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExprType(&[ExprKind::Path], val.kind()),
            ),
        }
    }
}

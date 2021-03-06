use crate::types::{
    result::Result,
    store::StrId,
    tree::{ExprId, ExprKind, ExprType, FnType},
};
use std::rc::Rc;

use crate::{
    types::{error::ErrorType, result::ResultUtil},
    Elang, Spanned,
};
use num_traits::ToPrimitive;
use std::collections::HashMap;
use crate::types::error::ErrorContext;
use crate::types::num::Number;
use std::convert::TryFrom;

impl Elang {
    pub(crate) fn get_bool_(&mut self, expr: ExprId) -> Result<bool> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Bool { val } => Ok(val),
            _ => self.eerror(
                expr,
                ErrorType::UnexpectedExprKind {
                    expected: &[ExprKind::Bool],
                    encountered: val.kind(),
                },
            ),
        }
    }

    pub(crate) fn get_string_(&mut self, expr: ExprId) -> Result<StrId> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::String { content } => Ok(content),
            _ => self.eerror(
                expr,
                ErrorType::UnexpectedExprKind {
                    expected: &[ExprKind::String],
                    encountered: val.kind(),
                },
            ),
        }
    }

    pub(crate) fn get_number_(&mut self, expr: ExprId) -> Result<Rc<Number>> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Number { ref val } => Ok(val.clone()),
            _ => self.eerror(
                expr,
                ErrorType::UnexpectedExprKind {
                    expected: &[ExprKind::Number],
                    encountered: val.kind(),
                },
            ),
        }
    }

    pub(crate) fn get_null_(&mut self, expr_id: ExprId) -> Result {
        let res = self.resolve_(expr_id)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Null => Ok(()),
            _ => self.eerror(
                expr_id,
                ErrorType::UnexpectedExprKind {
                    expected: &[ExprKind::Null],
                    encountered: val.kind(),
                },
            ),
        }
    }

    pub(crate) fn get_list_(&mut self, expr: ExprId) -> Result<Rc<[ExprId]>> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::List { ref elements } => Ok(elements.clone()),
            _ => self.eerror(
                expr,
                ErrorType::UnexpectedExprKind {
                    expected: &[ExprKind::List],
                    encountered: val.kind(),
                },
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

        self.eerror(
            expr,
            match *sel {
                ExprType::Ident { name } => {
                    ErrorType::MissingMapField { field_name: name }
                }
                ExprType::Number { ref val } => {
                    ErrorType::MissingListField { index: val.clone() }
                }
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
            (ExprType::Map { ref fields, .. }, ExprType::String { content }) => {
                if let Some(&val) = fields.get(content) {
                    return Ok(Some(val));
                }
                Ok(None)
            }
            (ExprType::List { ref elements }, ExprType::Number { ref val }) => {
                if let Ok(val) = usize::try_from(&**val) {
                    if val < elements.len() {
                        return Ok(Some(elements[val]));
                    }
                }
                Ok(None)
            }
            _ => {
                match *sel_val {
                    ExprType::String { .. } | ExprType::Number { .. } => {}
                    _ => {
                        return self.eerror(
                            sel_,
                            ErrorType::UnexpectedExprKind {
                                expected: &[ExprKind::String, ExprKind::Number],
                                encountered: sel_val.kind(),
                            },
                        );
                    }
                }
                let (et, ot) = match *base_val {
                    ExprType::Map { .. } => (&[ExprKind::String], ExprKind::Map),
                    ExprType::List { .. } => (&[ExprKind::Number], ExprKind::List),
                    _ => {
                        return self.eerror(
                            base_,
                            ErrorType::UnexpectedExprKind {
                                expected: &[ExprKind::Map, ExprKind::List],
                                encountered: base_val.kind(),
                            },
                        )
                    }
                };
                self.eerror(
                    sel_,
                    ErrorType::UnexpectedExprKind {
                        expected: et,
                        encountered: sel_val.kind(),
                    },
                )
                .ctx(ErrorContext::EvalOtherExprKind {
                    other_expr: base_,
                    other_expr_kind: ot,
                })
            }
        }
    }

    pub(crate) fn get_fields_(
        &mut self,
        expr: ExprId,
    ) -> Result<Rc<HashMap<Spanned<StrId>, ExprId>>> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Map { ref fields, .. } => Ok(fields.clone()),
            _ => self.eerror(
                expr,
                ErrorType::UnexpectedExprKind {
                    expected: &[ExprKind::Map],
                    encountered: val.kind(),
                },
            ),
        }
    }

    pub(crate) fn get_fn_(&mut self, expr: ExprId) -> Result<FnType> {
        let res = self.resolve_(expr)?;
        let val = res.val.borrow();
        match *val {
            ExprType::Fn { ref func } => Ok(func.clone()),
            _ => self.eerror(
                expr,
                ErrorType::UnexpectedExprKind {
                    expected: &[ExprKind::Fn],
                    encountered: val.kind(),
                },
            ),
        }
    }

    pub(crate) fn get_path(&mut self, expr: ExprId) -> Result<Rc<[ExprId]>> {
        let e = self.store.get_expr(expr);
        let val = e.val.borrow();
        match *val {
            ExprType::Path { ref path } => Ok(path.clone()),
            _ => self.eerror(
                expr,
                ErrorType::UnexpectedExprKind {
                    expected: &[ExprKind::Path],
                    encountered: val.kind(),
                },
            ),
        }
    }
}

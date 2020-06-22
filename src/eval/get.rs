use crate::types::{
    result::Result,
    store::StrId,
    tree::{ExprId, FnType, Selector, Value, ValueType},
};
use std::rc::Rc;

use crate::{eval::Eval, Fields};
use std::collections::HashMap;
use crate::types::diagnostic::ErrorType;

impl Eval {
    /// Forces the expression and tries to interpret the created datatype expression as a
    /// boolean.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a boolean, an error message is printed and an
    /// error is returned.
    pub fn get_bool(&mut self, expr: ExprId) -> Result<bool> {
        let res = self.resolve(expr)?;
        let val = res.val.borrow();
        match *val {
            Value::Bool(b) => Ok(b),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExpr(&[ValueType::Bool], val.ty()),
            ),
        }
    }

    pub fn get_string(&mut self, expr: ExprId) -> Result<StrId> {
        let res = self.resolve(expr)?;
        let val = res.val.borrow();
        match *val {
            Value::String(s) => Ok(s),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExpr(&[ValueType::String], val.ty()),
            ),
        }
    }

    /// Forces the expression and tries to interpret the created datatype expression as an
    /// integer.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not an integer, an error message is printed and an
    /// error is returned.
    pub fn get_int(&mut self, expr: ExprId) -> Result<i64> {
        let res = self.resolve(expr)?;
        let val = res.val.borrow();
        match *val {
            Value::Integer(i) => Ok(i),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExpr(&[ValueType::Integer], val.ty()),
            ),
        }
    }

    /// Forces the expression and tries to interpret the created datatype expression as a
    /// list.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a list, an error message is printed and an error
    /// is returned.
    pub fn get_list(&mut self, expr: ExprId) -> Result<Rc<[ExprId]>> {
        let res = self.resolve(expr)?;
        let val = res.val.borrow();
        match *val {
            Value::List(ref l) => Ok(l.clone()),
            Value::Null => Ok(Rc::from(vec![].into_boxed_slice())),
            _ => self.error(expr, ErrorType::UnexpectedExpr(&[ValueType::List], val.ty())),
        }
    }

    /// Forces the expression, tries to interpret the created datatype expression as a
    /// set, or overlay, and tries to retrieve a field.
    ///
    /// [argument, name]
    /// The name of the field to retrieve.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a set or an overlay, an error message is printed
    /// and an error is returned. If the set does not contain the field, `None` is
    /// returned but no error is printed.
    pub fn get_opt_field(
        &mut self,
        expr: ExprId,
        selector: &Selector,
        out: Option<&mut Selector>,
    ) -> Result<Option<ExprId>> {
        // Note: If we wanted to force the field in this function, we
        // would first have to drop the borrow since the field might refer
        // to this set. For example
        //
        // ----
        // let
        //     a = { x = a.y, y = 0 };
        // in
        //     a.x
        // ----

        let sel = match *selector {
            Selector::Expr(e) => {
                let expr = self.resolve(e)?;
                let res = expr.val.borrow();
                match *res {
                    Value::String(s) => Selector::Ident(s),
                    Value::Integer(i) => {
                        if (isize::max_value() as i64) < i {
                            return self.error(e, ErrorType::OutOfBoundsSelector(i));
                        }
                        Selector::Integer(i as usize)
                    }
                    _ => {
                        return self.error(
                            e,
                            ErrorType::UnexpectedExpr(&[ValueType::Integer, ValueType::String], res.ty()),
                        )
                    }
                }
            }
            _ => selector.clone(),
        };

        self.get_field_(expr, &sel, out)
    }

    pub fn get_field(
        &mut self,
        expr: ExprId,
        selector: &Selector,
        out: Option<&mut Selector>,
    ) -> Result<ExprId> {
        let mut eval_sel = Selector::Integer(0);
        let field = self.get_opt_field(expr, selector, Some(&mut eval_sel))?;

        if let Some(f) = field {
            return Ok(f);
        }

        let _ = out.map(|o| *o = eval_sel.clone());

        self.error(
            expr,
            match eval_sel {
                Selector::Ident(i) => ErrorType::MissingSetField(i),
                Selector::Integer(i) => ErrorType::MissingListField(i),
                _ => unreachable!(),
            },
        )
    }

    /// Like `get_field` but expects the selector to not be in expression form.
    fn get_field_(
        &mut self,
        expr: ExprId,
        sel: &Selector,
        out: Option<&mut Selector>,
    ) -> Result<Option<ExprId>> {
        let res = self.resolve(expr)?;
        let val = res.val.borrow();

        match (&*val, sel) {
            (&Value::Set(ref fields, _), &Selector::Ident(name)) => {
                if let Some(&(_, val)) = fields.get(&name) {
                    return Ok(Some(val));
                }
                let _ = out.map(|o| *o = Selector::Ident(name));
                Ok(None)
            }
            (&Value::List(ref fields), &Selector::Integer(i)) => {
                if fields.len() > i {
                    Ok(Some(fields[i]))
                } else {
                    let _ = out.map(|o| *o = Selector::Integer(i));
                    Ok(None)
                }
            }
            (&Value::Null, _) => Ok(None),
            _ => self.error(
                expr,
                if let Selector::Ident(..) = sel {
                    ErrorType::UnexpectedExpr(&[ValueType::Set], val.ty())
                } else {
                    ErrorType::UnexpectedExpr(&[ValueType::List], val.ty())
                },
            ),
        }
    }

    /// Forces the expression, tries to interpret the created datatype expression as a
    /// set, and returns all of its fields.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a set, an error message is printed and an error
    /// is returned.
    pub fn get_fields(&mut self, expr: ExprId) -> Result<Fields> {
        let res = self.resolve(expr)?;
        let val = res.val.borrow();
        match *val {
            Value::Set(ref fields, _) => Ok(fields.clone()),
            Value::Null => Ok(Rc::new(HashMap::new())),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExpr(
                    &[ValueType::Set, ValueType::Null],
                    val.ty()
                ),
            ),
        }
    }

    /// Forces the expression, tries to interpret the created datatype expression as a
    /// function, and returns its pattern and body.
    ///
    /// [argument, span]
    /// The span of the expression.
    ///
    /// = Remarks
    ///
    /// If the datatype expression is not a function, an error message is printed and an
    /// error is returned.
    pub fn get_func(&mut self, expr: ExprId) -> Result<FnType> {
        let res = self.resolve(expr)?;
        let val = res.val.borrow();
        match *val {
            Value::Fn(ref f) => Ok(f.clone()),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExpr(&[ValueType::Fn], val.ty()),
            ),
        }
    }

    pub fn get_path(&mut self, expr: ExprId) -> Result<Rc<[ExprId]>> {
        let e = self.store.get_expr(expr);
        let val = e.val.borrow();
        match *val {
            Value::Path(ref f) => Ok(f.clone()),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExpr(&[ValueType::Path], val.ty()),
            ),
        }
    }

    pub fn get_selector(&mut self, expr: ExprId) -> Result<Selector> {
        let e = self.store.get_expr(expr);
        let val = e.val.borrow();
        match *val {
            Value::Selector(ref s) => Ok(s.clone()),
            _ => self.error(
                expr,
                ErrorType::UnexpectedExpr(&[ValueType::Selector], val.ty()),
            ),
        }
    }
}

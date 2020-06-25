use crate::types::{
    store::Store,
    tree::{ExprId, FnArg, FnType, Selector, Value},
};
use std::{collections::HashMap, io, io::Write};

pub fn print_tree<W: Write>(e: ExprId, store: &Store, w: &mut W) -> io::Result<()> {
    let mut map = HashMap::new();
    let mut id = 0;
    write!(w, "digraph {{ graph [ordering=\"out\"];")?;
    print_tree_(e, store, w, &mut id, &mut map)?;
    write!(w, "}}")
}

fn print_tree_<W: Write>(
    e: ExprId,
    store: &Store,
    w: &mut W,
    id: &mut u32,
    map: &mut HashMap<ExprId, u32>,
) -> io::Result<()> {
    macro_rules! pt {
        ($ex:expr) => {
            print_tree_($ex, store, w, id, map)
        };
    }
    macro_rules! unary {
        ($label:expr, $ex:expr) => {{
            write!(
                w,
                "{} [shape=\"box\", label=\"{}\"]; {} -> {};",
                id,
                $label,
                id,
                *id + 1
            )?;
            *id += 1;
            pt!($ex)
        }};
    }

    macro_rules! binary {
        ($label:expr, $left:expr, $right:expr) => {{
            let sid = *id;
            write!(
                w,
                "{} [shape=\"box\", label=\"{}\"]; {} -> {};",
                id,
                $label,
                id,
                sid + 1
            )?;
            *id += 1;
            pt!($left)?;
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!($right)
        }};
    }

    map.insert(e, *id);

    let val = store.get_expr(e);
    let val = val.val.borrow();

    match *val {
        Value::Inherit => write!(w, "{} [shape=\"box\", label=\"Inherit\"];", id),
        Value::Number(ref i) => write!(w, "{} [label=\"{}\"];", id, i),
        Value::Ident(i) => {
            write!(w, "{} [label=\"", id)?;
            w.write_all(&store.get_str(i))?;
            write!(w, "\"];")
        }
        Value::String(s) => {
            write!(w, "{} [label=\"\\\"", id)?;
            w.write_all(&store.get_str(s))?;
            write!(w, "\\\"\"];")
        }
        Value::Resolved(i, e) => {
            if let Some(i) = i {
                write!(w, "{} [label=\"", id)?;
                w.write_all(&store.get_str(i))?;
                write!(w, "\"];")?;
            } else {
                write!(w, "{} [shape=\"box\", label=\"resolved\"];", id)?;
            }
            if let Some(val) = map.get(&e).copied() {
                write!(w, "{} -> {};", id, val)
            } else {
                write!(w, "{} -> {};", id, *id + 1)?;
                *id += 1;
                pt!(e)
            }
        }
        Value::Set(ref els, rec) => {
            if rec {
                write!(w, "{} [shape=\"box\", label=\"rec set\"];", id)?;
            } else {
                write!(w, "{} [shape=\"box\", label=\"set\"];", id)?;
            }
            let sid = *id;
            for el in els.iter() {
                *id += 1;
                write!(w, "{} [label=\"", id)?;
                w.write_all(&store.get_str(*el.0))?;
                write!(w, "\"];{} -> {};", sid, id)?;
                write!(w, "{} -> {};", id, *id + 1)?;
                *id += 1;
                pt!((el.1).1)?;
            }
            Ok(())
        }
        Value::And(l, r) => binary!("&&", l, r),
        Value::Or(l, r) => binary!("||", l, r),
        Value::Not(e) => unary!("!", e),
        Value::Add(l, r) => binary!("+", l, r),
        Value::Sub(l, r) => binary!("-", l, r),
        Value::Mul(l, r) => binary!("*", l, r),
        Value::Div(l, r) => binary!("/", l, r),
        Value::Mod(l, r) => binary!("%", l, r),
        Value::Gt(l, r) => binary!(">", l, r),
        Value::Lt(l, r) => binary!("<", l, r),
        Value::Ge(l, r) => binary!(">=", l, r),
        Value::Le(l, r) => binary!("<=", l, r),
        Value::Eq(l, r) => binary!("==", l, r),
        Value::Ne(l, r) => binary!("!=", l, r),
        Value::Impl(l, r) => binary!("->", l, r),
        Value::Overlay(l, r) => binary!("//", l, r),
        Value::Concat(l, r) => binary!("++", l, r),
        Value::Apl(l, r) => binary!("$", l, r),
        Value::Neg(e) => unary!("-", e),
        Value::Cond(cond, then, el) => {
            let sid = *id;
            write!(
                w,
                "{} [shape=\"box\", label=\"if\"];{} -> {};",
                id,
                id,
                sid + 1
            )?;
            *id += 1;
            pt!(cond)?;
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(then)?;
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(el)
        }
        Value::Bool(b) => write!(w, "{} [label=\"{}\"];", id, b),
        Value::Null => write!(w, "{} [label=\"null\"];", id),
        Value::Path(ref path) => {
            let sid = *id;
            write!(w, "{} [shape=\"box\", label=\"path\"];", sid)?;
            for &seg in path.iter() {
                *id += 1;
                write!(w, "{} -> {};", sid, id)?;
                pt!(seg)?;
            }
            Ok(())
        }
        Value::Selector(ref ty) => match *ty {
            Selector::Ident(i) => {
                write!(w, "{} [label=\"", id)?;
                w.write_all(&store.get_str(i))?;
                write!(w, "\"];")
            }
            Selector::Number(ref i) => write!(w, "{} [label=\"{}\"];", id, i),
            Selector::Expr(e) => pt!(e),
        },
        Value::Test(e, path) => {
            let sid = *id;
            write!(
                w,
                "{} [shape=\"box\", label=\"test\"];{} -> {};",
                sid,
                sid,
                sid + 1
            )?;
            *id += 1;
            pt!(e)?;
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(path)
        }
        Value::Select(e, path, alt) => {
            let sid = *id;
            write!(
                w,
                "{} [shape=\"box\", label=\"select\"];{} -> {};",
                sid,
                sid,
                sid + 1
            )?;
            *id += 1;
            pt!(e)?;
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(path)?;
            if let Some(alt) = alt {
                *id += 1;
                write!(w, "{} -> {};", sid, id)?;
                pt!(alt)?;
            }
            Ok(())
        }
        Value::Stringify(e) => unary!("stringify", e),
        Value::List(ref els) => {
            write!(w, "{} [shape=\"box\", label=\"list\"];", id)?;
            let sid = *id;
            for &el in els.iter() {
                *id += 1;
                write!(w, "{} -> {};", sid, id)?;
                pt!(el)?;
            }
            Ok(())
        }
        Value::Let(ref lets, e) => {
            let sid = *id;
            *id += 1;
            let vid = *id;
            write!(
                w,
                "{} [shape=\"box\", label=\"let\"]; {} -> {};",
                sid, sid, vid
            )?;
            write!(w, "{} [shape=\"box\", label=\"vars\"];", vid)?;
            for el in lets.iter() {
                *id += 1;
                write!(w, "{} [label=\"", id)?;
                w.write_all(&store.get_str(*el.0))?;
                write!(w, "\"];{} -> {}; {} -> {};", vid, id, id, *id + 1)?;
                *id += 1;
                pt!((el.1).1)?;
            }
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(e)
        }
        Value::Fn(FnType::Normal(ref arg, body)) => {
            let sid = *id;
            *id += 1;
            write!(
                w,
                "{} [shape=\"box\", label=\"fn\"]; {} -> {};",
                sid, sid, id
            )?;
            match arg.val {
                FnArg::Ident(i) => {
                    write!(w, "{} [label=\"", id)?;
                    w.write_all(&store.get_str(i))?;
                    write!(w, "\"];")?;
                }
                FnArg::Pat(i, ref args, wild) => {
                    let name = match i {
                        Some(i) => store.get_str(i.val),
                        _ => vec![].into_boxed_slice().into(),
                    };
                    write!(w, "{} [shape=\"box\", label=\"", id)?;
                    w.write_all(&name)?;
                    write!(w, " @\"];")?;
                    let aid = *id;
                    for arg in args.iter() {
                        *id += 1;
                        write!(w, "{} [label=\"", id)?;
                        w.write_all(&store.get_str(*arg.0))?;
                        write!(w, "\"];{} -> {};", aid, id)?;
                    }
                    if wild {
                        *id += 1;
                        write!(w, "{} [label=\"..\"];{} -> {};", id, aid, id)?;
                    }
                }
            }
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(body)
        }
        Value::Fn(FnType::BuiltIn(..)) => {
            write!(w, "{} [shape=\"box\", label=\"built-in fn\"];", *id)
        }
    }
}

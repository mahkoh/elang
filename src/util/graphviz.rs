use crate::types::{
    store::Store,
    tree::{ExprId, ExprType, FnParam, FnType},
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
        ExprType::Inherit => write!(w, "{} [shape=\"box\", label=\"Inherit\"];", id),
        ExprType::Number(ref i) => write!(w, "{} [label=\"{}\"];", id, i),
        ExprType::Ident(i) => {
            write!(w, "{} [label=\"", id)?;
            w.write_all(&store.get_str(i))?;
            write!(w, "\"];")
        }
        ExprType::String(s) => {
            write!(w, "{} [label=\"\\\"", id)?;
            w.write_all(&store.get_str(s))?;
            write!(w, "\\\"\"];")
        }
        ExprType::Resolved(i, e) => {
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
        ExprType::Set(ref els, rec) => {
            if rec {
                write!(w, "{} [shape=\"box\", label=\"rec set\"];", id)?;
            } else {
                write!(w, "{} [shape=\"box\", label=\"set\"];", id)?;
            }
            let sid = *id;
            for el in els.iter() {
                *id += 1;
                write!(w, "{} [label=\"", id)?;
                w.write_all(&store.get_str(**el.0))?;
                write!(w, "\"];{} -> {};", sid, id)?;
                write!(w, "{} -> {};", id, *id + 1)?;
                *id += 1;
                pt!(*el.1)?;
            }
            Ok(())
        }
        ExprType::And(l, r) => binary!("&&", l, r),
        ExprType::Or(l, r) => binary!("||", l, r),
        ExprType::Not(e) => unary!("!", e),
        ExprType::Add(l, r) => binary!("+", l, r),
        ExprType::Sub(l, r) => binary!("-", l, r),
        ExprType::Mul(l, r) => binary!("*", l, r),
        ExprType::Div(l, r) => binary!("/", l, r),
        ExprType::Mod(l, r) => binary!("%", l, r),
        ExprType::Gt(l, r) => binary!(">", l, r),
        ExprType::Lt(l, r) => binary!("<", l, r),
        ExprType::Ge(l, r) => binary!(">=", l, r),
        ExprType::Le(l, r) => binary!("<=", l, r),
        ExprType::Eq(l, r) => binary!("==", l, r),
        ExprType::Ne(l, r) => binary!("!=", l, r),
        ExprType::Impl(l, r) => binary!("->", l, r),
        ExprType::Overlay(l, r) => binary!("//", l, r),
        ExprType::Concat(l, r) => binary!("++", l, r),
        ExprType::Apl(l, r) => binary!("$", l, r),
        ExprType::Neg(e) => unary!("-", e),
        ExprType::Cond(cond, then, el) => {
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
        ExprType::Bool(b) => write!(w, "{} [label=\"{}\"];", id, b),
        ExprType::Null => write!(w, "{} [label=\"null\"];", id),
        ExprType::Path(ref path) => {
            let sid = *id;
            write!(w, "{} [shape=\"box\", label=\"path\"];", sid)?;
            for &seg in path.iter() {
                *id += 1;
                write!(w, "{} -> {};", sid, id)?;
                pt!(seg)?;
            }
            Ok(())
        }
        ExprType::Test(e, path) => {
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
        ExprType::Select(e, path, alt) => {
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
        ExprType::Stringify(e) => unary!("stringify", e),
        ExprType::List(ref els) => {
            write!(w, "{} [shape=\"box\", label=\"list\"];", id)?;
            let sid = *id;
            for &el in els.iter() {
                *id += 1;
                write!(w, "{} -> {};", sid, id)?;
                pt!(el)?;
            }
            Ok(())
        }
        ExprType::Let(ref lets, e) => {
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
                w.write_all(&store.get_str(**el.0))?;
                write!(w, "\"];{} -> {}; {} -> {};", vid, id, id, *id + 1)?;
                *id += 1;
                pt!(*el.1)?;
            }
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(e)
        }
        ExprType::Fn(FnType::Normal { ref param, body }) => {
            let sid = *id;
            *id += 1;
            write!(
                w,
                "{} [shape=\"box\", label=\"fn\"]; {} -> {};",
                sid, sid, id
            )?;
            match param.val {
                FnParam::Ident { param_name } => {
                    write!(w, "{} [label=\"", id)?;
                    w.write_all(&store.get_str(param_name))?;
                    write!(w, "\"];")?;
                }
                FnParam::Pat {
                    param_name,
                    ref fields,
                    wild,
                } => {
                    let name = match param_name {
                        Some(i) => store.get_str(i.val),
                        _ => vec![].into_boxed_slice().into(),
                    };
                    write!(w, "{} [shape=\"box\", label=\"", id)?;
                    w.write_all(&name)?;
                    write!(w, " @\"];")?;
                    let aid = *id;
                    for arg in fields.iter() {
                        *id += 1;
                        write!(w, "{} [label=\"", id)?;
                        w.write_all(&store.get_str(**arg.0))?;
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
        ExprType::Fn(FnType::BuiltIn { .. }) => {
            write!(w, "{} [shape=\"box\", label=\"built-in fn\"];", *id)
        }
    }
}

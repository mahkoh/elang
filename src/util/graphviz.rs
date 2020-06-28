#![allow(unused)]

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
        ExprType::Number { ref val } => write!(w, "{} [label=\"{}\"];", id, val),
        ExprType::Ident { name } => {
            write!(w, "{} [label=\"", id)?;
            w.write_all(&store.get_str(name))?;
            write!(w, "\"];")
        }
        ExprType::String { content } => {
            write!(w, "{} [label=\"\\\"", id)?;
            w.write_all(&store.get_str(content))?;
            write!(w, "\\\"\"];")
        }
        ExprType::Resolved { ident, dest } => {
            if let Some(i) = ident {
                write!(w, "{} [label=\"", id)?;
                w.write_all(&store.get_str(i))?;
                write!(w, "\"];")?;
            } else {
                write!(w, "{} [shape=\"box\", label=\"resolved\"];", id)?;
            }
            if let Some(val) = map.get(&dest).copied() {
                write!(w, "{} -> {};", id, val)
            } else {
                write!(w, "{} -> {};", id, *id + 1)?;
                *id += 1;
                pt!(e)
            }
        }
        ExprType::Map {
            ref fields,
            recursive,
        } => {
            if recursive {
                write!(w, "{} [shape=\"box\", label=\"rec map\"];", id)?;
            } else {
                write!(w, "{} [shape=\"box\", label=\"map\"];", id)?;
            }
            let sid = *id;
            for el in fields.iter() {
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
        ExprType::And { lhs, rhs } => binary!("&&", lhs, rhs),
        ExprType::Or { lhs, rhs } => binary!("||", lhs, rhs),
        ExprType::Not { val } => unary!("!", val),
        ExprType::Add { lhs, rhs } => binary!("+", lhs, rhs),
        ExprType::Sub { lhs, rhs } => binary!("-", lhs, rhs),
        ExprType::Mul { lhs, rhs } => binary!("*", lhs, rhs),
        ExprType::Div {
            numer,
            denom,
            int: true,
        } => binary!("int/", numer, denom),
        ExprType::Div {
            numer,
            denom,
            int: false,
        } => binary!("/", numer, denom),
        ExprType::Mod {
            numer,
            denom,
        } => binary!("%", numer, denom),
        ExprType::Gt { lhs, rhs } => binary!(">", lhs, rhs),
        ExprType::Lt { lhs, rhs } => binary!("<", lhs, rhs),
        ExprType::Ge { lhs, rhs } => binary!(">=", lhs, rhs),
        ExprType::Le { lhs, rhs } => binary!("<=", lhs, rhs),
        ExprType::Eq { lhs, rhs } => binary!("==", lhs, rhs),
        ExprType::Ne { lhs, rhs } => binary!("!=", lhs, rhs),
        ExprType::Overlay { lower, upper } => binary!("\\\\", lower, upper),
        ExprType::Concat { lhs, rhs } => binary!("++", lhs, rhs),
        ExprType::Apl { func, arg } => binary!("$", func, arg),
        ExprType::Neg { val } => unary!("-", val),
        ExprType::Cond { cond, then, el } => {
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
        ExprType::Bool { val } => write!(w, "{} [label=\"{}\"];", id, val),
        ExprType::Null => write!(w, "{} [label=\"null\"];", id),
        ExprType::Path { ref path } => {
            let sid = *id;
            write!(w, "{} [shape=\"box\", label=\"path\"];", sid)?;
            for &seg in path.iter() {
                *id += 1;
                write!(w, "{} -> {};", sid, id)?;
                pt!(seg)?;
            }
            Ok(())
        }
        ExprType::Test { base, path } => {
            let sid = *id;
            write!(
                w,
                "{} [shape=\"box\", label=\"test\"];{} -> {};",
                sid,
                sid,
                sid + 1
            )?;
            *id += 1;
            pt!(base)?;
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(path)
        }
        ExprType::Select { base, path, alt } => {
            let sid = *id;
            write!(
                w,
                "{} [shape=\"box\", label=\"select\"];{} -> {};",
                sid,
                sid,
                sid + 1
            )?;
            *id += 1;
            pt!(base)?;
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
        ExprType::Stringify { val } => unary!("stringify", val),
        ExprType::List { ref elements } => {
            write!(w, "{} [shape=\"box\", label=\"list\"];", id)?;
            let sid = *id;
            for &el in elements.iter() {
                *id += 1;
                write!(w, "{} -> {};", sid, id)?;
                pt!(el)?;
            }
            Ok(())
        }
        ExprType::Let { ref fields, body } => {
            let sid = *id;
            *id += 1;
            let vid = *id;
            write!(
                w,
                "{} [shape=\"box\", label=\"let\"]; {} -> {};",
                sid, sid, vid
            )?;
            write!(w, "{} [shape=\"box\", label=\"vars\"];", vid)?;
            for el in fields.iter() {
                *id += 1;
                write!(w, "{} [label=\"", id)?;
                w.write_all(&store.get_str(**el.0))?;
                write!(w, "\"];{} -> {}; {} -> {};", vid, id, id, *id + 1)?;
                *id += 1;
                pt!(*el.1)?;
            }
            *id += 1;
            write!(w, "{} -> {};", sid, id)?;
            pt!(body)
        }
        ExprType::Fn {
            func: FnType::Normal { ref param, body },
        } => {
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
        ExprType::Fn {
            func: FnType::BuiltIn { .. },
        } => write!(w, "{} [shape=\"box\", label=\"built-in fn\"];", *id),
    }
}

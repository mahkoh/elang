use elang::{Diagnostic, Elang, Error, ErrorType, ExprId, ExprType};
use num_rational::BigRational;
use std::{
    fmt,
    fmt::{Display, Formatter},
    fs::DirEntry,
    os::unix::ffi::OsStrExt,
    rc::Rc,
};

#[test]
fn eval() {
    let mut has_errors = false;
    for dir in std::fs::read_dir("tests/eval").unwrap() {
        let dir = dir.unwrap();
        has_errors |= test(dir);
    }
    if has_errors {
        panic!("there were errors");
    }
}

fn test(dir: DirEntry) -> bool {
    let path = dir.path();
    let in_path = path.join("in.exl");
    let out_path = path.join("out.json");
    let out: serde_json::Value =
        serde_json::from_str(&std::fs::read_to_string(&out_path).unwrap()).unwrap();
    let in_bytes: Rc<[u8]> =
        Rc::from(std::fs::read(&in_path).unwrap().into_boxed_slice());

    println!("testing {}", in_path.display());

    let mut diag = Diagnostic::new();
    let lo = diag.add_src(
        Rc::from(in_path.as_os_str().as_bytes().to_vec().into_boxed_slice()),
        in_bytes.clone(),
    );

    let mut e = Elang::new();

    let res = match e.parse(lo, &in_bytes) {
        Ok(r) => r,
        Err(msg) => {
            diag.handle(&mut e, &msg, |_| format!(""));
            return true;
        }
    };

    if let Err(msg) = e.eval(res) {
        diag.handle(&mut e, &msg, |e| format!("{:?}", e));
        return true;
    }

    eprintln!("{:?}", e.get_value(res));

    Test { e, diag }.compare(res, &out)
}

struct Test {
    e: Elang,
    diag: Diagnostic,
}

impl Test {
    fn compare(&mut self, actual: ExprId, expected: &serde_json::Value) -> bool {
        let expr = match self.e.resolve(actual) {
            Ok(e) => e,
            _ => return true,
        };
        let expr = expr.value().borrow();
        match (&*expr, expected) {
            (&ExprType::Number { val: ref i1 }, serde_json::Value::Number(i2)) => {
                let i2 = if i2.is_i64() {
                    BigRational::from((i2.as_i64().unwrap().into(), 1.into()))
                } else if i2.is_u64() {
                    BigRational::from((i2.as_u64().unwrap().into(), 1.into()))
                } else {
                    BigRational::from_float(i2.as_f64().unwrap()).unwrap()
                };
                if &**i1 != &i2 {
                    self.error(actual, format!("expected {}, got {}", i2, i1));
                    return true;
                }
            }
            (ExprType::Bool { val: b1 }, serde_json::Value::Bool(b2)) => {
                if b1 != b2 {
                    self.error(actual, format!("expected {}, got {}", b2, b1));
                    return true;
                }
            }
            (ExprType::Null, serde_json::Value::Null) => {}
            (ExprType::List { elements: ref l }, serde_json::Value::Array(a)) => {
                if l.len() != a.len() {
                    self.error(
                        actual,
                        format!(
                            "expected list of size {}, got list of size {}",
                            a.len(),
                            l.len()
                        ),
                    );
                    return true;
                }
                let mut err = false;
                for (l, a) in l.iter().zip(a.iter()) {
                    err |= self.compare(*l, a);
                }
                return err;
            }
            (&ExprType::String { content: s1 }, serde_json::Value::String(ref s2)) => {
                let s1 = self.e.get_interned(s1);
                if &*s1 != s2.as_bytes() {
                    self.error(
                        actual,
                        format!(
                            "expected `{}`, got `{}`",
                            s2,
                            &String::from_utf8_lossy(&s1)
                        ),
                    );
                    return true;
                }
            }
            (
                ExprType::Set {
                    fields: ref s1,
                    recursive: false,
                },
                serde_json::Value::Object(ref s2),
            ) => {
                if s1.len() != s2.len() {
                    self.error(
                        actual,
                        format!(
                            "expected set with {} elements, got set with {} elements",
                            s2.len(),
                            s1.len()
                        ),
                    );
                    return true;
                }
                let mut err = false;
                for (ks, v) in s2 {
                    let k = self
                        .e
                        .intern(ks.as_bytes().to_vec().into_boxed_slice().into());
                    err |= match s1.get(&k) {
                        Some(a) => self.compare(*a, v),
                        _ => {
                            self.error(
                                actual,
                                format!("expected field with name `{}`", ks),
                            );
                            true
                        }
                    };
                }
                return err;
            }
            _ => {
                self.error(actual, format!("cannot handle {:?}", expr.kind()));
                return true;
            }
        }
        false
    }

    fn error(&self, expr: ExprId, msg: String) {
        #[derive(Debug)]
        struct Ce(String);
        impl std::error::Error for Ce {
        }
        impl Display for Ce {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }

        self.diag.handle(
            &self.e,
            &Error {
                span: self.e.span(expr),
                error: ErrorType::Custom(Rc::new(Ce(msg))),
                context: vec![],
            },
            |c| format!("{}", c),
        );
    }
}

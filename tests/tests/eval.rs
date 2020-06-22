use crate::diag::{TestDiag};
use bstr::ByteSlice;
use elang::{util::codemap::Codemap, Elang, ExprId, Value, Error, ErrorType};
use std::{cell::RefCell, fs::DirEntry, os::unix::ffi::OsStrExt, rc::Rc};

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

    let codemap = Codemap::new();
    let lo = codemap.add_file(
        Rc::from(in_path.as_os_str().as_bytes().to_vec().into_boxed_slice()),
        in_bytes.clone(),
    );
    let codemap = Rc::new(RefCell::new(codemap));

    let e = Rc::new(Elang::new());

    let diag = TestDiag::new(codemap, e.clone());

    let res = match e.parse(lo, &in_bytes) {
        Ok(r) => r,
        Err(e) => {
            diag.handle(&e);
            return true;
        }
    };

    if let Err(e) = e.eval(res) {
        diag.handle(&e);
        return true;
    }

    Test { e, diag }.compare(res, &out)
}

struct Test {
    e: Rc<Elang>,
    diag: TestDiag,
}

impl Test {
    fn compare(&self, actual: ExprId, expected: &serde_json::Value) -> bool {
        let expr = match self.e.resolve(actual) {
            Ok(e) => e,
            _ => return true,
        };
        let expr = expr.value().borrow();
        match (&*expr, expected) {
            (&Value::Integer(i1), serde_json::Value::Number(i2)) => {
                let i2 = i2.as_i64().unwrap();
                if i1 != i2 {
                    self.error(actual, format!("expected {}, got {}", i2, i1));
                    return true;
                }
            }
            (Value::Bool(b1), serde_json::Value::Bool(b2)) => {
                if b1 != b2 {
                    self.error(actual, format!("expected {}, got {}", b2, b1));
                    return true;
                }
            }
            (Value::Null, serde_json::Value::Null) => {}
            (Value::List(ref l), serde_json::Value::Array(a)) => {
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
            (&Value::String(s1), serde_json::Value::String(ref s2)) => {
                let s1 = self.e.get_interned(s1);
                if &*s1 != s2.as_bytes() {
                    self.error(
                        actual,
                        format!("expected `{:?}`, got `{:?}`", s2, s1.as_bstr()),
                    );
                    return true;
                }
            }
            (Value::Set(ref s1, false), serde_json::Value::Object(ref s2)) => {
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
                        Some(a) => self.compare(a.1, v),
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
                self.error(actual, format!("cannot handle {:?}", expr.debug(&self.e)));
                return true;
            }
        }
        false
    }

    fn error(&self, mut expr: ExprId, msg: String) {
        self.diag.handle(&Error {
            span: self.e.span(expr),
            error: ErrorType::UnexpectedEndOfInput,
            context: vec![]
        });
    }
}

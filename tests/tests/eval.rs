use elang::{Diagnostic, Elang, Error, ErrorType, ExprId};
use num_rational::BigRational;
use std::{
    fmt,
    fmt::{Display, Formatter},
    fs::DirEntry,
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
        Rc::from(
            format!("{}", in_path.display())
                .into_bytes()
                .into_boxed_slice(),
        ),
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

    if let Err(err) = (Test { e: &mut e }.compare(res, &out)) {
        for err in err.0 {
            diag.handle(&e, &err, |x| format!("{}", x));
        }
        return true;
    }

    false
}

struct Test<'a> {
    e: &'a mut Elang,
}

impl<'a> Test<'a> {
    fn compare(&mut self, actual: ExprId, expected: &serde_json::Value) -> std::result::Result<(), ErrorCollection> {
        match expected {
            serde_json::Value::Number(i2) => {
                let i1 = self.e.get_number(actual)?;
                let i2 = if i2.is_i64() {
                    BigRational::from((i2.as_i64().unwrap().into(), 1.into()))
                } else if i2.is_u64() {
                    BigRational::from((i2.as_u64().unwrap().into(), 1.into()))
                } else {
                    BigRational::from_float(i2.as_f64().unwrap()).unwrap()
                };
                if &*i1 != &i2 {
                    return self.error(actual, format!("expected {}, got {}", i2, i1));
                }
            }
            &serde_json::Value::Bool(b2) => {
                let b1 = self.e.get_bool(actual)?;
                if b1 != b2 {
                    return self.error(actual, format!("expected {}, got {}", b2, b1));
                }
            }
            serde_json::Value::Null => self.e.get_null(actual)?,
            serde_json::Value::Array(a) => {
                let l = self.e.get_list(actual)?;
                if l.len() != a.len() {
                    return self.error(
                        actual,
                        format!(
                            "expected list of size {}, got list of size {}",
                            a.len(),
                            l.len()
                        ),
                    );
                }
                let mut err = vec!();
                for (l, a) in l.iter().zip(a.iter()) {
                    if let Err(e) = self.compare(*l, a) {
                        err.extend(e.0.into_iter());
                    }
                }
                if err.len() > 0 {
                    return Err(ErrorCollection(err));
                }
            }
            serde_json::Value::String(ref s2) => {
                let s1 = self.e.get_string(actual)?;
                let s1 = self.e.get_interned(s1);
                if &*s1 != s2.as_bytes() {
                    return self.error(
                        actual,
                        format!(
                            "expected `{}`, got `{}`",
                            s2,
                            &String::from_utf8_lossy(&s1)
                        ),
                    );
                }
            }
            serde_json::Value::Object(ref s2) => {
                let s1 = self.e.get_map(actual)?;
                if s1.len() != s2.len() {
                    return self.error(
                        actual,
                        format!(
                            "expected map with {} elements, got map with {} elements",
                            s2.len(),
                            s1.len()
                        ),
                    );
                }
                let mut err = vec!();
                for (ks, v) in s2 {
                    let k = self.e.intern(ks.as_str());
                    let res = match s1.get(&k) {
                        Some(a) => self.compare(*a, v),
                        _ => {
                            return self.error(
                                actual,
                                format!("expected field with name `{}`", ks),
                            );
                        }
                    };
                    if let Err(e) = res {
                        err.extend(e.0.into_iter());
                    }
                }
                if err.len() > 0 {
                    return Err(ErrorCollection(err));
                }
            }
        }
        Ok(())
    }

    fn error(&self, expr: ExprId, msg: String) -> Result<(), ErrorCollection> {
        Err(self.e.error(expr, ErrorType::Custom { error: Rc::new(Ce(msg)) }).into())
    }
}

struct ErrorCollection(Vec<Error>);

impl From<Error> for ErrorCollection {
    fn from(e: Error) -> Self {
        ErrorCollection(vec!(e))
    }
}

#[derive(Debug)]
struct Ce(String);

impl std::error::Error for Ce { }

impl Display for Ce {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

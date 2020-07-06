use elang::{Diagnostic, Elang, Error};
use std::{cell::RefCell, fs::DirEntry, os::unix::ffi::OsStrExt, rc::Rc};
use std::io::stdout;

#[test]
fn error() {
    let mut has_errors = false;
    for dir in std::fs::read_dir("tests/error").unwrap() {
        let dir = dir.unwrap();
        has_errors |= test(dir);
    }
    if has_errors {
        panic!("there were errors");
    }
}

struct ErrorDiag {
    td: Diagnostic,
    lo: Rc<RefCell<Option<u32>>>,
}

impl ErrorDiag {
    fn handle(&self, e: &mut Elang, message: Error) {
        let mut lo = self.lo.borrow_mut();
        if lo.is_some() {
            panic!("multiple errors");
        }
        *lo = Some(message.span().lo());
        eprint!("{}", self.td.handle(e, &message));
    }
}

fn test(dir: DirEntry) -> bool {
    let path = dir.path();
    let in_str = std::fs::read_to_string(&path).unwrap();
    let mut bytes = Vec::new();

    let mut prev_line_pos = 0;
    let mut prev_line_len = 0;
    let mut epos = 0;
    for line in in_str.lines() {
        if let Some(pos) = line.find("^ERROR") {
            epos = prev_line_pos + pos;
            prev_line_len += 1;
        } else {
            prev_line_pos += prev_line_len;
            prev_line_len = line.len() + 1;
            bytes.extend_from_slice(line.as_bytes());
        }
        bytes.push(b'\n');
    }

    let in_bytes: Rc<[u8]> = bytes.into_boxed_slice().into();

    println!("testing {}", path.display());

    let mut diag = ErrorDiag {
        td: Diagnostic::new(),
        lo: Rc::new(RefCell::new(None)),
    };
    diag.td.add_src(
        path.as_os_str()
            .as_bytes(),
        in_bytes.clone(),
    );

    let mut e = Elang::new();

    match e.parse(0, &in_bytes) {
        Ok(res) => {
            if let Err(msg) = e.eval(res) {
                diag.handle(&mut e, msg);
            }
        }
        Err(msg) => diag.handle(&mut e, msg),
    };

    let res = match *diag.lo.borrow() {
        Some(e) if e == epos as u32 => false,
        _ => {
            eprintln!("XXXXXXXXXXXXXX");
            eprintln!("XXXXXXXXXXXXXX");
            eprintln!("XXXXXXXXXXXXXX");
            eprintln!(
                "             no error occurred or it occurred in the wrong position"
            );
            eprintln!("XXXXXXXXXXXXXX");
            eprintln!("XXXXXXXXXXXXXX");
            eprintln!("XXXXXXXXXXXXXX");
            true
        }
    };
    res
}

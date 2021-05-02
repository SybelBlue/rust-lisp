use rust_lisp::{lexer::parse_all, parser::Context};

fn main() -> std::io::Result<()> {
    let mut ctxt = Context::new();
    let s = std::fs::read_to_string("test/t0.rsp")?;
    let exprs = parse_all(&mut s.chars().peekable());
    for res in exprs {
        match res {
            Ok(e) => println!("{:?}", e.exec(&mut ctxt, true)),
            Err(s) => println!("Error\n{}\n", s),
        }
    }
    Ok(())
}

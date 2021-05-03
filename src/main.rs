use rust_lisp::parser::exec;

fn main() -> std::io::Result<()> {
    let s = std::fs::read_to_string("test/t0.rsp")?;
    let (reses, ctxt) = exec(s);
    for r in reses {
        match r {
            Ok(v) => println!(">> {}", v),
            Err(s) => println!(" * {} ", s),
        }
    }
    println!("Batch finished w/ {} symbols", ctxt.size());
    Ok(())
}

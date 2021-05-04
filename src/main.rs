use rust_lisp::evaluator::exec;
// todo:
//      Strings
//      recur, if
//      Quotes
//      Duplicate param names
fn main() -> std::io::Result<()> {
    let s = std::fs::read_to_string("test/main.rsp")?;
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

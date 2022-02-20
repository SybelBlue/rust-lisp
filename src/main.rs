use linefeed::{Interface, ReadResult};

use rust_lisp::parsing::lex::Source;

fn main() -> std::io::Result<()> {
    let reader = Interface::new("risp-repl")?;
    let ref name = format!("term-input");
    reader.set_prompt(">> ")?;
    
    while let ReadResult::Input(input) = reader.read_line()? {
        match Source::new(&input, Some(name)).lex() {
            Ok(v) => println!("{:?}", v),
            Err(e) => println!("** {:?}", e)
        }
    }
    
    // println!("Batch finished w/ {} symbols", ctxt.size());
    Ok(())
}

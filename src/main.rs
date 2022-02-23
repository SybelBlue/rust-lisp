use linefeed::{Interface, ReadResult};

use rust_lisp::{parsing::{lex::Source, parse_tokens}, exprs::{types::type_expr, contexts::Context}};

fn main() -> std::io::Result<()> {
    let reader = Interface::new("risp-repl")?;
    let ref name = format!("term-input");
    reader.set_prompt(">> ")?;
    
    while let ReadResult::Input(input) = reader.read_line()? {
        match Source::new(&input, Some(name)).lex() {
            Ok(ts) => {
                // println!("tokens {:?}", &ts);
                match parse_tokens(ts) {
                    Ok(es) => {
                        let mut ctxt = Context::new();
                        for e in &es {
                            match type_expr(e, &mut ctxt) {
                                Ok(t) => {
                                    println!("(:: {} {})", e, t);
                                },
                                Err(e) => println!("** {:?}", e),
                            }
                        }
                    },
                    Err(e) => println!("** {:?}", e),
                }
            },
            Err(e) => println!("** {}", e)
        }
    }
    
    // println!("Batch finished w/ {} symbols", ctxt.size());
    Ok(())
}

use linefeed::{Interface, ReadResult};

use rust_lisp::{parsing::{parse, sources::Source}, typing::{checking::type_mod, contexts::TypeContext}};

fn main() -> std::io::Result<()> {
    let reader = Interface::new("risp-repl")?;
    reader.set_prompt(">> ")?;
    let mut ctxt = TypeContext::new();

    while let ReadResult::Input(input) = reader.read_line()? {
            let ref mut buf = String::new();
            match Source::Anon(input.as_str()).lex(buf) {
            Ok(ts) => {
                match parse(ts) {
                    Ok(es) => {
                        match type_mod(&es, ctxt.clone()) {
                            Ok(new) => {
                                ctxt = new;
                            },
                            Err(e) => println!("** {}", e),
                        }
                        
                    },
                    Err(e) => println!("** {}", e),
                }
            },
            Err(e) => println!("** {}", e)
        }
    }
    
    // println!("Batch finished w/ {} symbols", ctxt.size());
    Ok(())
}

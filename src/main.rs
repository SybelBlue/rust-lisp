use linefeed::{Interface, ReadResult};

use rust_lisp::{parsing::{parse, sources::Source}, typing::{checking::type_mod, contexts::Context}};

fn main() -> std::io::Result<()> {
    let reader = Interface::new("risp-repl")?;
    reader.set_prompt(">> ")?;
    let mut ctxt = Context::new();

    while let ReadResult::Input(input) = reader.read_line()? {
            let ref mut buf = String::new();
            match Source::Anon(input.as_str()).lex(buf) {
            Ok(ts) => {
                match parse(ts) {
                    Ok(es) => {
                        match type_mod(&es, ctxt.clone()) {
                            Ok((ts, new)) => {
                                ctxt = new;
                                for (e, t) in es.iter().zip(ts) {
                                    println!(" | {} :: {}", e, t);
                                }
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

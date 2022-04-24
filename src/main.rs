use linefeed::{Interface, ReadResult};

use rust_lisp::{parsing::{parse_tokens, sources::Source}, exprs::typing::{checking::type_expr, contexts::TypeContext}};

fn main() -> std::io::Result<()> {
    let reader = Interface::new("risp-repl")?;
    reader.set_prompt(">> ")?;

    while let ReadResult::Input(input) = reader.read_line()? {
            let ref mut buf = String::new();
            match Source::Anon(input.as_str()).lex(buf) {
            Ok(ts) =>
                match parse_tokens(ts) {
                    Ok(es) => {
                        let mut ctxt = TypeContext::new();
                        for e in &es {
                            match type_expr(e, ctxt.clone()) {
                                Ok((t, new)) => {
                                    ctxt = new;
                                    println!(" | {} :: {}", e, t);
                                },
                                Err(e) => println!("** {}", e),
                            }
                        }
                    },
                    Err(e) => println!("** {}", e),
            },
            Err(e) => println!("** {}", e)
        }
    }
    
    // println!("Batch finished w/ {} symbols", ctxt.size());
    Ok(())
}

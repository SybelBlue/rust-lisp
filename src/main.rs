use std::sync::Arc;

use linefeed::{Interface, ReadResult};

use rust_lisp::{parsing::{sources::Source, parse}, typing::{infer::infer, contexts::Context}, errors::{LexError, LexErrorBody}};

fn main() -> std::io::Result<()> {
    let reader = Interface::new("risp-repl")?;
    reader.set_prompt(">> ")?;
    let mut ctxt = Context::new();
    reader.set_completer(Arc::new(ctxt.clone()));
    let mut lines = Vec::new();

    while let ReadResult::Input(input) = reader.read_line()? {
            reader.add_history(input.clone());
            if lines.is_empty() && input.starts_with(":") {
                match input.as_bytes() {
                    b":ctxt" => println!("{:?}", &ctxt),
                    b":dir" => println!("{:?}", ctxt.keys()),
                    _ => println!("** Unrecognized repl cmd '{}'", input),
                }
                continue;
            }
            lines.push(input);
            let instr = lines.join("\n");
            let ref mut buf = String::new();
            match Source::Anon(instr.as_str()).lex(buf) {
            Ok(ts) => {
                match parse(ts) {
                    Ok(stmts) => {
                        let (new, res) = infer(ctxt, &stmts);
                        ctxt = new;
                        reader.set_completer(Arc::new(ctxt.clone()));
                        match res {
                            Ok(ts) => {
                                for (e, t) in stmts.iter().zip(ts) {
                                    println!(" | {} :: {}", e, t);
                                }
                            },
                            Err(e) => println!("** {}", e),
                        }
                        
                    },
                    Err(e) => println!("** {}", e),
                }
            },
            Err(LexError { body: LexErrorBody::Unclosed(_), .. } ) => {
                reader.set_prompt(".. ")?;
                continue;
            }
            Err(e) => println!("** {}", e)
        }
        reader.set_prompt(">> ")?;
        lines.clear();
    }
    
    // println!("Batch finished w/ {} symbols", ctxt.size());
    Ok(())
}

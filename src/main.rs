use linefeed::{Interface, ReadResult};

use rust_lisp::{context::Context, evaluator::{exec, exec_using}};

// todo:
//      recur
//      pmatch
//      let
//      Strings
//      fix imports
//      list-likes (iterator, sll)

fn main() -> std::io::Result<()> {
    let arg = std::env::args().nth(1);
    let mut ctxt = arg.map_or_else(Context::new, |path| {
        match std::fs::read_to_string(path.as_str()) {
            Ok(s) => {
                println!("Loading {}...", path);

                let (reses, ctxt) = exec(s.clone());
                for r in reses {
                    match r {
                        Ok(v) => println!(" | {}", v),
                        Err(e) => println!(" * {} ", e),
                    }
                }

                println!("Loaded {} w/ {} symbols", path, ctxt.size());

                ctxt
            },
            Err(e) => {
                println!("Loading {} failed with\n{}", path, e);
                
                Context::new()
            }
        }
    });

    let reader = Interface::new("risp-repl")?;
    
    reader.set_prompt(">> ")?;
    
    while let ReadResult::Input(input) = reader.read_line()? {
        if input.as_bytes() == b"dir" {
            println!("{:?}", ctxt.dir());
            continue;
        }
        let reses = exec_using(input, &mut ctxt, &None);
        for r in reses {
            match r {
                Ok(v) => println!(" | {}", v),
                Err(e) => println!("** {} ", e),
            }
        }
    }
    
    println!("Batch finished w/ {} symbols", ctxt.size());

    Ok(())
}

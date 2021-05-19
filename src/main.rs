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
    let path = std::env::args().nth(1).expect("Need path to file");
    println!("Loading {}...", path);
    let mut ctxt = if let Ok(s) = std::fs::read_to_string(path.as_str()) {
        let (reses, ctxt) = exec(s.clone());
        for r in reses {
            match r {
                Ok(v) => println!(" | {}", v),
                Err(e) => println!(" * {} ", e),
            }
        }

        println!("Loaded {} w/ {} symbols", path, ctxt.size());

        ctxt
    } else {
        Context::new()
    };

    let reader = Interface::new("risp")?;
    
    reader.set_prompt(">> ")?;
    
    while let ReadResult::Input(input) = reader.read_line()? {
        let reses = exec_using(input, &mut ctxt, &None);
        for r in reses {
            match r {
                Ok(v) => println!(" | {}", v),
                Err(e) => println!(" * {} ", e),
            }
        }
    }
    
    println!("Batch finished w/ {} symbols", ctxt.size());

    Ok(())
}

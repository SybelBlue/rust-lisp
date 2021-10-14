use linefeed::{Interface, ReadResult};

use rust_lisp::{context::Context, rtype::reify};

// todo:
//      recur
//      pmatch
//      let
//      Strings
//      fix imports
//      list-likes (iterator, sll)

fn main() -> std::io::Result<()> {
    let arg = std::env::args().nth(1);
    let ctxt = arg.map_or_else(Context::new, |path| {
        match std::fs::read_to_string(path.as_str()) {
            Ok(_s) => {
                println!("Loading {}...", path);

                // let (reses, ctxt) = exec(s.clone());
                // for r in reses {
                //     match r {
                //         Ok(v) => println!(" | {}", v),
                //         Err(e) => println!(" * {} ", e),
                //     }
                // }

                // println!("Loaded {} w/ {} symbols", path, ctxt.size());

                // ctxt
                unimplemented!()
            }
            Err(e) => {
                println!("Loading {} failed with\n{}", path, e);

                Context::new()
            }
        }
    });

    let reader = Interface::new("risp-repl")?;

    reader.set_prompt(">> ")?;

    while let ReadResult::Input(input) = reader.read_line()? {
        // let reses = exec_using(input, &mut ctxt, &None);
        // for r in reses {
        //     match r {
        //         Ok(v) => println!(" | {}", v),
        //         Err(e) => println!("** {} ", e),
        //     }
        // }
        let mut cs = input.chars().peekable();
        let mut lexstream = rust_lisp::lexer::LexStream::new(&mut cs);
        let tkn_res = rust_lisp::lexer::lex_all(&mut lexstream);
        for t in tkn_res {
            match t {
                Ok(tkn) => match rust_lisp::parser::parse_tkn(tkn, &ctxt) {
                    Ok(e) => {
                        println!("{:?}", &e);
                        println!("{:?}", reify(&e, &ctxt));
                    }
                    Err(e) => println!("Err: {}", e),
                },
                Err(e) => println!("Err: {}", e),
            }
        }
    }

    println!("Batch finished w/ {} symbols", ctxt.size());

    Ok(())
}

pub mod lexer;
pub mod parser;
pub mod context;

#[cfg(test)]
mod tests {
    mod lexer {
        use crate::parser::{*, Expr::*, Value::*};

        #[test]
        fn basics() {
            let s = std::fs::read_to_string("test/basics.rsp").expect("file not found");
            let (reses, _) = exec(s);
            for (ex, ac) in vec![ Unit
                , Int(3)
                , Unit
                , Int(4)
                , Unit
                , Int(2)
                , Float(6.1)
                , Unit
                , Int(-1)
                , Unit
                , Int(2)
                , Fn(vec![format!("a"), format!("b")], 
                    Box::new(Form(
                        Ident { name: format!("+"), file_pos: FilePos { col: 12, line: 23 } }, 
                        vec![ Idnt(Ident { name: format!("a"), file_pos: FilePos { col: 14, line: 23 } })
                            , Idnt(Ident { name: format!("b"), file_pos: FilePos { col: 16, line: 23 } })
                            ])))
            ].into_iter().zip(reses.into_iter()) {
                assert_eq!(Ok(ex), ac)
            }
        }
    }
}

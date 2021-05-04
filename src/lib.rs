pub mod parser;
pub mod evaluator;
pub mod context;

#[cfg(test)]
mod tests {
    mod lexer {
        use crate::evaluator::{*, Expr::*, Value::*};

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
                , Fn(
                    vec![ Ident { name: format!("a"), file_pos: FilePos { col: 6, line: 19 } }
                        , Ident { name: format!("b"), file_pos: FilePos { col: 8, line: 19 } }
                        ]
                    , Box::new(Token { 
                        expr: Form(
                            vec![ Token { expr: Var(format!("+")), file_pos: FilePos { col: 12, line: 19 } }
                                , Token { expr: Var(format!("a")), file_pos: FilePos { col: 14, line: 19 } }
                                , Token { expr: Var(format!("b")), file_pos: FilePos { col: 16, line: 19 } }
                                ]), 
                        file_pos: FilePos { col: 11, line: 19 } }))
            ].into_iter().zip(reses.into_iter()) {
                assert_eq!(Ok(ex), ac)
            }
        }

        #[test]
        fn basic_errs() {
            let s = std::fs::read_to_string("test/basic-errs.rsp").expect("file not found");
            let (reses, _) = exec(s);
            assert!(reses.iter().all(|v| matches!(v, Ok(Unit) | Err(_))));
        }

        #[test]
        fn complex_fns() {
            let s = std::fs::read_to_string("test/funcs.rsp").expect("file not found");
            let (reses, _) = exec(s);
            for (ex, ac) in vec![ Unit
                , Unit
                , Int(3)
                , Unit
                , Int(1)
                ].into_iter().zip(reses.into_iter()) {
                assert_eq!(Ok(ex), ac)
            }
        }
    }
}

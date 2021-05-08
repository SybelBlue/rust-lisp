pub mod parser;
pub mod evaluator;
pub mod context;

pub(crate) mod builtin_fn;

#[cfg(test)]
mod tests {
    use crate::evaluator::{*, token::*, expr::Expr::*, result::{FilePos, Error::*}, value::{Ident, Value::*}};

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
                vec![ Ident { name: format!("a"), file_pos: FilePos { col: 6, line: 23 } }
                    , Ident { name: format!("b"), file_pos: FilePos { col: 8, line: 23 } }
                    ]
                , None
                , Box::new(Token { 
                    expr: Form(
                        vec![ Token { expr: Var(format!("+")), file_pos: FilePos { col: 12, line: 23 } }
                            , Token { expr: Var(format!("a")), file_pos: FilePos { col: 14, line: 23 } }
                            , Token { expr: Var(format!("b")), file_pos: FilePos { col: 16, line: 23 } }
                            ]), 
                    file_pos: FilePos { col: 11, line: 19 } }))
        ].into_iter().zip(reses.into_iter()) {
            assert_eq!(Ok(ex), ac)
        }
    }

    #[test]
    fn basic_errs() {
        use crate::parser::ParseError::*;
        let s = std::fs::read_to_string("test/basic-errs.rsp").expect("file not found");
        let (reses, _) = exec(s);
        let mut i = 0;
        assert!(matches!(reses[i], Err(ParseError(Missing(_,_))))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(NameError(_)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Ok(Unit))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(NameError(_)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(Missing(_,_))))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(NameError(_)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Ok(Unit))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(RedefError(_, _)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ArgError { .. } ))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ValueError(_, _)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ValueError(_, _)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ArgError { .. } ))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(Missing(_,_))))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(NameError(_)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(BadChar(_,_,_))))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(Missing(_,_))))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(DupArg { .. })))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(BadQuote(_,_))))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(BadQuote(_,_))))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(BadQuote(_,_))))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(IllegalDefError(_)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(RedefError(_, _)))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(DupArg { .. })))); i += 1; println!("{:?}", reses[i]);
        assert!(matches!(reses[i], Err(ParseError(Eof(_))))); i += 1;
        assert_eq!(None, reses.get(i))
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

    #[test]
    fn big_expr() {
        let s = std::fs::read_to_string("test/big-forms.rsp").expect("file not found");
        let (reses, _) = exec(s);
        for (ex, ac) in vec![Int(0)].into_iter().zip(reses.into_iter()) {
            assert_eq!(Ok(ex), ac)
        }
    }

    #[test]
    fn rest() {
        let s = std::fs::read_to_string("test/rest.rsp").expect("file not found");
        let (reses, _) = exec(s);
        for r in reses {
            assert_eq!(Ok(Unit), r);
        }
    }

    #[test]
    fn macros() {
        let s = std::fs::read_to_string("test/macros.rsp").expect("file not found");
        let (reses, _) = exec(s);
        for r in reses {
            assert_eq!(Ok(Unit), r);
        }
    }
}

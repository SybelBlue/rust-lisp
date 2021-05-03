pub mod lexer;
pub mod parser;
pub mod context;

#[cfg(test)]
mod tests {
    mod lexer {
        use std::collections::VecDeque;

        use crate::{lexer::*, parser::{Expr, Value}};

        #[test]
        fn identifier() {
            let s = String::from("hello-world");
            let cs = &mut s.chars().peekable();
            let chars = &mut ParseStream::new(cs);
            assert_eq!(Ok(String::from("hello-world")), parse_identifier(chars));
            assert_eq!(None, chars.next());

            let s = String::from("hello-world\t\n\t   \n\t ");
            let cs = &mut s.chars().peekable();
            let chars = &mut ParseStream::new(cs);
            assert_eq!(Ok(String::from("hello-world")), parse_identifier(chars));
            assert_eq!(None, chars.next());
            
            let s = String::from("+");
            let cs = &mut s.chars().peekable();
            let chars = &mut ParseStream::new(cs);
            assert_eq!(Ok(String::from("+")), parse_identifier(chars));
            assert_eq!(None, chars.next());
            
            let s = String::from("");
            let cs = &mut s.chars().peekable();
            let chars = &mut ParseStream::new(cs);
            assert!(matches!(parse_identifier(chars), Err(_)));
            assert_eq!(None, chars.next());
        }

        #[test]
        fn expr() {
            use Expr::*;
            use Value::*;

            let s = format!("\n\t(+    1  2 3  )  \n\t(apply f ()) \n(def add (fn [a b] (+ a b)))");
            let cs = &mut s.chars().peekable();
            let chars = &mut ParseStream::new(cs);
            assert_eq!(parse_all(chars), 
                vec![ Ok(Form(format!("+"), vec![Lit(Int(1)), Lit(Int(2)), Lit(Int(3))]))
                    , Ok(Form(format!("apply"), vec![Ident(format!("f")), Lit(Unit)]))
                    , Ok(Def(format!("add"), Box::new(Lit(Fn(
                            VecDeque::from(vec![format!("a"), format!("b")]), 
                            Box::new(Form(format!("+"), vec![Ident(format!("a")), Ident(format!("b"))])))))))
                ]);
        }
    }
}

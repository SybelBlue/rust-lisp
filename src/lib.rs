pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests {
    mod lexer {
        use crate::{lexer::*, parser::{Expr, Value}};

        #[test]
        fn identifier() {
            let s = String::from("hello-world");
            let chars = &mut s.chars().peekable();
            assert_eq!(Ok(String::from("hello-world")), parse_identifier(chars));
            assert_eq!(None, chars.next());

            let s = String::from("hello-world\t\n\t   \n\t ");
            let chars = &mut s.chars().peekable();
            assert_eq!(Ok(String::from("hello-world")), parse_identifier(chars));
            assert_eq!(None, chars.next());
            
            let s = String::from("+");
            let chars = &mut s.chars().peekable();
            assert_eq!(Ok(String::from("+")), parse_identifier(chars));
            assert_eq!(None, chars.next());
            
            let s = String::from("");
            let chars = &mut s.chars().peekable();
            assert!(matches!(parse_identifier(chars), Err(_)));
            assert_eq!(None, chars.next());
        }

        #[test]
        fn number() {
            use Value::*;
            let s = String::from("47");
            let chars = &mut s.chars().peekable();
            assert_eq!(Ok(Int(47)), parse_number(chars));
            assert_eq!(None, chars.next());

            let s = String::from("-47\t\n  ");
            let chars = &mut s.chars().peekable();
            assert_eq!(Ok(Int(-47)), parse_number(chars));
            assert_eq!(None, chars.next());

            let s = String::from("-0047.34");
            let chars = &mut s.chars().peekable();
            assert_eq!(Ok(Float(-47.34)), parse_number(chars));
            assert_eq!(None, chars.next());
            
            let s = String::from("");
            let chars = &mut s.chars().peekable();
            assert!(matches!(parse_number(chars), Err(_)));
            assert_eq!(None, chars.next());
            
            let s = String::from("1.");
            let chars = &mut s.chars().peekable();
            assert!(matches!(parse_number(chars), Err(_)));
            assert_eq!(None, chars.next());
            
            let s = String::from("-");
            let chars = &mut s.chars().peekable();
            assert!(matches!(parse_number(chars), Err(_)));
            assert_eq!(None, chars.next());
        }

        #[test]
        fn expr() {
            use Expr::*;
            use Value::*;

            let s = String::from("\n\t(+    1  2 3  )  \n\t(apply f ())");
            let chars = &mut s.chars().peekable();
            assert_eq!(parse_all(chars), 
                vec![ Ok(Form(String::from("+"), vec![Lit(Int(1)), Lit(Int(2)), Lit(Int(3))]))
                    , Ok(Form(String::from("apply"), vec![Ident(String::from("f")), Lit(Unit)]))
                ]);
        }
    }
}

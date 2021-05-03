pub mod lexer;
pub mod parser;
pub mod context;

#[cfg(test)]
mod tests {
    mod lexer {
        use crate::parser::exec;

        #[test]
        fn basics() {
            let s = std::fs::read_to_string("test/basics.rsp").expect("file not found");
            let (reses, ctxt) = exec(s);
            println!("{:?}", reses);
            assert!(false)
        }
    }
}

pub mod lexer;
pub mod result;
pub mod value;
pub mod token;
pub mod context;
pub mod parser;
pub mod expr;
pub mod rtype;

pub(crate) mod builtin_fn;

#[cfg(test)]
mod tests {
    #[test]
    fn success() {
        // z3 = {version="0.11.2", features = ["static-link-z3"]}
        // use z3::*;
        
    }
}

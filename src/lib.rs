pub mod context;
pub mod expr;
pub mod lexer;
pub mod parser;
pub mod result;
pub mod rtype;
pub mod token;
pub mod value;

pub(crate) mod builtin_fn;

#[cfg(test)]
mod tests {
    #[test]
    fn success() {
        // z3 = {version="0.11.2", features = ["static-link-z3"]}
        // use z3::*;
    }
}

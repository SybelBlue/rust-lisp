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
    fn success() {}
}

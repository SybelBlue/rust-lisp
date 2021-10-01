pub mod parser;
pub mod result;
pub mod expr;
pub mod value;
pub mod token;
pub mod context;

pub(crate) mod builtin_fn;

#[cfg(test)]
mod tests {
    #[test]
    fn success() {}
}

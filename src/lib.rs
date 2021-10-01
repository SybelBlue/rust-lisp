pub mod parser;
pub mod evaluator;
pub mod context;

pub(crate) mod builtin_fn;

#[cfg(test)]
mod tests {
    #[test]
    fn success() {}
}

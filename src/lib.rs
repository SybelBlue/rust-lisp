use std::iter::Peekable;

mod parsing;

pub(crate) fn take_while<T, I>(it: &mut Peekable<I>, p: fn(&T) -> bool) -> Vec<T> 
        where I: Iterator<Item = T> {
    let mut out = Vec::new();
    while let Some(t) = it.next_if(p) {
        out.push(t);
    }
    out
}

pub(crate) fn skip_while<T, I>(it: &mut Peekable<I>, p: fn(&T) -> bool)
        where I: Iterator<Item = T> {
    while it.next_if(p).is_some() {}
}

#[cfg(test)]
mod tests {
    #[test]
    fn success() {
    }
}

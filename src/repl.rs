pub(crate) use linefeed::{Terminal, Completer, Completion};

use crate::typing::contexts::Context;

impl<Term: Terminal> Completer<Term> for Context {
    fn complete(&self, word: &str, _prompter: &linefeed::Prompter<Term>,
        _start: usize, _end: usize) -> Option<Vec<Completion>> {
        let mut out = Vec::new();
        for k in self.get_varnames() {
            if k.starts_with(word) {
                out.push(
                    Completion::simple(k.clone())
                );
            }
        }
        Some(out)
    }
}
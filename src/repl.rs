use linefeed::{Terminal, Completer};

use crate::typing::contexts::Context;


struct ReplCompleter(Context);

impl<Term: Terminal> Completer<Term> for ReplCompleter {
    fn complete(&self, word: &str, prompter: &linefeed::Prompter<Term>,
        start: usize, end: usize) -> Option<Vec<linefeed::Completion>> {
        todo!()
    }
}
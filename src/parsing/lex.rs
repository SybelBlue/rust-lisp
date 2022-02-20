#[derive(Debug)]
pub struct Source<'a> {
    txt: std::str::Chars<'a>,
    pub name: Option<&'a String>
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    SExp(Vec<Token>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LexError {
    TooManyClosing,
    Unclosed
}

pub type LexResult<T> = Result<T, LexError>;

impl<'a> Source<'a> {
    pub fn new(source: &'a String, name: Option<&'a String>) -> Self {
        Self { txt: source.chars(), name }
    }

    pub fn lex(&mut self) -> LexResult<Vec<Token>> {        
        let mut stack = LexStack::new();

        loop {
            let (skipped, next) = self.advance();
            
            if skipped {
                stack.try_push_word();
            }
            
            match next {
                None => break,
                Some('(') => stack.open_sexp(),
                Some(')') => {
                    let sexp = stack.close_sexp()?;
                    stack.push_token(Token::SExp(sexp));
                },
                Some(ch) => stack.push_char(ch),
            }
        }

        stack.try_push_word();

        stack.finish()
    }

    fn advance(&mut self) -> (bool, Option<char>) {
        let mut skipped = false;
        loop {
            let n = self.txt.next();
            if !matches!(n, Some(c) if c.is_whitespace()) {
                return (skipped, n);
            } else {
                skipped = true;
            }
        }
    }
}

/// Used to store 
#[derive(Debug)]
struct LexStack {
    sexp_stack: Vec<Vec<Token>>,
    finished: Vec<Token>,
    curr_word: String,
}

impl LexStack {
    fn new() -> Self {
        Self {
            sexp_stack: Vec::with_capacity(8),      // 8 deep nested sexp
            finished: Vec::with_capacity(50),       // 50 tokens on top level
            curr_word: String::with_capacity(10),   // 10 char words
        }
    }

    fn open_sexp(&mut self) {
        self.sexp_stack.push(Vec::with_capacity(4)) // 4 long sexp
    }

    fn close_sexp(&mut self) -> LexResult<Vec<Token>> {
        let last_sexp = self.sexp_stack.pop();
        let mut finished = last_sexp.ok_or(LexError::TooManyClosing)?;

        if self.mid_word() {
            finished.push(self.dump_curr());
        }

        Ok(finished)
    }

    fn push_token(&mut self, t: Token) {
        self.sexp_stack
            .last_mut()
            .unwrap_or(&mut self.finished)
            .push(t)
    }

    fn dump_curr(&mut self) -> Token {
        let out = Token::Word(self.curr_word.clone());
        self.curr_word.clear();
        out
    }

    fn push_char(&mut self, ch: char) {
        self.curr_word.push(ch)
    }

    fn try_push_word(&mut self) {
        if self.mid_word() {
            let t = self.dump_curr();
            self.push_token(t);
        }
    }

    fn mid_word(&self) -> bool {
        !self.curr_word.is_empty()
    }

    fn finish(self) -> LexResult<Vec<Token>> {
        if self.sexp_stack.is_empty() {
            Ok(self.finished)
        } else {
            Err(LexError::Unclosed)
        }
    }
}
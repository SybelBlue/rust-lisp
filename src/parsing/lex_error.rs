use super::{FilePos, lex::Source};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum LexErrorType<'a> {
    TooManyClosing,
    Unclosed(FilePos<'a>),
}

impl<'a> LexErrorType<'a> {
    pub fn col_arrow(&self, file_pos: &FilePos) -> String {
        let file_pos = match &self {
            Self::TooManyClosing => file_pos,
            Self::Unclosed(file_pos) => file_pos
        };

        let mut out = String::new();
        out.extend((0..(file_pos.col - 2)).map(|_| ' '));
        out.push('^');
        out
    }

    pub fn name(&self) -> String {
        match self {
            Self::TooManyClosing => format!("Extra Closing Parenthesis"),
            Self::Unclosed(_) => format!("Unclosed S-Expression")
        }
    }
}

#[derive(Debug)]
pub struct LexError<'a> {
    pub src: Source<'a>,
    pub(crate) tipe: LexErrorType<'a>,
}

impl<'a> std::fmt::Display for LexError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line_indicator = format!(" {} ", self.src.pos.row);
        let margin: String = (0..line_indicator.len()).map(|_| ' ').collect();
        write!(f, "error: {} at {}\n{}|\n{}| {}\n{}| {}", 
            self.tipe.name(), 
            self.src.pos, 
            margin,
            line_indicator, 
            self.src.current_line(),
            margin,
            self.tipe.col_arrow(&self.src.pos))
    }
}
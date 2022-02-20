pub mod lex;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct FilePos<'a> {
    pub name: Option<&'a String>,
    row: usize,
    col: usize,
}

impl<'a> FilePos<'a> {
    pub fn new(name: Option<&'a String>) -> Self {
        Self { name, row: 1, col: 1 }
    }

    pub fn advance(&mut self, och: &Option<char>) {
        match och {
            None => {},
            Some('\n') => {
                self.row += 1;
                self.col = 1;
            },
            _ => self.col += 1,
        }
    }
}

impl<'a> std::fmt::Display for FilePos<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "./{}:{}:{}", 
            self.name.map(|n| n.as_str()).unwrap_or("anon"), 
            self.row,
            self.col)
    }
}
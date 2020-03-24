use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind {
    // Keywords etc.
    Name,

    // Literals
    Int,
    Float,

    // Arithmetic
    Plus,
    Minus,
    Slash,
    Star, // + - / *

    // Misc
    OpenParen,
    CloseParen,
    // ( )
    OpenBracket,
    CloseBracket,
    // [ ]
    OpenBrace,
    CloseBrace,
    // { }
    Semicolon,
    Comma,
    Dot, // ; , .
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    kind: TokenKind,
    text: String,
}

impl Token {
    pub(crate) fn new(kind: TokenKind, text: String) -> Self {
        Token {
            kind,
            text,
        }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn text(&self) -> &String {
        &self.text
    }
}


pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    token: String,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Self::Item> {
        // skip whitespace
        self.skip_whitespace();

        // fetch the next character and return it (or return None if there are no more)
        let c = match self.push_next_char() {
            Some(c) => c,
            None => return None,
        };

        // get the token kind and call any necessary methods to collect its text
        let kind = match c {
            // ident
            c if is_name(&c) => {
                self.push_next_char_while(is_name);
                TokenKind::Name
            }

            c if c.is_ascii_digit() => self.lex_int(),

            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,

            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,

            ';' => TokenKind::Semicolon,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,

            _ => return Some(Err(format!("Unexpected character '{}'", c))),
        };

        // retrieve the token text and clear it for next iteration
        let text = self.token.drain(..).collect();

        Some(Ok(Token::new(kind, text)))
    }
}

impl<'a> Lexer<'a> {
    /// Construct a new lexer for the given input only
    pub fn new(input: &'a str) -> Lexer {
        Lexer {
            input: input.chars().peekable(),
            token: String::new(),
        }
    }

    // Skip all next consecutive whitespace
    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.input.peek() {
            if c.is_ascii_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }
    }

    /// Read the next char (if there is one) and push it to `self.token.text`
    fn push_next_char(&mut self) -> Option<char> {
        if let Some(c) = self.input.next() {
            self.token.push(c);
            Some(c)
        } else {
            None
        }
    }

    // call `push_next_char` while the predicate is true
    fn push_next_char_while<F: Fn(&char) -> bool>(&mut self, f: F) {
        while let Some(true) = self.input.peek().map(&f) {
            self.push_next_char().unwrap();
        }
    }

    /// Lex an integer
    fn lex_int(&mut self) -> TokenKind {
        while let Some(true) = self.input.peek().map(char::is_ascii_digit) {
            self.push_next_char().unwrap();
        }

        if self.input.peek() == Some(&'.') {
            self.push_next_char().unwrap();
            return self.lex_float();
        }

        TokenKind::Int
    }

    /// Lex a float
    fn lex_float(&mut self) -> TokenKind {
        while let Some(true) = self.input.peek().map(char::is_ascii_digit) {
            self.push_next_char().unwrap();
        }

        TokenKind::Float
    }
}

fn is_name(c: &char) -> bool {
    c.is_ascii_alphabetic() || *c == '_'
}
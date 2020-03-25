use std::fmt::{self, Display, Error, Formatter};
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
    // +
    Minus,
    // -
    Slash,
    // /
    Star,  // *

    // Comparison
    Equals,
    // =
    DoubleEquals,
    // ==
    NotEquals,
    // !=
    LeftArrow,
    // <
    LessOrEqual,
    // <=
    RightArrow,
    // >
    GreaterOrEqual, // >=

    // Misc
    OpenParen,
    // [
    CloseParen,
    // )
    OpenBracket,
    // ]
    CloseBracket,
    // [
    OpenBrace,
    // {
    CloseBrace,   // }

    // Punctuation
    Bang,
    // !
    Semicolon,
    // ;
    Comma,
    // ,
    Dot,       // .
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Token {
    kind: TokenKind,
    text: String,
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl Token {
    pub fn new(kind: TokenKind, text: String) -> Self {
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

    // @TODO could change single char characters to match only by token.text(),
    // so they have kind: TokenKind::SingleChar, just for brevity and ease of use
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

            // literals
            c if c.is_ascii_digit() => self.lex_int(),

            // arithmetic operators
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,

            // parentheses
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '[' => TokenKind::OpenBracket,
            ']' => TokenKind::CloseBracket,
            '{' => TokenKind::OpenBrace,
            '}' => TokenKind::CloseBrace,

            // 2 character tokens
            '=' => self.accept_or('=', TokenKind::DoubleEquals, TokenKind::Equals),
            '!' => self.accept_or('=', TokenKind::NotEquals, TokenKind::Bang),
            '<' => self.accept_or('=', TokenKind::LessOrEqual, TokenKind::LeftArrow),
            '>' => self.accept_or('>', TokenKind::GreaterOrEqual, TokenKind::RightArrow),

            // single punctuation
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

    // Test the next character and push it if valid
    fn accept(&mut self, c: char) -> bool {
        if let Some(&peeked) = self.input.peek() {
            if peeked == c {
                self.push_next_char();
                return true
            }
        }
        false
    }

    // Accept `c` and return `a` or else return `b`
    #[inline]
    fn accept_or<T>(&mut self, c: char, a: T, b: T) -> T {
        if self.accept(c) {
            a
        } else {
            b
        }
    }

    // Call `push_next_char` while the predicate is true
    fn push_next_char_while<F: Fn(&char) -> bool>(&mut self, f: F) {
        while let Some(true) = self.input.peek().map(&f) {
            self.push_next_char().unwrap();
        }
    }

    /// Lex an integer
    fn lex_int(&mut self) -> TokenKind {
        self.push_next_char_while(char::is_ascii_digit);

        if self.input.peek() == Some(&'.') {
            self.push_next_char().unwrap();
            return self.lex_float();
        }

        TokenKind::Int
    }

    /// Lex a float
    fn lex_float(&mut self) -> TokenKind {
        // token.text currently = "."

        self.push_next_char_while(char::is_ascii_digit);

        TokenKind::Float
    }
}

fn is_name(c: &char) -> bool {
    c.is_ascii_alphabetic() || *c == '_'
}

#[cfg(test)]
mod test {
    macro_rules! tok {
        ($kind:ident, $text:expr) => (Token::new(TokenKind::$kind, String::from($text)))
    }

    #[test]
    fn numbers_work() {
        use super::*;

        let input = "asdf 1234 1234.5678 0.1234 1. 1.0";

        let mut lexer = Lexer::new(input);
        let tokens = lexer.collect::<Result<Vec<_>, String>>().unwrap();

        assert_eq!(tokens, vec![
            tok!(Name, "asdf"),
            tok!(Int, "1234"),
            tok!(Float, "1234.5678"),
            tok!(Float, "0.1234"),
            tok!(Float, "1."),
            tok!(Float, "1.0"),
        ]);
    }
}
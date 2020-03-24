use std::iter::Peekable;

use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug, Eq, PartialEq)]
enum Expect {
    Kind(TokenKind),
    Text(String),
    Token {
        kind: TokenKind,
        text: String,
    },
    Any(Vec<Expect>),
}

impl Expect {
    fn matches(&self, token: &Token) -> bool {
        match *self {
            Expect::Kind(ref kind) => *token.kind() == *kind,
            Expect::Text(ref text) => *token.text() == *text,
            Expect::Token { ref kind, ref text } => *token.kind() == *kind && *token.text() == *text,
            Expect::Any(ref options) => options.iter().any(|opt| opt.matches(token)),
        }
    }
}

/// A parser
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

/// A value
#[derive(Debug)]
pub enum Term {
    Int(i64),
    Float(f64),
    Name(String),
}

/// An expression
#[derive(Debug)]
pub enum Expr {
    Term(Term),
    Binary {
        op: String,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

impl<'a> Parser<'a> {
    /// Create a new parser
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer: lexer.peekable(),
        }
    }

    // Fetch the next token (if there is one)
    #[inline]
    fn next_token(&mut self) -> Result<Token, String> {
        self.lexer.next().ok_or(String::from("Unexpected EOF"))?
    }

    /// Parse the next item
    pub fn parse_next(&mut self) -> Result<Expr, String> {
        self.parse_expr()
    }

    /// Parse the next expr
    fn parse_expr(&mut self) -> Result<Expr, String> {
        let expr = self.parse_sum()?;
        Ok(expr)
    }

    // + or -
    fn parse_sum(&mut self) -> Result<Expr, String> {
        let left = self.parse_product()?;

        let plus_or_minus = Expect::Any(vec![Expect::Kind(TokenKind::Plus), Expect::Kind(TokenKind::Minus)]);

        match self.accept(plus_or_minus)? {
            Some(token) => {
                let right = self.parse_product()?;
                Ok(Expr::Binary {
                    op: token.text().clone(),
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
            None => Ok(left),
        }
    }

    // * or /
    fn parse_product(&mut self) -> Result<Expr, String> {
        let left = self.parse_term()?;

        let star_or_slash = Expect::Any(vec![Expect::Kind(TokenKind::Star), Expect::Kind(TokenKind::Slash)]);

        match self.accept(star_or_slash)? {
            Some(token) => {
                let right = self.parse_term()?;
                Ok(Expr::Binary {
                    op: token.text().clone(),
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
            None => Ok(left),
        }
    }

    /// Parse the next term
    fn parse_term(&mut self) -> Result<Expr, String> {
        let token = self.next_token()?;

        let term = match token.kind() {
            TokenKind::Int => Expr::Term(Term::Int(token.text().parse().unwrap())),
            TokenKind::Float => Expr::Term(Term::Float(token.text().parse().unwrap())),
            TokenKind::Name => Expr::Term(Term::Name(token.text().clone())),
            _ => return Err(format!("Expected value, got {:?}", token)),
        };

        Ok(term)
    }

    /// Expect a given token
    fn expect(&mut self, expected: Expect) -> Result<Token, String> {
        let actual = self.next_token()?;

        if !expected.matches(&actual) {
            return Err(format!("Expected {:?} but got {:?}", expected, actual));
        }

        Ok(actual)
    }

    /// Peek a token and advance if it matches
    fn accept(&mut self, expected: Expect) -> Result<Option<Token>, String> {
        let actual = match self.lexer.peek() {
            Some(Ok(a)) => a,
            Some(Err(e)) => return Err(e.clone()),
            None => return Ok(None),
        };

        if expected.matches(&actual) {
            Ok(Some(self.next_token()?))
        } else {
            Ok(None)
        }
    }
}
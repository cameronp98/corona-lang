use std::error::Error;
use std::fmt::{self, Display};
use std::iter::Peekable;

use generic_lexer::{Lexer, MatchError};

use crate::lexer::{self, Token, TokenKind};
use crate::vm::{Command, ProgramItem, ToProgramItems};

#[derive(Debug)]
pub enum Expect {
    Kind(TokenKind),
    Text(String),
    Token(Token),
    Any(Vec<Expect>),
}

impl Expect {
    fn matches(&self, token: &Token) -> bool {
        match *self {
            Expect::Kind(kind) => *token.kind() == kind,
            Expect::Text(ref text) => *token.text() == *text,
            Expect::Token(ref expect) => *token.kind() == *expect.kind() && *token.text() == *expect.text(),
            Expect::Any(ref options) => options.iter().any(|opt| opt.matches(token)),
        }
    }
}

/// A value
#[derive(Debug)]
pub enum Term {
    Int(String),
    Float(String),
    Name(String),
}

impl ToProgramItems for Term {
    fn to_program_items(&self) -> Vec<ProgramItem> {
        match *self {
            Term::Int(ref s) => vec![ProgramItem::Data(s.clone()), ProgramItem::Command(Command::CreateInt)],
            Term::Float(ref s) => vec![ProgramItem::Data(s.clone()), ProgramItem::Command(Command::CreateFloat)],
            Term::Name(ref s) => vec![ProgramItem::Data(s.clone()), ProgramItem::Command(Command::ResolveName)],
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match *self {
            Term::Int(ref s) => s,
            Term::Float(ref s) => s,
            Term::Name(ref s) => s,
        };

        s.fmt(f)
    }
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

impl ToProgramItems for Expr {
    fn to_program_items(&self) -> Vec<ProgramItem> {
        match *self {
            Expr::Term(ref term) => term.to_program_items(),
            Expr::Binary { ref op, ref left, ref right } => {
                let mut items = left.to_program_items();
                items.extend(right.to_program_items());
                items.extend(vec![ProgramItem::Data(op.clone()), ProgramItem::Command(Command::BinOp)]);
                items
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Expr::Term(ref x) => x.fmt(f),
            Expr::Binary { ref op, ref left, ref right } => write!(f, "{} {} {}", left, op, right),
        }
    }
}

/// A statement
#[derive(Debug)]
pub enum Statement {
    Let(String, Expr),
    // let <name> = <expr>;
    Expr(Expr, bool), // <expr> (;)?
}

impl ToProgramItems for Statement {
    fn to_program_items(&self) -> Vec<ProgramItem> {
        match *self {
            Statement::Let(ref name, ref expr) => {
                let mut items = vec![ProgramItem::Data(name.clone())];
                items.extend(expr.to_program_items());
                items.push(ProgramItem::Command(Command::Let));
                items
            }
            Statement::Expr(ref expr, _) => expr.to_program_items(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Statement::Let(ref name, ref expr) => write!(f, "let {} = {};", name, expr),
            Statement::Expr(ref expr, semi) => {
                expr.fmt(f)?;
                if semi {
                    write!(f, ";")?;
                }
                Ok(())
            }
        }
    }
}

/// A block (delimited by `{ }`)
#[derive(Debug)]
pub struct Block {
    body: Vec<Statement>,
    // main block body
    end: Option<Expr>, // return expression (rust style, without semicolon)
}


impl Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;

        for statement in &self.body {
            statement.fmt(f)?;
        }

        if let Some(ref expr) = self.end {
            expr.fmt(f)?;
        }

        write!(f, "}}")
    }
}

/// An error occurring at some point during parsing
#[derive(Debug)]
pub enum ParseError {
    Simple(String),
    Match(MatchError),
    ExpectedButGot(Expect, Token),
    UnexpectedEof,
    Unexpected(Token),
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            ParseError::Simple(ref err) => err.fmt(f),
            ParseError::Match(ref err) => err.fmt(f),
            ParseError::ExpectedButGot(ref exp, ref got) => write!(f, "Expected {:?} but got {:?}", exp, got),
            ParseError::UnexpectedEof => write!(f, "Unexpected EOF"),
            ParseError::Unexpected(ref tok) => write!(f, "Unexpected {:?}", tok),
        }
    }
}

impl Error for ParseError {}

impl From<MatchError> for ParseError {
    fn from(err: MatchError) -> Self {
        ParseError::Match(err)
    }
}

/// Result of some part of the parse process
pub type ParseResult<T> = Result<T, ParseError>;

/// A parser
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a, TokenKind>>,
}

impl<'a> Parser<'a> {
    /// Create a new parser
    pub fn new(input: &'a str) -> Self {
        Parser {
            lexer: lexer::lex(input).peekable(),
        }
    }

    // Fetch the next token (if there is one) or return `ParseError::UnexpectedEof`
    // Use peek to test if there is a token beforehand.
    #[inline]
    fn next_token(&mut self) -> ParseResult<Token> {
        self.lexer.next()
            .ok_or(ParseError::UnexpectedEof)?
            .map_err(|e| e.into())
    }

    /// Parse the next item
    pub fn parse_next(&mut self) -> ParseResult<Vec<Statement>> {
        let mut statements = vec![];

        while self.lexer.peek().is_some() {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    /// Parse a statement
    fn parse_statement(&mut self) -> ParseResult<Statement> {
        if let Some(_) = self.accept(Expect::Token(Token::new(TokenKind::Name, "let".to_string())))? {
            self.parse_let()
        } else {
            let expr = self.parse_expr()?;
            let semi = self.accept(Expect::Kind(TokenKind::Semicolon))?.is_some();
            Ok(Statement::Expr(expr, semi))
        }
    }

    /// Parse let statement
    fn parse_let(&mut self) -> ParseResult<Statement> {
        let name = self.expect(Expect::Kind(TokenKind::Name))?;
        let _ = self.expect(Expect::Kind(TokenKind::Equals))?;
        let expr = self.parse_expr()?;
        let _ = self.expect(Expect::Kind(TokenKind::Semicolon));
        Ok(Statement::Let(name.text().clone(), expr))
    }

    /// Parse the next expr
    fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_sum()
    }

    // + or -
    fn parse_sum(&mut self) -> ParseResult<Expr> {
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
    fn parse_product(&mut self) -> ParseResult<Expr> {
        let left = Expr::Term(self.parse_term()?);

        let star_or_slash = Expect::Any(vec![Expect::Kind(TokenKind::Star), Expect::Kind(TokenKind::Slash)]);

        match self.accept(star_or_slash)? {
            Some(token) => {
                let right = Expr::Term(self.parse_term()?);
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
    fn parse_term(&mut self) -> ParseResult<Term> {
        let token = self.next_token()?;

        let term = match token.kind() {
            TokenKind::Int => Term::Int(token.text().parse().unwrap()),
            TokenKind::Float => Term::Float(token.text().parse().unwrap()),
            TokenKind::Name => Term::Name(token.text().to_string()),
            // TokenKind::OpenBrace => self.parse_block()?,
            _ => return Err(ParseError::Unexpected(token)),
        };

        Ok(term)
    }

    /// Parse a block
    fn parse_block(&mut self) -> ParseResult<Block> {
        unimplemented!()
    }

    /// Expect a given token
    fn expect(&mut self, expected: Expect) -> ParseResult<Token> {
        let actual = self.next_token()?;

        if !expected.matches(&actual) {
            return Err(ParseError::ExpectedButGot(expected, actual));
        }

        Ok(actual)
    }

    /// Peek a token and advance if it matches
    fn accept(&mut self, expected: Expect) -> ParseResult<Option<Token>> {
        // Return Ok(None) if there is no token or if there is and it does not match `expected`
        match self.lexer.peek() {
            None => return Ok(None),
            Some(Ok(token)) => {
                if !expected.matches(token) {
                    return Ok(None)
                }
            },
            _ => {},
        }

        // otherwise unwrap the token because we will now know there will be one
        // and wrap it in an option
        self.next_token().map(|t| Some(t))
    }
}
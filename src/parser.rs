use std::fmt::{self, Display, Error, Formatter};
use std::iter::Peekable;

use crate::lexer::{Lexer, Token, TokenKind};
use crate::vm::{Command, ProgramItem, ToProgramItems};

#[derive(Debug, Eq, PartialEq)]
enum Expect {
    Kind(TokenKind),
    Text(String),
    Token(Token),
    Any(Vec<Expect>),
}

impl Expect {
    fn matches(&self, token: &Token) -> bool {
        match *self {
            Expect::Kind(ref kind) => *token.kind() == *kind,
            Expect::Text(ref text) => *token.text() == *text,
            Expect::Token(ref expect) => *token.kind() == *expect.kind() && *token.text() == *expect.text(),
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
    pub fn parse_next(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements = vec![];

        while self.lexer.peek().is_some() {
            statements.push(self.parse_statement()?);
        }

        Ok(statements)
    }

    /// Parse a statement
    fn parse_statement(&mut self) -> Result<Statement, String> {
        if let Some(_) = self.accept(Expect::Token(Token::new(TokenKind::Name, String::from("let"))))? {
            self.parse_let()
        } else {
            let expr = self.parse_expr()?;
            let semi = self.accept(Expect::Kind(TokenKind::Semicolon))?.is_some();
            Ok(Statement::Expr(expr, semi))
        }
    }

    /// Parse let statement
    fn parse_let(&mut self) -> Result<Statement, String> {
        let name = self.expect(Expect::Kind(TokenKind::Name))?;
        let _ = self.expect(Expect::Kind(TokenKind::Equals))?;
        let expr = self.parse_expr()?;
        let _ = self.expect(Expect::Kind(TokenKind::Semicolon));
        Ok(Statement::Let(name.text().clone(), expr))
    }

    /// Parse the next expr
    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_sum()
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
            TokenKind::OpenBrace => self.parse_block()?,
            _ => return Err(format!("Expected value, got {:?}", token)),
        };

        Ok(term)
    }

    /// Parse a block
    fn parse_block(&mut self) -> Result<Expr, String> {
        unimplemented!()
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
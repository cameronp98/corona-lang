use generic_lexer::{Lexer, LexerInput, MatchError, MatchResult, Token as GenericToken};

pub type Token = GenericToken<TokenKind>;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum TokenKind {
    // Name / Literals.
    Name,
    Int,
    Float,

    // Arithmetic
    Plus,
    Minus,
    Slash,
    Star,

    // Comparison
    Equals,
    DoubleEquals,
    NotEquals,
    LeftArrow,
    LessOrEqual,
    RightArrow,
    GreaterOrEqual,

    // Misc
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,

    // Punctuation
    Bang,
    Semicolon,
    Comma,
    Dot,
}

/// Lex an entire input string
pub fn lex(input: &str) -> Lexer<TokenKind> {
    Lexer::new(input, &match_next, true)
}

// Lex the next token
fn match_next(first_char: char, input: &mut LexerInput) -> MatchResult<TokenKind> {
    let kind = match first_char {
        c if is_name(&c) => match_name(input),
        c if c.is_ascii_digit() => match_int(input),

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

        // double character tokens
        '=' => input.accept_or(|c| *c == '=', TokenKind::DoubleEquals, TokenKind::Equals),
        '!' => input.accept_or(|c| *c == '=', TokenKind::NotEquals, TokenKind::Bang),
        '<' => input.accept_or(|c| *c == '=', TokenKind::LessOrEqual, TokenKind::LeftArrow),
        '>' => input.accept_or(|c| *c == '>', TokenKind::GreaterOrEqual, TokenKind::RightArrow),

        // single punctuation
        ';' => TokenKind::Semicolon,
        ',' => TokenKind::Comma,
        '.' => TokenKind::Dot,

        c => return Err(MatchError::Unexpected(c)),
    };

    Ok(kind)
}

// Lex an integer
fn match_int(input: &mut LexerInput) -> TokenKind {
    input.accept_while(char::is_ascii_digit);
    if let Some(_) = input.accept(|c| *c == '.') {
        return match_float(input);
    }
    TokenKind::Int
}

// Lex a float
fn match_float(input: &mut LexerInput) -> TokenKind {
    input.accept_while(char::is_ascii_digit);
    TokenKind::Float
}

// Match the next name
fn match_name(input: &mut LexerInput) -> TokenKind {
    input.accept_while(is_name);
    TokenKind::Name
}

// Match a name character
fn is_name(c: &char) -> bool {
    c.is_ascii_alphabetic() || *c == '_'
}

use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

use lexer::Lexer;
use parser::Parser;

mod lexer;
mod parser;

fn main() -> Result<(), Box<dyn Error>> {
    let path = "input.pc";

    let mut file = File::open(path)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;

    let mut parser = Parser::new(Lexer::new(input.as_ref()));

    println!("{:?}", parser.parse_next());

    Ok(())
}

use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

use parser::Parser;
use vm::Program;

use crate::vm::ToProgramItems;

mod lexer;
mod parser;
mod vm;


fn main() -> Result<(), Box<dyn Error>> {
    let path = "input.pc";

    let mut file = File::open(path)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;

    let mut parser = Parser::new(&input);

    for statement in parser.parse_next()? {
        let program = Program::new(statement.to_program_items());
        let values = program.evaluate()?;
        if values.len() > 0 {
            println!("{:?}", values);
        }
    }

    Ok(())
}

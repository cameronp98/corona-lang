use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

use parser::Parser;
use vm::Vm;

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

    let mut vm = Vm::new();

    for statement in parser.parse_next()? {
        vm.evaluate(&statement.to_program_items())?;
    }

    println!("{:#?}", vm.vars());
    let values = vm.finish();
    println!("{:#?}", values);

    Ok(())
}

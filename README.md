# corona-lang
yep its the all time favourite hobby of self-isolating quarantiners...
Programming!

## Roadmmap

- [ ] multi-character operator tokens (`==`, `!=` etc.)
- [ ] semantic analysis
- [ ] expression evaluation
- [ ] user friendliness
- [ ] example programs
- [ ] flow control, for loops
- [ ] I/O
- [ ] objects / functions etc.
- [ ] dynamic or static????? GC???????????

# Example usage

```rust
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
```

```
CreateInt
Let
CreateInt
Let
CreateFloat
Let
ResolveName
ResolveName
BinOp
unimplemented command BinOp
ResolveName
BinOp
unimplemented command BinOp
Let
{
    "c": Float(
        3.14,
    ),
    "+": Float(
        3.14,
    ),
    "b": Int(
        69,
    ),
    "a": Int(
        42,
    ),
}
[
    Int(
        42,
    ),
    Int(
        69,
    ),
]
```
use crate::parser::Statement;

pub(crate) trait ToProgramItems {
    fn to_program_items(&self) -> Vec<ProgramItem>;
}

#[derive(Debug, Clone)]
/// A runtime value
pub(crate) enum Value {
    Int(i64),
    Float(f64),
    Nil,
}

#[derive(Debug, Clone)]
pub enum Command {
    CreateInt,
    CreateFloat,

    ResolveName,

    Let,

    BinOp,
}

/// A stack item
#[derive(Debug, Clone)]
pub(crate) enum ProgramItem {
    Command(Command),
    Data(String),
}

struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    fn new() -> Stack<T> {
        Stack(vec![])
    }

    fn push(&mut self, item: T) {
        self.0.push(item)
    }

    fn pop(&mut self) -> Result<T, String> {
        self.0.pop().ok_or(String::from("error: stack empty"))
    }
}

pub(crate) struct Program {
    items: Vec<ProgramItem>,
    stack: Stack<String>,
    values: Stack<Value>,
}

impl Program {
    pub(crate) fn new(items: Vec<ProgramItem>) -> Program {
        Program {
            items,
            stack: Stack::new(),
            values: Stack::new(),
        }
    }

    fn command(&mut self, cmd: Command) -> Result<Option<Value>, String> {
        let value = match cmd {
            Command::CreateInt => {
                let item = self.stack.pop()?;
                let value = item.parse().map_err(|_| format!("Failed to parse int {}", item))?;
                Some(Value::Int(value))
            }
            Command::Let => {
                let value = self.values.pop()?;
                let name = self.stack.pop()?;
                println!("let {} = {:?}", name, value);
                None
            }
            _ => return Err(format!("Unimplemented command {:?}", cmd)),
        };

        Ok(value)
    }

    pub(crate) fn evaluate(mut self) -> Result<Vec<Value>, String> {
        let items = self.items.clone().into_iter();

        for item in items {
            match item {
                ProgramItem::Command(cmd) => {
                    if let Some(value) = self.command(cmd)? {
                        self.values.push(value)
                    }
                }
                ProgramItem::Data(data) => self.stack.push(data.clone()),
            }
        }

        let Stack(values) = self.values;

        Ok(values)
    }
}
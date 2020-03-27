use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::str::FromStr;

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

    fn pop(&mut self) -> VmResult<T> {
        self.0.pop().ok_or(VmError::StackEmpty)
    }
}

#[derive(Debug)]
pub enum VmError {
    UndefinedVariable(String),
    ParseError(String),
    StackEmpty,
}

impl fmt::Display for VmError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VmError::UndefinedVariable(ref name) => write!(f, "Undefined variable '{}'", name),
            VmError::StackEmpty => write!(f, "Tried to pop but stack was empty"),
            VmError::ParseError(ref item) => write!(f, "Failed to parse '{}'", item),
        }
    }
}

impl Error for VmError {}

type VmResult<T> = Result<T, VmError>;

pub(crate) struct Vm {
    /// Stack for program data
    data_stack: Stack<String>,
    /// Stack for runtime values
    value_stack: Stack<Value>,
    /// Runtime variables
    vars: HashMap<String, Value>,
}

impl Vm {
    pub(crate) fn new() -> Vm {
        Vm {
            // items,
            data_stack: Stack::new(),
            value_stack: Stack::new(),
            vars: HashMap::new(),
        }
    }

    pub fn vars(&self) -> &HashMap<String, Value> {
        &self.vars
    }

    fn pop_parse<T: FromStr>(&mut self) -> VmResult<T> {
        let item = self.data_stack.pop()?;
        item.parse().map_err(|_| VmError::ParseError(item.to_string()))
    }

    fn command(&mut self, cmd: &Command) -> VmResult<()> {
        println!("{:?}", cmd);

        match *cmd {
            Command::CreateInt => {
                let value = self.pop_parse()?;
                self.value_stack.push(Value::Int(value));
            }
            Command::CreateFloat => {
                let value = self.pop_parse()?;
                self.value_stack.push(Value::Float(value));
            }
            Command::Let => {
                let value = self.value_stack.pop()?;
                let name = self.data_stack.pop()?;
                self.vars.insert(name, value);
            }
            Command::ResolveName => {
                // currently pass by value
                let name = self.data_stack.pop()?;
                match self.vars.get(&name) {
                    Some(value) => self.value_stack.push(value.clone()),
                    None => return Err(VmError::UndefinedVariable(name)),
                }
            }
            _ => {
                println!("unimplemented command {:?}", cmd);
            },
        }

        Ok(())
    }

    pub(crate) fn evaluate(&mut self, items: &[ProgramItem]) -> VmResult<()> {
        for item in items {
            match item {
                ProgramItem::Command(cmd) => self.command(cmd)?,
                ProgramItem::Data(data) => self.data_stack.push(data.clone()),
            }
        }

        Ok(())
    }

    pub fn finish(self) -> Vec<Value> {
        let Stack(values) = self.value_stack;
        values
    }
}
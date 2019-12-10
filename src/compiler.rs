use crate::{ast::{Node}, code::Instructions, object::Object};

#[derive(Default)]
pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        let instructions = Instructions { stream: Vec::new() };
        let constants: Vec<Object> = Vec::new();

        Compiler {
            instructions,
            constants,
        }
    }

    pub fn compile(&mut self, node: Node) -> Result<(), CompilerError> {
        Ok(())
    }
}

#[derive(Debug)]
pub enum CompilerError{

}
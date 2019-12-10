use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use std::{error, fmt, io, result};

pub fn make(op: OpCode, operands: &[usize]) -> Result<Vec<u8>, CodeError> {
    let def = lookup(op)?;
    let mut buf = Vec::with_capacity(instr_size(&def.operand_widths));

    let byte = match op {
        OpCode::Control(c) => c as u8,
    };
    buf.push(byte);

    for (oper, width) in operands.iter().zip(def.operand_widths.iter()) {
        match width {
            Width::One => buf.write_u8(*oper as u8).map_err(CodeError::Io)?,
            Width::Two => buf
                .write_u16::<BigEndian>(*oper as u16)
                .map_err(CodeError::Io)?,
        };
    }

    Ok(buf)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum OpCode {
    Control(ControlOpCode),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ControlOpCode {
    I32Const = 0x41,
}

impl From<u8> for ControlOpCode {
    fn from(v: u8) -> Self {
        match v {
            0x41 => ControlOpCode::I32Const,
            _ => panic!("unhandled u8 to ControlOpcode conversion: {}", v),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Width {
    One = 1,
    Two = 2,
}

#[derive(Clone, Debug)]
struct Definition<'a> {
    name: &'a str,
    operand_widths: Vec<Width>,
}

fn lookup<'a>(op: OpCode) -> Result<Definition<'a>, CodeError> {
    match op {
        OpCode::Control(c) => match c {
            ControlOpCode::I32Const => Ok(Definition {
                name: "I32Const",
                operand_widths: vec![Width::Two],
            }),
            _ => Err(CodeError::UknownOpCode(format!(
                "unkwown definition for opcode {:?}",
                op
            ))),
        },
    }
}

#[derive(Debug,Default, PartialEq)]
pub struct Instructions {
    pub stream: Vec<(usize, OpCode, Vec<usize>)>,
}

impl Instructions {
    pub fn parse(buf: &[u8]) -> Result<Self, CodeError> {
        Ok(Instructions { stream: Vec::new() })
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (idx, op, operands) in &self.stream {
            writeln!(f, "{:04} {:?} {:?}", idx, op, operands)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum CodeError {
    Error,
    UknownOpCode(String),
    Io(io::Error),
}

fn instr_size(operand_widths: &Vec<Width>) -> usize {
    operand_widths.iter().map(|w| *w as usize).sum::<usize>() + 1
}

#[test]
fn code_make_instruction() {
    let tests = vec![(
        OpCode::Control(ControlOpCode::I32Const),
        vec![65534],
        vec![ControlOpCode::I32Const as u8, 0xff, 0xfe],
    )];

    for (op, operands, want) in &tests {
        let instr = make(*op, operands).expect("make returned an error");
        assert_eq!(*want, instr);
        assert_eq!(instr.len(), want.len());

        for i in 0..instr.len() {
            assert_eq!(instr[i], want[i]);
        }
    }
}

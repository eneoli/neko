use std::{
    fs::File,
    io::{self, Write},
    path::Path,
    process::Command,
};

use rand::distr::{Alphanumeric, SampleString};

use crate::compile::ir::sea::Sea;

pub mod codegen;
pub mod inst;
pub mod regalloc;

#[derive(Clone, Debug)]
pub enum MachineRegister {
    EBX,
    ECX,
    ESI,
    EDI,
    R8D,
    R9D,
    R10D,
    R11D,
    R12D,
    R13D,
    R14D,

    // special/reserved
    RBP,
    RSP,
    EAX,
    R15D, // spill
}

#[derive(Clone, Debug)]
pub enum Register {
    Machine(MachineRegister),
    Temp(u64),
}

#[derive(Clone, Debug)]
pub enum Location {
    Register(Register),
    Memory(u64),
}

#[derive(Clone, Debug)]
pub enum Value {
    Location(Location),
    Immediate(u32),
}

#[derive(Clone, Debug)]
pub enum Asm {
    NOP,
    MOV(Location, Value),
    NEG(Location),
    ADD(Location, Value),    // Two memory operands forbidden
    SUB(Location, Value),    // Two memory operands forbidden
    MUL(Register, Location), // dest is AL, AX or EAX
    DIV(Register, Location), // dest is AX, DX:AX, or EDX:EAX
    PUSH(Value),
    LEAVE,
    RET(Option<u32>),
}

pub fn generate_assembly(sea: &Sea) -> String {
    let mut asm = inst::select(sea);
    regalloc::allocate(&mut asm);
    codegen::generate(&asm)
}

pub fn assemble(sea: &Sea, out_path: &Path) -> Result<(), io::Error> {
    let Some(binary_file_name) = out_path.file_name() else {
        panic!("Invalid file name.")
    };

    let Some(binary_path_parent) = out_path.parent() else {
        panic!("Invalid parent.")
    };

    let asm = generate_assembly(sea);

    let slug: String = Alphanumeric.sample_string(&mut rand::rng(), 8);
    let asm_file_name = format!("{}.{}.s", binary_file_name.to_str().unwrap(), slug);

    let mut asm_file = File::create(format!(
        "{}{}",
        binary_path_parent.as_os_str().to_str().unwrap(),
        asm_file_name
    ))
    .unwrap();

    asm_file.write(asm.as_bytes())?;
    asm_file.flush()?;

    Command::new("gcc")
        .arg(binary_path_parent.as_os_str().to_str().unwrap().to_owned() + &asm_file_name)
        .arg("-o")
        .arg(out_path)
        .status()?;

    Ok(())
}

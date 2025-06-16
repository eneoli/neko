use std::{
    fs::File,
    io::{self, Write},
    path::Path,
    process::Command,
};

use rand::distr::{Alphanumeric, SampleString};

use crate::compile::ir::graph::IrGraph;

pub mod codegen;
pub mod inst;
pub mod regalloc;

pub type Label = usize;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
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
    EDX,
    R15D, // spill
}

impl MachineRegister {
    pub const SPILL: MachineRegister = MachineRegister::R15D;

    pub fn num_free_regs() -> usize {
        11
    }

    pub fn from_index(idx: u64) -> Option<MachineRegister> {
        match idx {
            0 => Some(MachineRegister::EBX),
            1 => Some(MachineRegister::ECX),
            2 => Some(MachineRegister::ESI),
            3 => Some(MachineRegister::EDI),
            4 => Some(MachineRegister::R8D),
            5 => Some(MachineRegister::R9D),
            6 => Some(MachineRegister::R10D),
            7 => Some(MachineRegister::R11D),
            8 => Some(MachineRegister::R12D),
            9 => Some(MachineRegister::R13D),
            10 => Some(MachineRegister::R14D),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Register {
    Machine(MachineRegister),
    Temp(usize),
}

#[derive(Clone, Copy, Debug)]
pub enum Location {
    Register(Register),
    Memory(u64),
    Immediate(u32),
}

impl Location {
    pub fn temp(id: usize) -> Location {
        Location::Register(Register::Temp(id))
    }
    pub fn register(reg: MachineRegister) -> Location {
        Location::Register(Register::Machine(reg))
    }

    pub fn memory(id: u64) -> Location {
        Location::Memory(id)
    }

    pub fn from_index(idx: u64) -> Location {
        if idx < MachineRegister::num_free_regs() as u64 {
            return Location::register(MachineRegister::from_index(idx).unwrap());
        }

        Location::Memory(idx - MachineRegister::num_free_regs() as u64)
    }

    pub fn get_register(&self) -> Option<Register> {
        match self {
            Self::Register(reg) => Some(*reg),
            _ => None,
        }
    }

    pub fn get_register_mut(&mut self) -> Option<&mut Register> {
        match self {
            Self::Register(reg) => Some(reg),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    NOP,
    Label(Label),
    MOV(Location, Location),  // Two memory operands forbidden
    ADD(Location, Location),  // Two memory operands forbidden
    SUB(Location, Location),  // Two memory operands forbidden
    IMUL(Location, Location), // Dest can only be a register
    AND(Location, Location),  // Two memory operands forbidden
    OR(Location, Location),   // Two memory operands forbidden
    XOR(Location, Location),  // Two memory operands forbidden
    SAL(Location, Location),  // TODO
    SAR(Location, Location),  // TODO
    EQ(Location, Location),   // Result in dest, Stub
    NEQ(Location, Location),  // Result in dest, Stub
    LT(Location, Location),   // Result in dest, Stub
    LE(Location, Location),   // Result in dest, Stub
    NEG(Location),
    IDIV(Location), // dest is AX, DX:AX, or EDX:EAX
    CDQ,
    PUSH(Location),
    LEAVE,
    RET(Option<u32>),
    JMP(Label),
    CJMP(Location, Label, Label),
}

impl Instruction {
    pub fn locations(&self) -> Vec<&Location> {
        match self {
            Self::MOV(dest, src)
            | Self::ADD(dest, src)
            | Self::SUB(dest, src)
            | Self::IMUL(dest, src)
            | Self::AND(dest, src)
            | Self::OR(dest, src)
            | Self::XOR(dest, src)
            | Self::SAL(dest, src)
            | Self::SAR(dest, src)
            | Self::EQ(dest, src)
            | Self::NEQ(dest, src)
            | Self::LT(dest, src)
            | Self::LE(dest, src) => vec![dest, src],

            Self::NEG(loc) | Self::IDIV(loc) | Self::PUSH(loc) | Self::CJMP(loc, _, _) => vec![loc],
            Self::NOP | Self::Label(_) | Self::CDQ | Self::LEAVE | Self::RET(_) | Self::JMP(_) => {
                Vec::new()
            }
        }
    }

    pub fn locations_mut(&mut self) -> Vec<&mut Location> {
        match self {
            Self::MOV(dest, src)
            | Self::ADD(dest, src)
            | Self::SUB(dest, src)
            | Self::IMUL(dest, src)
            | Self::AND(dest, src)
            | Self::OR(dest, src)
            | Self::XOR(dest, src)
            | Self::SAL(dest, src)
            | Self::SAR(dest, src)
            | Self::EQ(dest, src)
            | Self::NEQ(dest, src)
            | Self::LT(dest, src)
            | Self::LE(dest, src) => vec![dest, src],

            Self::NEG(loc) | Self::IDIV(loc) | Self::PUSH(loc) | Self::CJMP(loc, _, _) => vec![loc],
            Self::NOP | Self::Label(_) | Self::CDQ | Self::LEAVE | Self::RET(_) | Self::JMP(_) => {
                Vec::new()
            }
        }
    }

    pub fn registers(&self) -> Vec<Register> {
        self.locations()
            .iter()
            .map(|loc| loc.get_register())
            .filter(|reg| reg.is_some())
            .map(Option::unwrap)
            .collect()
    }

    pub fn registers_mut(&mut self) -> Vec<&mut Register> {
        self.locations_mut()
            .into_iter()
            .map(|loc| loc.get_register_mut())
            .filter(|reg| reg.is_some())
            .map(Option::unwrap)
            .collect()
    }

    pub fn written_registers(&self) -> Vec<Register> {
        match self {
            Self::MOV(dest, _)
            | Self::ADD(dest, _)
            | Self::SUB(dest, _)
            | Self::IMUL(dest, _)
            | Self::NEG(dest)
            | Self::IDIV(dest)
            | Self::AND(dest, _)
            | Self::OR(dest, _)
            | Self::XOR(dest, _)
            | Self::SAL(dest, _)
            | Self::SAR(dest, _)
            | Self::EQ(dest, _)
            | Self::NEQ(dest, _)
            | Self::LT(dest, _)
            | Self::LE(dest, _) => {
                if let Some(reg) = dest.get_register() {
                    return vec![reg];
                }

                Vec::new()
            }

            Self::NOP
            | Self::Label(_)
            | Self::CDQ
            | Self::LEAVE
            | Self::RET(_)
            | Self::PUSH(_)
            | Self::JMP(_)
            | Self::CJMP(_, _, _) => Vec::new(),
        }
    }
}

pub struct Asm {
    asm: Vec<Instruction>,
    stack_size: usize,
}

impl Asm {
    pub fn new(asm: Vec<Instruction>, stack_size: usize) -> Self {
        Self { asm, stack_size }
    }

    pub fn len(&self) -> usize {
        self.asm.len()
    }

    pub fn instructions(&self) -> &Vec<Instruction> {
        &self.asm
    }

    pub fn instructions_mut(&mut self) -> &mut Vec<Instruction> {
        &mut self.asm
    }

    pub fn stack_size(&self) -> usize {
        self.stack_size
    }
}

pub fn generate_assembly(ir: &IrGraph) -> String {
    let instructions = inst::select(ir);
    println!("Inst select done");
    // for inst in instructions.0.iter() {
    //     println!("{:?}", inst);
    // }
    // println!("{:#?}", instructions);
    let asm = regalloc::allocate(instructions);
    println!("Allocation done");
    let x = codegen::generate(&asm).unwrap();
    println!("Codegen done");
    x
}

pub fn assemble(ir: &IrGraph, out_path: &Path) -> Result<(), io::Error> {
    let Some(binary_file_name) = out_path.file_name() else {
        panic!("Invalid file name.")
    };

    let Some(binary_path_parent) = out_path.parent() else {
        panic!("Invalid parent.")
    };

    let asm = generate_assembly(ir);

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

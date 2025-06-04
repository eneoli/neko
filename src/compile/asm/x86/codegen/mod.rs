use std::fmt::{Display, Write};

use crate::{
    compile::asm::x86::{Instruction, Location, MachineRegister, Register},
    infra::NekoError,
};

use super::Asm;

impl Display for MachineRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EBX => write!(f, "ebx"),
            Self::ECX => write!(f, "ecx"),
            Self::ESI => write!(f, "esi"),
            Self::EDI => write!(f, "edi"),
            Self::R8D => write!(f, "r8d"),
            Self::R9D => write!(f, "r9d"),
            Self::R10D => write!(f, "r10d"),
            Self::R11D => write!(f, "r11d"),
            Self::R12D => write!(f, "r12d"),
            Self::R13D => write!(f, "r13d"),
            Self::R14D => write!(f, "r14d"),

            Self::RBP => write!(f, "rbp"),
            Self::RSP => write!(f, "rsp"),
            Self::R15D => write!(f, "r15d"),
            Self::EAX => write!(f, "eax"),
            Self::EDX => write!(f, "edx"),
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(Register::Temp(_)) => {
                panic!("Temps cannot appear in this stage of the compiler anymore")
            }
            Self::Register(Register::Machine(reg)) => write!(f, "{}", reg),
            Self::Memory(i) => write!(f, "DWORD PTR [rbp-{}]", 4 * i),
            Self::Immediate(x) => write!(f, "{}", x),
        }
    }
}

pub fn generate(instructions: &Asm) -> Result<String, NekoError> {
    let mut asm = String::from(include_str!("base.s"));

    // Allocate stack
    if instructions.stack_size() > 0 {
        writeln!(&mut asm, "PUSH rbp")?;
        writeln!(&mut asm, "MOV rbp, rsp")?;
        writeln!(&mut asm, "SUB rsp, {}", 4 * instructions.stack_size)?;
    }

    for inst in instructions.instructions().iter() {
        match inst {
            Instruction::MOV(dest, src) => {
                if let (Location::Memory(dest), Location::Memory(src)) = (dest, src) {
                    // Two memory operators forbidden
                    // Load src in Spill
                    writeln!(
                        &mut asm,
                        "MOV {}, DWORD PTR [rbp-{}]",
                        MachineRegister::SPILL,
                        4 * src
                    )?;
                    writeln!(
                        &mut asm,
                        "MOV [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL
                    )?;

                    continue;
                }

                writeln!(&mut asm, "MOV {dest}, {src}")?
            }
            Instruction::NEG(dest) => {
                if let Location::Immediate(_) = dest {
                    panic!(
                        "Cannot negate immediate. It has to be a register or a memory location."
                    );
                }

                writeln!(&mut asm, "NEG {dest}")?
            }
            Instruction::ADD(dest, src) => {
                if let (Location::Memory(dest), Location::Memory(src)) = (dest, src) {
                    // Two memory operators forbidden
                    // Load src in Spill
                    writeln!(
                        &mut asm,
                        "MOV {}, DWORD PTR [rbp-{}]",
                        MachineRegister::SPILL,
                        4 * src
                    )?;
                    writeln!(
                        &mut asm,
                        "ADD DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL
                    )?;

                    continue;
                }

                writeln!(&mut asm, "ADD {dest}, {src}")?;
            }
            Instruction::SUB(dest, src) => {
                if let (Location::Memory(dest), Location::Memory(src)) = (dest, src) {
                    // Two memory operators forbidden
                    // Load src in Spill
                    writeln!(
                        &mut asm,
                        "MOV {}, DWORD PTR [rbp-{}]",
                        MachineRegister::SPILL,
                        4 * src
                    )?;
                    writeln!(
                        &mut asm,
                        "SUB DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL
                    )?;

                    continue;
                }

                writeln!(&mut asm, "SUB {}, {}", dest, src)?;
            }

            Instruction::IMUL(dest, src) => {
                // Dest can only be a register
                match dest {
                    Location::Register(Register::Temp(_)) => {
                        panic!("Found temporary register while codegen.")
                    }
                    Location::Register(Register::Machine(dest)) => {
                        writeln!(&mut asm, "IMUL {}, {}", dest, src)?;
                    }
                    Location::Memory(i) => {
                        writeln!(
                            &mut asm,
                            "MOV {}, DWORD PTR [rbp-{}]",
                            MachineRegister::SPILL,
                            4 * i
                        )?;

                        writeln!(&mut asm, "IMUL {}, {}", MachineRegister::SPILL, src)?;
                        writeln!(&mut asm, "MOV {}, {}", dest, MachineRegister::SPILL)?;
                    }
                    Location::Immediate(x) => {
                        writeln!(&mut asm, "MOV {}, {}", MachineRegister::SPILL, *x)?;
                        writeln!(&mut asm, "IMUL {}, {}", MachineRegister::SPILL, src)?;
                        writeln!(&mut asm, "MOV {}, {}", dest, MachineRegister::SPILL)?;
                    }
                };
            }
            Instruction::IDIV(src) => writeln!(&mut asm, "IDIV {}", src)?, // TODO only certain registers
            Instruction::CDQ => {
                writeln!(&mut asm, "CDQ")?;
            }
            Instruction::PUSH(loc) => writeln!(&mut asm, "PUSH {}", loc)?,
            Instruction::LEAVE => writeln!(&mut asm, "LEAVE")?,
            Instruction::RET(i) => {
                // Deallocate stack
                if instructions.stack_size() > 0 {
                    writeln!(&mut asm, "LEAVE")?;
                }

                if let Some(i) = i {
                    writeln!(&mut asm, "RET {i}")?
                } else {
                    writeln!(&mut asm, "RET")?
                }
            }
            Instruction::NOP => {}
        }
    }

    Ok(asm)
}

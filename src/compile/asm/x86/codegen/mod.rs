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
            Instruction::IDIV(src) => writeln!(&mut asm, "IDIV {}", src)?,
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
            Instruction::AND(dest, src) => {
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
                        "AND DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL
                    )?;

                    continue;
                }

                writeln!(&mut asm, "AND {dest}, {src}")?;
            }
            Instruction::OR(dest, src) => {
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
                        "OR DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL
                    )?;

                    continue;
                }

                writeln!(&mut asm, "OR {dest}, {src}")?;
            }
            Instruction::XOR(dest, src) => {
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
                        "XOR DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL
                    )?;

                    continue;
                }

                writeln!(&mut asm, "XOR {dest}, {src}")?;
            }
            Instruction::SAL(dest, src) => todo!(),
            Instruction::SAR(dest, src) => todo!(),
            Instruction::EQ(dest, src) => {
                let src = spill(&mut asm, dest, src)?;

                writeln!(&mut asm, "CMP {dest}, {src}")?;
                writeln!(&mut asm, "SETE AL")?;

                if let Location::Memory(dest) = dest {
                    writeln!(&mut asm, "MOVZX {}, AL", MachineRegister::SPILL)?;
                    writeln!(
                        asm,
                        "MOV DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL,
                    )?;
                } else {
                    writeln!(&mut asm, "MOVZX {dest}, AL")?;
                }
            }
            Instruction::NEQ(dest, src) => {
                let src = spill(&mut asm, dest, src)?;

                writeln!(&mut asm, "CMP {dest}, {src}")?;
                writeln!(&mut asm, "SETNE AL")?;

                if let Location::Memory(dest) = dest {
                    writeln!(&mut asm, "MOVZX {}, AL", MachineRegister::SPILL)?;
                    writeln!(
                        asm,
                        "MOV DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL,
                    )?;
                } else {
                    writeln!(&mut asm, "MOVZX {dest}, AL")?;
                }
            }
            Instruction::LT(dest, src) => {
                let src = spill(&mut asm, dest, src)?;

                writeln!(&mut asm, "CMP {dest}, {src}")?;
                writeln!(&mut asm, "SETL AL")?;

                if let Location::Memory(dest) = dest {
                    writeln!(&mut asm, "MOVZX {}, AL", MachineRegister::SPILL)?;
                    writeln!(
                        asm,
                        "MOV DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL,
                    )?;
                } else {
                    writeln!(&mut asm, "MOVZX {dest}, AL")?;
                }
            }
            Instruction::LE(dest, src) => {
                let src = spill(&mut asm, dest, src)?;

                writeln!(&mut asm, "CMP {dest}, {src}")?;
                writeln!(&mut asm, "SETLE AL")?;

                if let Location::Memory(dest) = dest {
                    writeln!(&mut asm, "MOVZX {}, AL", MachineRegister::SPILL)?;
                    writeln!(
                        asm,
                        "MOV DWORD PTR [rbp-{}], {}",
                        4 * dest,
                        MachineRegister::SPILL,
                    )?;
                } else {
                    writeln!(&mut asm, "MOVZX {dest}, AL")?;
                }
            }
            Instruction::JMP(label) => {
                writeln!(&mut asm, "JMP .block_{label}")?;
            }
            Instruction::CJMP(cond, then, otherwise) => {
                writeln!(&mut asm, "CMP {cond}, 0")?;
                writeln!(&mut asm, "JE .block_{otherwise}")?;
                writeln!(&mut asm, "JMP .block_{then}")?;
            }
            Instruction::Label(label) => {
                writeln!(&mut asm, ".block_{label}:")?;
            }
            Instruction::NOP => {}
        }
    }

    Ok(asm)
}

fn spill(asm: &mut String, dest: &Location, src: &Location) -> Result<Location, NekoError> {
    if let (Location::Memory(_), Location::Memory(src)) = (dest, src) {
        // Two memory operators forbidden
        // Load src in Spill
        writeln!(
            asm,
            "MOV {}, DWORD PTR [rbp-{}]",
            MachineRegister::SPILL,
            4 * src
        )?;

        return Ok(Location::register(MachineRegister::SPILL));
    }

    Ok(*src)
}

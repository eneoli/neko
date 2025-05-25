use std::{
    collections::HashMap,
    fmt::Display,
    fs::File,
    io::{self, Write},
    path::Path,
    process::Command,
};

// TODO this has to be rewritten completely in order to be a proper *maximal* munch
// and because the code looks like shit.
// In the future this gets replaced by a proper IR, but I'm on a time crunch right now...

pub mod regalloc;

use rand::distr::{Alphanumeric, SampleString};
use regalloc::Allocator;

use super::ast::{AST, AssignOp, BinaryOp, Expr, Stmt};

type Assembly<'a> = &'a str;
type Temp = usize;

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    Stack(usize),
}

impl MachineRegister {
    pub fn from_index(i: usize) -> Self {
        match i {
            0 => MachineRegister::EBX,
            1 => MachineRegister::ECX,
            2 => MachineRegister::ESI,
            3 => MachineRegister::EDI,
            4 => MachineRegister::R8D,
            5 => MachineRegister::R9D,
            6 => MachineRegister::R10D,
            7 => MachineRegister::R11D,
            8 => MachineRegister::R12D,
            9 => MachineRegister::R13D,
            10 => MachineRegister::R14D,
            _ => MachineRegister::Stack(i),
        }
    }
}

impl Display for MachineRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MachineRegister::EBX => write!(f, "ebx"),
            MachineRegister::ECX => write!(f, "ecx"),
            MachineRegister::ESI => write!(f, "esi"),
            MachineRegister::EDI => write!(f, "edi"),
            MachineRegister::R8D => write!(f, "r8d"),
            MachineRegister::R9D => write!(f, "r9d"),
            MachineRegister::R10D => write!(f, "r10d"),
            MachineRegister::R11D => write!(f, "r11d"),
            MachineRegister::R12D => write!(f, "r12d"),
            MachineRegister::R13D => write!(f, "r13d"),
            MachineRegister::R14D => write!(f, "r14d"),

            MachineRegister::RBP => write!(f, "rbp"),
            MachineRegister::RSP => write!(f, "rsp"),
            MachineRegister::R15D => write!(f, "r15d"),
            MachineRegister::EAX => write!(f, "eax"),
            MachineRegister::Stack(i) => write!(f, "DWORD PTR [rbp-{}]", 4 * i),
        }
    }
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Register {
    Temp(Temp),
    Machine(MachineRegister),
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::Temp(temp) => write!(f, "%{}", temp),
            Register::Machine(reg) => write!(f, "{}", reg),
        }
    }
}

#[derive(Clone, Debug)]
pub enum AsmIr {
    NOP,
    MOV(Register, Register),
    MOVI(Register, u32),
    NEG(Register),
    ADD(Register, Register),
    SUB(Register, Register),
    SUBI(Register, usize),
    MULT(Register, Register),
    DIV(Register, Register),
    MOD(Register, Register),
    PUSH(Register),
    RET(Register),
    RET_LEAVE(Register),
}

impl AsmIr {
    pub fn registers(&self) -> Vec<Register> {
        match self {
            Self::NOP => Vec::new(),
            Self::NEG(reg)
            | Self::RET_LEAVE(reg)
            | Self::MOVI(reg, _)
            | Self::PUSH(reg)
            | Self::SUBI(reg, _)
            | Self::RET(reg) => vec![*reg],
            Self::MOV(reg1, reg2)
            | Self::ADD(reg1, reg2)
            | Self::SUB(reg1, reg2)
            | Self::MULT(reg1, reg2)
            | Self::DIV(reg1, reg2)
            | Self::MOD(reg1, reg2) => vec![*reg1, *reg2],
        }
    }

    pub fn serialize<'a>(&self, apply_spill: bool) -> String {
        let spill = |reg: &Register| {
            if !apply_spill {
                return (*reg, vec![]);
            }
            match reg {
                Register::Machine(MachineRegister::Stack(i)) => (
                    Register::Machine(MachineRegister::R15D),
                    vec![AsmIr::MOV(
                        Register::Machine(MachineRegister::R15D),
                        Register::Machine(MachineRegister::Stack(*i)),
                    )],
                ),
                Register::Machine(_) => (*reg, vec![]),
                Register::Temp(_) => unreachable!("Why are temps here?"),
            }
        };

        let s = |asm: Vec<AsmIr>| {
            asm.into_iter()
                .map(|i| i.serialize(false))
                .collect::<Vec<String>>()
                .join("\n")
        };

        match self {
            Self::NOP => String::from("nop"),
            Self::MOV(dest, src) => {
                let (new_src, asm) = spill(src);
                format!(
                    "{}\nmov {}, {}",
                    asm.into_iter()
                        .map(|i| i.serialize(false))
                        .collect::<Vec<String>>()
                        .join("\n"),
                    dest,
                    new_src
                )
            }
            Self::MOVI(dest, i) => format!("mov {}, {}", dest, i),
            Self::NEG(dest) => format!("neg {}", dest),
            Self::ADD(dest, other) => {
                let (new_other, asm) = spill(other);

                format!("{}\nadd {}, {}", s(asm), dest, new_other)
            }
            Self::SUB(dest, other) => {
                let (new_other, asm) = spill(other);
                format!("{}\nsub {}, {}", s(asm), dest, new_other)
            }
            Self::SUBI(dest, other) => {
                format!("sub {}, {}", dest, other)
            }
            Self::MULT(dest, other) => {
                let (new_dest, asm) = spill(dest);
                // fst op (dest) has to be reg
                format!(
                    "{}\nimul {}, {}\nmov {dest}, {new_dest}",
                    s(asm),
                    new_dest,
                    other
                )
            }
            Self::DIV(dest, other) => {
                let (new_other, asm) = spill(other);
                format!(
                    "{}\nmov eax, {}\ncdq\nidiv {}\nmov {}, eax",
                    s(asm),
                    dest,
                    new_other,
                    dest
                )
            }
            Self::MOD(dest, other) => {
                let (new_other, asm) = spill(other);
                format!(
                    "{}\nmov eax, {}\ncdq\nidiv {}\nmov {}, edx",
                    s(asm),
                    dest,
                    new_other,
                    dest
                )
            }
            Self::PUSH(reg) => format!("push {reg}"),
            Self::RET(dest) => format!("mov eax, {}\nret", dest),
            Self::RET_LEAVE(dest) => format!("mov eax, {}\nleave\nret", dest),
        }
    }
}

struct AsmGen {
    // Variable <-> temp reg assignment
    registers: HashMap<String, Register>,
    next_temp: Temp,
}

impl AsmGen {
    pub fn new() -> Self {
        AsmGen {
            registers: HashMap::new(),
            next_temp: 0,
        }
    }

    pub fn generate_assembly(&mut self, ast: &AST) -> String {
        let AST::Block(stmts, _) = ast;

        let mut assembly = stmts.iter().map(|s| self.munch_stmt(s)).flatten().collect();

        // optimize assembly
        Self::fold_and_propagate(&mut assembly);

        let asm = Allocator::new(assembly)
            .allocate()
            .into_iter()
            .map(|a| a.serialize(true))
            .collect::<Vec<String>>()
            .join("\n");

        format!("{}", asm)
    }

    fn fresh_temp(&mut self) -> Register {
        let temp = self.next_temp;
        self.next_temp = self.next_temp + 1;

        Register::Temp(temp)
    }

    fn get_reg(&mut self, var: &String) -> Register {
        if !self.registers.contains_key(var) {
            let temp = self.fresh_temp();
            self.registers.insert(var.clone(), temp);
        }

        self.registers.get(var).unwrap().clone()
    }

    fn munch_expr(&mut self, expr: &Expr) -> (Register, Vec<AsmIr>) {
        match expr {
            Expr::Ident(name, _) => {
                if let Some(temp) = self.registers.get(name) {
                    (temp.clone(), vec![AsmIr::NOP])
                } else {
                    panic!("")
                }
            }
            Expr::Int(lit, _) => {
                let temp = self.fresh_temp();
                (temp.clone(), vec![AsmIr::MOVI(temp, lit.parse().unwrap())])
            }
            Expr::Unary(op, expr) => {
                let (child_temp, child_asm) = self.munch_expr(expr);
                let temp = self.fresh_temp();

                // TODO hard coded op as -
                (
                    temp.clone(),
                    [
                        child_asm,
                        vec![
                            AsmIr::MOV(temp.clone(), child_temp),
                            AsmIr::NEG(temp.clone()),
                        ],
                    ]
                    .concat(),
                )
            }
            Expr::Binary(op, expr1, expr2) => {
                let (temp1, asm1) = self.munch_expr(expr1);
                let (temp2, asm2) = self.munch_expr(expr2);
                let temp = self.fresh_temp();

                let asm = match op {
                    BinaryOp::Add => vec![AsmIr::MOV(temp, temp1), AsmIr::ADD(temp.clone(), temp2)],
                    BinaryOp::Sub => vec![AsmIr::MOV(temp, temp1), AsmIr::SUB(temp.clone(), temp2)],
                    BinaryOp::Mul => {
                        vec![AsmIr::MOV(temp, temp1), AsmIr::MULT(temp.clone(), temp2)]
                    }
                    BinaryOp::Div => vec![AsmIr::MOV(temp, temp1), AsmIr::DIV(temp, temp2)],
                    BinaryOp::Mod => vec![AsmIr::MOV(temp, temp1), AsmIr::MOD(temp.clone(), temp2)],

                    _ => panic!("Neg as binary op?"),
                };

                (temp.clone(), [asm1, asm2, asm].concat())
            }
        }
    }

    fn munch_stmt(&mut self, stmt: &Stmt) -> Vec<AsmIr> {
        match stmt {
            Stmt::Decl(ty, name, None, _) => vec![],
            Stmt::Decl(ty, name, Some(expr), _) => {
                let (temp, asm_expr) = self.munch_expr(expr);

                let var_temp = self.get_reg(name);

                [asm_expr, vec![AsmIr::MOV(var_temp, temp)]].concat()
            }
            Stmt::Assign(name, op, given_expr, span) => {
                let ident = Expr::Ident(name.clone(), span.clone());
                let expr = match op {
                    AssignOp::Eq => given_expr.clone(),
                    AssignOp::Op(op) => {
                        Expr::Binary(op.clone(), Box::new(ident), Box::new(given_expr.clone()))
                    }
                };

                let (expr_temp, expr_asm) = self.munch_expr(&expr);
                let temp = self.get_reg(name);

                [expr_asm, vec![AsmIr::MOV(temp, expr_temp)]].concat()
            }
            Stmt::Return(expr, _) => {
                let (expr_temp, expr_asm) = self.munch_expr(expr);

                [expr_asm, vec![AsmIr::RET(expr_temp)]].concat()
            }
        }
    }

    fn fold_and_propagate(asm: &mut Vec<AsmIr>) {
        let mut constants: HashMap<Register, u32> = HashMap::new();
        let mut new_asm = vec![];

        for instr in asm.iter() {
            match instr {
                AsmIr::MOVI(dst, val) => {
                    constants.insert(*dst, *val);
                    new_asm.push(instr.clone());
                }
                AsmIr::MOV(dst, src) => {
                    if let Some(val) = constants.get(src) {
                        new_asm.push(AsmIr::MOVI(*dst, *val));
                        constants.insert(*dst, *val);
                    } else {
                        constants.remove(dst);
                        new_asm.push(instr.clone());
                    }
                }
                AsmIr::ADD(dst, src) => match (constants.get(dst), constants.get(src)) {
                    (Some(&a), Some(&b)) => {
                        let result = a.wrapping_add(b);
                        constants.insert(*dst, result);
                        new_asm.push(AsmIr::MOVI(*dst, result));
                    }
                    _ => {
                        constants.remove(dst);
                        new_asm.push(instr.clone());
                    }
                },
                AsmIr::SUB(dst, src) => match (constants.get(dst), constants.get(src)) {
                    (Some(&a), Some(&b)) => {
                        let result = a.wrapping_sub(b);
                        constants.insert(*dst, result);
                        new_asm.push(AsmIr::MOVI(*dst, result));
                    }
                    _ => {
                        constants.remove(dst);
                        new_asm.push(instr.clone());
                    }
                },
                AsmIr::SUBI(dst, imm) => {
                    if let Some(&val) = constants.get(dst) {
                        let result = val.wrapping_sub(*imm as u32);
                        constants.insert(*dst, result);
                        new_asm.push(AsmIr::MOVI(*dst, result));
                    } else {
                        constants.remove(dst);
                        new_asm.push(instr.clone());
                    }
                }

                AsmIr::NEG(dst) => {
                    if let Some(&val) = constants.get(dst) {
                        let result = (!val).wrapping_add(1);
                        constants.insert(*dst, result);
                        new_asm.push(AsmIr::MOVI(*dst, result));
                    } else {
                        constants.remove(dst);
                        new_asm.push(instr.clone());
                    }
                }

                AsmIr::MULT(dst, src) => match (constants.get(dst), constants.get(src)) {
                    (Some(&a), Some(&b)) => {
                        let result = a.wrapping_mul(b);
                        constants.insert(*dst, result);
                        new_asm.push(AsmIr::MOVI(*dst, result));
                    }
                    _ => {
                        constants.remove(dst);
                        new_asm.push(instr.clone());
                    }
                },

                AsmIr::PUSH(_) => {
                    new_asm.push(instr.clone());
                }

                AsmIr::DIV(dst, src) | AsmIr::MOD(dst, src) => {
                    match (constants.get(dst), constants.get(src)) {
                        (Some(&a), Some(&b)) => {
                            // check for special cases
                            if i32::checked_div(a as i32, b as i32).is_none() {
                                constants.remove(dst);
                                new_asm.push(instr.clone());
                                break; // we will crash anyway after this instruction
                            }

                            let result = match instr {
                                AsmIr::DIV(_, _) => i32::wrapping_div(a as i32, b as i32),
                                AsmIr::MOD(_, _) => i32::wrapping_rem(a as i32, b as i32),
                                _ => unreachable!(),
                            };

                            constants.insert(*dst, result as u32);
                            new_asm.push(AsmIr::MOVI(*dst, result as u32));
                        }
                        _ => {
                            constants.remove(dst);
                            new_asm.push(instr.clone());
                        }
                    }
                }

                AsmIr::NOP => {}
                AsmIr::RET(_) | AsmIr::RET_LEAVE(_) => {
                    new_asm.push(instr.clone());
                    break;
                }
                _ => new_asm.push(instr.clone()),
            }
        }

        *asm = new_asm;
    }
}

pub fn generate_assembly(ast: &AST) -> String {
    let AST::Block(stmts, _) = ast;

    let template = include_str!("base.s");

    let asm = AsmGen::new().generate_assembly(ast);

    format!("{}\n{}", template, asm)
}

pub fn assemble(asm: Assembly, out_path: &Path) -> Result<(), io::Error> {
    let Some(binary_file_name) = out_path.file_name() else {
        panic!("Invalid file name.")
    };

    let Some(binary_path_parent) = out_path.parent() else {
        panic!("Invalid parent.")
    };

    let slug: String = Alphanumeric.sample_string(&mut rand::rng(), 8);
    let asm_file_name = format!("{}_{}.s", binary_file_name.to_str().unwrap(), slug);

    let mut asm_file =
        File::create(binary_path_parent.as_os_str().to_str().unwrap().to_owned() + &asm_file_name)
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

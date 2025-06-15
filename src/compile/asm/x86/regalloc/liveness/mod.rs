use std::collections::HashMap;

use crate::compile::asm::x86::{Instruction, Label, Register};

pub type LiveIn = Vec<Vec<Register>>; // Indexed by line number
pub type Use = Vec<Vec<Register>>; // Indexed by line number
pub type Def = Vec<Vec<Register>>; // Indexed by line number
pub type Succ = Vec<Vec<usize>>; // Indexed by line number

pub fn analyze(asm: &Vec<Instruction>, labels: &HashMap<Label, usize>) -> LiveIn {
    Analyzer::new(asm, labels).analyze()
}

struct Analyzer<'a> {
    asm: &'a Vec<Instruction>,
    labels: &'a HashMap<Label, usize>,
    live_in: LiveIn,
    uses: Use,
    defs: Def,
    succs: Succ,
}

impl<'a> Analyzer<'a> {
    pub fn new(asm: &'a Vec<Instruction>, labels: &'a HashMap<Label, usize>) -> Self {
        let mut live_in = Vec::with_capacity(asm.len());
        let mut uses = Vec::with_capacity(asm.len());
        let mut defs = Vec::with_capacity(asm.len());
        let mut succs = Vec::with_capacity(asm.len());

        // Init every line
        (0..asm.len()).for_each(|_| live_in.push(Vec::new()));
        (0..asm.len()).for_each(|_| uses.push(Vec::new()));
        (0..asm.len()).for_each(|_| defs.push(Vec::new()));
        (0..asm.len()).for_each(|_| succs.push(Vec::new()));

        Self {
            asm,
            labels,
            live_in,
            uses,
            defs,
            succs,
        }
    }

    fn define(&mut self, line: usize, reg: Register) {
        if !self.defs[line].contains(&reg) {
            self.defs[line].push(reg);
        }
    }

    fn _use(&mut self, line: usize, reg: Register) {
        if !self.uses[line].contains(&reg) {
            self.uses[line].push(reg);
        }
    }

    fn succ(&mut self, line: usize, succ_line: usize) {
        if !self.succs[line].contains(&succ_line) {
            self.succs[line].push(succ_line);
        }
    }

    fn live(&mut self, line: usize, reg: Register) {
        if !self.live_in[line].contains(&reg) {
            self.live_in[line].push(reg);
        }
    }

    pub fn analyze(mut self) -> LiveIn {
        let mut old_live_in = self.live_in.clone();
        self.analyze_one_pass();

        while old_live_in != self.live_in {
            old_live_in = self.defs.clone();
            self.analyze_one_pass();
        }

        self.live_in
    }

    fn analyze_one_pass(&mut self) {
        for l in (0..self.asm.len()).rev() {
            let used = &self.uses[l];
            let defined = &self.defs[l].clone();
            let succs = &self.succs[l].clone();

            // K1
            self.live_in[l].extend(used.clone());

            // K2
            for lp in succs.iter() {
                let live_p = self.live_in[*lp].clone();
                for x in live_p.into_iter() {
                    if !defined.contains(&x) {
                        self.live(l, x);
                    }
                }
            }
        }
    }

    fn analyze_common_one_pass(&mut self) {
        for l in (0..self.asm.len()).rev() {
            let inst = &self.asm[l];
            let has_next_line = l < self.asm.len() - 1;

            match inst {
                Instruction::ADD(dest, src)
                | Instruction::SUB(dest, src)
                | Instruction::IMUL(dest, src)
                | Instruction::AND(dest, src)
                | Instruction::OR(dest, src)
                | Instruction::XOR(dest, src)
                | Instruction::SAL(dest, src)
                | Instruction::SAR(dest, src)
                | Instruction::EQ(dest, src)
                | Instruction::NEQ(dest, src)
                | Instruction::LT(dest, src)
                | Instruction::LE(dest, src)
                | Instruction::MOV(dest, src) => {
                    if let Some(dest) = dest.get_register() {
                        self.define(l, dest);
                        self._use(l, dest);
                    }

                    if let Some(src) = src.get_register() {
                        self._use(l, src);
                    }

                    if has_next_line {
                        self.succ(l, l + 1);
                    }
                }
                Instruction::JMP(label) => {
                    self.succ(l, self.labels[&label]);
                }
                Instruction::CJMP(cond, then, otherwise) => {
                    if let Some(cond) = cond.get_register() {
                        self._use(l, cond);
                    }

                    self.succ(l, self.labels[then]);
                    self.succ(l, self.labels[otherwise]);
                }
                Instruction::NEG(dest) | Instruction::IDIV(dest) | Instruction::PUSH(dest) => {
                    if let Some(dest) = dest.get_register() {
                        self._use(l, dest);
                        self.define(l, dest);
                    }

                    if has_next_line {
                        self.succ(l, l + 1);
                    }
                }
                Instruction::RET(_)
                | Instruction::CDQ
                | Instruction::NOP
                | Instruction::LEAVE
                | Instruction::Label(_) => {
                    if has_next_line {
                        self.succ(l, l + 1);
                    }
                }
            }
        }
    }

    fn analyze_common(&mut self) {
        let mut old_uses = self.uses.clone();
        let mut old_defs = self.defs.clone();
        self.analyze_common_one_pass();

        while old_uses != self.uses || old_defs != self.defs {
            old_uses = self.uses.clone();
            old_defs = self.defs.clone();
            self.analyze_common_one_pass();
        }
    }

    // pub fn analyze(mut self) -> LiveIn {
    //     for i in (0..self.asm.len()).rev() {
    //         let inst = &self.asm[i];

    //         match inst {
    //             Instruction::NOP
    //             | Instruction::NEG(_)
    //             | Instruction::PUSH(_)
    //             | Instruction::RET(_)
    //             | Instruction::LEAVE
    //             | Instruction::CDQ => self.adopt_from_next_line(i, None),
    //             Instruction::ADD(dest, src)
    //             | Instruction::SUB(dest, src)
    //             | Instruction::IMUL(dest, src) => {
    //                 if let Some(reg) = dest.get_register() {
    //                     self.live_in[i].push(reg);
    //                 }

    //                 if let Some(reg) = src.get_register() {
    //                     self.live_in[i].push(reg);
    //                 }

    //                 self.adopt_from_next_line(i, None);
    //             }
    //             Instruction::IDIV(dest) => {
    //                 if let Some(reg) = dest.get_register() {
    //                     self.live_in[i].push(reg);
    //                 }

    //                 self.adopt_from_next_line(i, None);
    //             }
    //             Instruction::MOV(dest, src) => {
    //                 let except = if let Some(reg) = dest.get_register() {
    //                     Some(vec![reg])
    //                 } else {
    //                     None
    //                 };

    //                 self.adopt_from_next_line(i, except);
    //             }
    //             _ => {
    //                 // TODO
    //                 self.adopt_from_next_line(i, None);
    //             }
    //         }

    //         // cleanup double entries
    //         self.live_in[i].sort();
    //         self.live_in[i].dedup();
    //     }

    //     self.live_in
    // }

    fn adopt_from_next_line(&mut self, i: usize, except: Option<Vec<Register>>) {
        if i < self.asm.len() - 1 {
            let (a, b) = self.live_in.split_at_mut(i + 1);
            let next_line = &b[0];
            a[i].extend(next_line.clone());

            if let Some(except) = except {
                self.live_in[i].retain(|r| !except.contains(r));
            }
        }
    }
}

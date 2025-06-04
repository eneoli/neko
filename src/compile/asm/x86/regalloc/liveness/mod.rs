use crate::compile::asm::x86::{Asm, Instruction, Register};

pub type LiveIn = Vec<Vec<Register>>; // Indexed by line number

pub fn analyze(asm: &Vec<Instruction>) -> LiveIn {
    Analyzer::new(asm).analyze()
}

struct Analyzer<'a> {
    asm: &'a Vec<Instruction>,
    live_in: LiveIn,
}

impl<'a> Analyzer<'a> {
    pub fn new(asm: &'a Vec<Instruction>) -> Self {
        let mut live_in = Vec::with_capacity(asm.len());

        // Init every line
        (0..asm.len()).for_each(|_| live_in.push(Vec::new()));

        Self { asm, live_in }
    }

    pub fn analyze(mut self) -> LiveIn {
        for i in (0..self.asm.len()).rev() {
            let inst = &self.asm[i];

            match inst {
                Instruction::NOP
                | Instruction::NEG(_)
                | Instruction::PUSH(_)
                | Instruction::RET(_)
                | Instruction::LEAVE
                | Instruction::CDQ => self.adopt_from_next_line(i, None),
                Instruction::ADD(dest, src)
                | Instruction::SUB(dest, src)
                | Instruction::IMUL(dest, src) => {
                    if let Some(reg) = dest.get_register() {
                        self.live_in[i].push(reg);
                    }

                    if let Some(reg) = src.get_register() {
                        self.live_in[i].push(reg);
                    }

                    self.adopt_from_next_line(i, None);
                }
                Instruction::IDIV(dest) => {
                    if let Some(reg) = dest.get_register() {
                        self.live_in[i].push(reg);
                    }

                    self.adopt_from_next_line(i, None);
                }
                Instruction::MOV(dest, src) => {
                    let except = if let Some(reg) = dest.get_register() {
                        Some(vec![reg])
                    } else {
                        None
                    };

                    self.adopt_from_next_line(i, except);
                }
            }

            // cleanup double entries
            self.live_in[i].sort();
            self.live_in[i].dedup();
        }

        self.live_in
    }

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

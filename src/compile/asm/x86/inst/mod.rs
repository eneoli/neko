use std::collections::{HashMap, HashSet};

use chumsky::container::Seq;

use crate::compile::{
    asm::x86::{Instruction, Location, MachineRegister},
    ir::graph::{
        BlockId, IrGraph, NodeId,
        node::{Node, binary::BinaryNodeOp, constant::ConstantNode},
    },
};

type Temp = usize;

type Label = usize;

pub fn select(ir: &IrGraph) -> (Vec<Instruction>, HashMap<BlockId, usize>) {
    InstSelect::new(ir).munch()
}

#[derive(Clone, Debug)]
struct BlockAsm {
    pub work_asm: Vec<Instruction>,
    pub exit_asm: Vec<Instruction>,
    pub phi_asm: HashMap<NodeId, Vec<Instruction>>,
    pub phi_order: Vec<NodeId>,
}

impl BlockAsm {
    pub fn new() -> Self {
        Self {
            work_asm: vec![],
            exit_asm: vec![],
            phi_asm: HashMap::new(),
            phi_order: vec![],
        }
    }
}

struct InstSelect<'a> {
    ir: &'a IrGraph,
    next_temp: Temp,
    phi_temps: HashMap<NodeId, Temp>,
    block_asm: HashMap<BlockId, BlockAsm>,
    next_label: Label,
}

impl<'a> InstSelect<'a> {
    pub fn new(ir: &'a IrGraph) -> Self {
        let next_label = ir.blocks().iter().max().copied().unwrap_or(0) + 1;

        Self {
            ir,
            next_temp: 0,
            phi_temps: HashMap::new(),
            block_asm: HashMap::new(),
            next_label,
        }
    }

    fn fresh_temp(&mut self) -> Temp {
        let temp = self.next_temp;
        self.next_temp = self.next_temp + 1;

        temp
    }

    fn block_asm(&mut self, id: BlockId) -> &mut BlockAsm {
        if !self.block_asm.contains_key(&id) {
            self.block_asm.insert(id, BlockAsm::new());
        }

        self.block_asm.get_mut(&id).unwrap()
    }

    pub fn munch(mut self) -> (Vec<Instruction>, HashMap<BlockId, usize>) {
        let mut blocks = self.ir.blocks();
        blocks.sort();

        for block in blocks.into_iter() {
            if block == self.ir.end() {
                continue;
            }

            if self.ir.block_exit(block).is_none() {
                self.block_asm(block);
                continue;
            }

            self.munch_block(block);
        }

        self.linearlize()
    }

    fn phi_dfs(
        phi: NodeId,
        all_phis: &[NodeId],
        visited: &mut HashSet<NodeId>,
        order: &mut Vec<NodeId>,
        ir: &IrGraph,
    ) {
        if !visited.insert(phi) {
            return;
        }

        for pred in ir.predecessors(phi).clone() {
            if all_phis.contains(&pred) {
                Self::phi_dfs(pred, all_phis, visited, order, ir);
            }
        }

        order.push(phi);
    }

    fn has_as_pred(&self, id: NodeId, pred: NodeId, visited: &mut HashSet<NodeId>) -> bool {
        if !visited.insert(id) {
            return false;
        }

        for some_pred in self.ir.predecessors(id) {
            if some_pred == pred {
                return true;
            }
        }

        for some_pred in self.ir.predecessors(id) {
            if self.has_as_pred(some_pred, pred, visited) {
                return true;
            }
        }

        return false;
    }

    pub fn linearlize_block(&self, block: &BlockAsm) -> Vec<Instruction> {
        let phis: Vec<NodeId> = block.phi_asm.keys().copied().collect();

        // let mut visited = HashSet::new();
        let mut phi_order: Vec<_> = block.phi_order.iter().rev().copied().collect();

        // for &phi in phis.iter() {
        //     Self::phi_dfs(phi, &phis, &mut visited, &mut phi_order, &self.ir);
        // }

        // phi_order.reverse();

        for i in 0..phi_order.len() {
            for j in 0..phi_order.len() {
                // let node_i = &mut phi_order[i];
                // let node_j = &mut phi_order[j];
                // let mut visited = HashSet::new();
                if i > j && self.ir.predecessors(phi_order[i]).contains(&phi_order[j]) {
                    // println!("SWAP");
                    let tmp = phi_order[j];
                    phi_order[j] = phi_order[i];
                    phi_order[i] = tmp;
                }
            }
        }

        // println!("{:#?}", phi_order);

        let mut p_asm = vec![];
        for phi in phi_order.iter() {
            p_asm.extend(block.phi_asm[phi].clone());
        }

        vec![block.work_asm.clone(), p_asm, block.exit_asm.clone()].concat()
    }

    fn linearlize(self) -> (Vec<Instruction>, HashMap<BlockId, usize>) {
        let mut instructions = vec![];
        let mut blocks = HashMap::new();

        instructions.push(Instruction::JMP(self.ir.start()));
        for (label, block) in self.block_asm.iter() {
            blocks.insert(*label, instructions.len());
            instructions.push(Instruction::Label(*label));
            instructions.extend(self.linearlize_block(&block));
        }

        (instructions, blocks)
    }

    fn munch_block(&mut self, id: BlockId) -> Label {
        if self.block_asm.contains_key(&id) && self.block_asm[&id].exit_asm.len() > 0 {
            return id;
        }

        let block_exit = self.ir.block_exit(id).unwrap();
        let (work_asm, exit_asm) = self.munch_control_node(block_exit);

        self.block_asm(id).work_asm = work_asm;
        self.block_asm(id).exit_asm = exit_asm;

        id
    }

    fn munch_control_node(&mut self, id: NodeId) -> (Vec<Instruction>, Vec<Instruction>) {
        let mut effect_asm = vec![];
        if let Some(effect) = self.ir.side_effect(id) {
            effect_asm = self.munch_data_node(effect).1;
        }

        let node = self.ir.node(id).unwrap();

        let (work_asm, exit_asm) = match node {
            Node::Return => {
                let value = self.ir.lhs(id);
                let (value_temp, value_asm) = self.munch_data_node(value);

                (
                    vec![
                        value_asm,
                        vec![Instruction::MOV(
                            Location::register(MachineRegister::EAX),
                            Location::temp(value_temp),
                        )],
                    ]
                    .concat(),
                    vec![Instruction::RET(None)],
                )
                // vec![
                //     value_asm,
                //     vec![
                //         Instruction::MOV(
                //             Location::register(MachineRegister::EAX),
                //             Location::temp(value_temp),
                //         ),
                //         Instruction::RET(None),
                //     ],
                // ]
                // .concat()
            }
            Node::Jump => {
                let target = self.ir.successors(id)[0];
                let target_label = target;
                (vec![], vec![vec![Instruction::JMP(target_label)]].concat())
            }
            Node::CondJump => {
                let condition = self.ir.predecessors(id)[0];
                let (cond_temp, cond_asm) = self.munch_data_node(condition);

                let target_then = self.ir.successors(id)[0];
                let target_then_label = target_then;

                let target_otherwise = self.ir.successors(id)[1];
                let target_otherwise_label = target_otherwise;

                (
                    cond_asm,
                    vec![Instruction::CJMP(
                        Location::temp(cond_temp),
                        target_then_label,
                        target_otherwise_label,
                    )],
                )
                // vec![
                //     cond_asm,
                //     vec![Instruction::CJMP(
                //         Location::temp(cond_temp),
                //         target_then_label,
                //         target_otherwise_label,
                //     )],
                // ]
                // .concat()
            }
            _ => unreachable!("Not a control node"),
        };

        (vec![effect_asm, work_asm].concat(), exit_asm)
        // vec![effect_asm, asm].concat()
    }

    fn munch_data_node(&mut self, id: NodeId) -> (Temp, Vec<Instruction>) {
        let mut effect_asm = vec![];
        if let Some(effect) = self.ir.side_effect(id) {
            effect_asm = self.munch_data_node(effect).1;
        }

        let node = self.ir.node(id).unwrap();

        let munch_const = |x: &ConstantNode| match x {
            ConstantNode::Int(value) => Location::Immediate(*value),
            ConstantNode::Bool(true) => Location::Immediate(1),
            ConstantNode::Bool(false) => Location::Immediate(0),
        };

        let (temp, asm) = match node {
            Node::Constant(value) => {
                let temp = self.fresh_temp();
                (
                    temp.clone(),
                    vec![Instruction::MOV(Location::temp(temp), munch_const(value))],
                )
            }
            Node::Binary(op) => {
                let lhs = self.ir.lhs(id);
                let rhs = self.ir.rhs(id);

                let (lhs_temp, lhs_asm) = self.munch_data_node(lhs);
                let (rhs_temp, rhs_asm) = self.munch_data_node(rhs);

                let temp = self.fresh_temp();

                let asm = match op {
                    BinaryNodeOp::Add => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::ADD(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::Sub => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::SUB(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::Mul => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::IMUL(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::Div => vec![
                        Instruction::MOV(
                            Location::register(MachineRegister::EAX),
                            Location::temp(lhs_temp),
                        ),
                        Instruction::CDQ,
                        Instruction::IDIV(Location::temp(rhs_temp)),
                        Instruction::MOV(
                            Location::temp(temp),
                            Location::register(MachineRegister::EAX),
                        ),
                    ],
                    BinaryNodeOp::Mod => vec![
                        Instruction::MOV(
                            Location::register(MachineRegister::EAX),
                            Location::temp(lhs_temp),
                        ),
                        Instruction::CDQ,
                        Instruction::IDIV(Location::temp(rhs_temp)),
                        Instruction::MOV(
                            Location::temp(temp),
                            Location::register(MachineRegister::EDX),
                        ),
                    ],
                    BinaryNodeOp::BitwiseAnd => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::AND(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::BitwiseOr => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::OR(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::BitwiseXor => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::XOR(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::ShiftLeft => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::SAL(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::ShiftRight => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::SAR(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::Eq => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::EQ(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::NotEq => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::NEQ(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::Less => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::LT(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::LessEq => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(lhs_temp)),
                        Instruction::LE(Location::temp(temp), Location::temp(rhs_temp)),
                    ],
                    BinaryNodeOp::Greater => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(rhs_temp)),
                        Instruction::LT(Location::temp(temp), Location::temp(lhs_temp)),
                    ],
                    BinaryNodeOp::GreaterEq => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(rhs_temp)),
                        Instruction::LE(Location::temp(temp), Location::temp(lhs_temp)),
                    ],
                };

                (temp, vec![lhs_asm, rhs_asm, asm].concat())
            }
            Node::Ternary => {
                let preds = self.ir.predecessors(id);
                let cond = preds[0];
                let then = preds[1];
                let otherwise = preds[2];

                let (cond_temp, cond_asm) = self.munch_data_node(cond);
                let (then_temp, then_asm) = self.munch_data_node(then);
                let (otherwise_temp, otherwise_asm) = self.munch_data_node(otherwise);

                let temp = self.fresh_temp();
                let then_label = self.label();
                let otherwise_label = self.label();
                let next_label = self.label();

                (
                    temp,
                    vec![
                        cond_asm,
                        vec![
                            Instruction::CJMP(
                                Location::temp(cond_temp),
                                then_label,
                                otherwise_label,
                            ),
                            Instruction::Label(then_label),
                        ],
                        then_asm,
                        vec![
                            Instruction::MOV(Location::temp(temp), Location::temp(then_temp)),
                            Instruction::JMP(next_label),
                            Instruction::Label(otherwise_label),
                        ],
                        otherwise_asm,
                        vec![
                            Instruction::MOV(Location::temp(temp), Location::temp(otherwise_temp)),
                            // Instruction::JMP(next_label),
                            Instruction::Label(next_label),
                        ],
                    ]
                    .concat(),
                )
            }
            Node::Phi => {
                if self.is_phi_set(id) {
                    return (self.phi_temp(id), vec![]);
                }

                // if self.ir.predecessors(id).len() == 0 {
                //     return (self.phi_temp(id), vec![]);
                // }

                let phi_temp = self.phi_temp(id);
                let block = self.ir.block(id);
                // debug_assert_eq!(
                //     self.ir.predecessors(block).len(),
                //     self.ir.predecessors(id).len()
                // );

                for (i, pred) in self.ir.predecessors(block).iter().enumerate() {
                    if self.ir.predecessors(id).len() < i + 1 {
                        println!(
                            "[WARNING] Phi has less predecessors then the block containing it."
                        );
                        break;
                    }

                    let value = self.ir.predecessors(id)[i];
                    let (value_temp, value_asm) = self.munch_data_node(value);

                    self.block_asm(self.ir.block(*pred)).phi_asm.insert(
                        id,
                        vec![
                            value_asm,
                            vec![Instruction::MOV(
                                Location::temp(phi_temp),
                                Location::temp(value_temp),
                            )],
                        ]
                        .concat(),
                    );

                    self.block_asm(self.ir.block(*pred)).phi_order.push(id);
                }

                (self.phi_temp(id), vec![])
            }
            Node::Undef => (self.fresh_temp(), vec![]),
            _ => unreachable!("Not a data node: {id}, {:#?}", self.ir.node(id)),
        };

        // let phis = self.dependent_phis(id);
        // let mut phi_asm = vec![];
        // for phi in phis {
        //     let phi_temp = self.phi_temp(phi);
        //     phi_asm.push(Instruction::MOV(
        //         Location::temp(phi_temp),
        //         Location::temp(temp),
        //     ));
        // }

        (temp, vec![effect_asm, asm].concat())
    }

    fn phi_temp(&mut self, id: NodeId) -> Temp {
        if !self.phi_temps.contains_key(&id) {
            let temp = self.fresh_temp();
            self.phi_temps.insert(id, temp);
        }

        *self.phi_temps.get(&id).unwrap()
    }

    fn is_phi_set(&self, phi: NodeId) -> bool {
        self.phi_temps.contains_key(&phi)
    }

    fn label(&mut self) -> Label {
        let label = self.next_label;
        self.next_label = self.next_label + 1;

        label
    }

    fn dependent_phis(&self, id: NodeId) -> Vec<NodeId> {
        let succs = self.ir.successors(id);
        let mut phis = vec![];

        for succ in succs {
            let node = self.ir.node(succ).unwrap();
            println!("{:#?}", node);

            if let Node::Phi = node {
                phis.push(succ);
            }
        }

        phis
    }
}

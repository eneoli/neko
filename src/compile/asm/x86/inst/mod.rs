use crate::compile::{
    asm::x86::Instruction,
    ir::{
        node::{
            NodeId,
            node_kind::{BinaryNodeOp, NodeKind, UnaryNodeOp},
        },
        sea::Sea,
        types::Type,
    },
};

use super::{Location, MachineRegister};

pub fn select(sea: &Sea) -> Vec<Instruction> {
    InstSelect::new(sea).munch()
}

struct InstSelect<'a> {
    sea: &'a Sea,
    next_temp: u64,
}

impl<'a> InstSelect<'a> {
    pub fn new(sea: &'a Sea) -> Self {
        Self {
            sea,
            next_temp: 0,
        }
    }

    pub fn munch(&mut self) -> Vec<Instruction> {
        let Some(end_id) = self.sea.end() else {
            panic!("No End node present in IR")
        };

        self.traverse_control_node(end_id)
    }

    fn traverse_data_node(&mut self, node_id: NodeId) -> (u64, Vec<Instruction>) {
        let node = self.sea.node(node_id);

        // TODO prevent double computations: effect + input
        let mut effect_asm = vec![];
        for effect in node.effects.iter() {
            effect_asm = self.traverse_data_node(*effect).1;
        }

        let (node_temp, node_asm) = match &node.kind {
            NodeKind::Constant { ty, ctrl } => {
                let Type::Int(value) = ty else { todo!() };
                let ctrl_asm = self.traverse_control_node(*ctrl);
                let temp = self.fresh_temp();

                (
                    temp.clone(),
                    vec![
                        ctrl_asm,
                        vec![Instruction::MOV(
                            Location::temp(temp),
                            Location::Immediate(*value),
                        )],
                    ]
                    .concat(),
                )
            }
            NodeKind::Unary { op, rhs } => {
                let (rhs_temp, rhs_asm) = self.traverse_data_node(*rhs);
                let temp = self.fresh_temp();

                let asm = match op {
                    UnaryNodeOp::Neg => vec![
                        Instruction::MOV(Location::temp(temp), Location::temp(rhs_temp)),
                        Instruction::NEG(Location::temp(temp)),
                    ],
                };

                (temp.clone(), vec![rhs_asm, asm].concat())
            }
            NodeKind::Binary { op, lhs, rhs } => {
                let (lhs_temp, lhs_asm) = self.traverse_data_node(*lhs);
                let (rhs_temp, rhs_asm) = self.traverse_data_node(*rhs);
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
                };

                (temp.clone(), [lhs_asm, rhs_asm, asm].concat())
            }
            _ => panic!("{:#?} is not a data node.", node),
        };

        (node_temp, vec![effect_asm, node_asm].concat())
    }

    fn traverse_control_node(&mut self, node_id: NodeId) -> Vec<Instruction> {
        let node = self.sea.node(node_id);
        
        // TODO prevent double computations: effect + input
        let mut effect_asm = vec![];
        for effect in node.effects.iter() {
            effect_asm = self.traverse_data_node(*effect).1;
        }

        let node_asm = match node.kind {
            NodeKind::Start => vec![],
            NodeKind::Return { ctrl, value } => {
                let ctrl_asm = self.traverse_control_node(ctrl);
                let (value_temp, value_asm) = self.traverse_data_node(value);

                vec![
                    ctrl_asm,
                    value_asm,
                    vec![
                        Instruction::MOV(
                            Location::register(MachineRegister::EAX),
                            Location::temp(value_temp),
                        ),
                        Instruction::RET(None),
                    ],
                ]
                .concat()
            }
            _ => panic!("{:#?} is not a control node.", node),
        };

        vec![effect_asm, node_asm].concat()
    }

    fn fresh_temp(&mut self) -> u64 {
        let temp = self.next_temp;
        self.next_temp = self.next_temp + 1;

        temp
    }
}

// TODO this is even shittier then the asm module

use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    fmt::Display,
};

use chumsky::container::Seq;
use petgraph::{
    Graph,
    dot::{Config, Dot},
    graph::NodeIndex,
};

use super::{AsmIr, MachineRegister, Register};

#[derive(Clone, Debug)]
struct RegisterNode {
    register: Register,
    weight: usize,
    color: Option<usize>,
}

impl Display for RegisterNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.register, self.weight)
    }
}

impl RegisterNode {
    pub fn new(register: Register) -> Self {
        Self {
            register,
            weight: 0,
            color: None,
        }
    }
}

pub struct Allocator {
    asm: Vec<AsmIr>,
    live_in: Vec<Vec<Register>>,
    graph: Option<Graph<RegisterNode, usize>>,
}

impl Allocator {
    pub fn new(asm: Vec<AsmIr>) -> Self {
        let mut live_in = Vec::with_capacity(asm.len());
        for _ in 0..asm.len() {
            live_in.push(Vec::new());
        }

        Allocator {
            live_in,
            asm,
            graph: None,
        }
    }

    pub fn allocate(&mut self) -> Vec<AsmIr> {
        // println!("before regalloc");
        // 1. Liveness Analysis
        self.liveness_analysis();

        // println!("After liveness");

        // println!(
        //     "{}\n\n{:?}",
        //     self.asm
        //         .iter()
        //         .map(|a| a.serialize())
        //         .collect::<Vec<String>>()
        //         .join("\n"),
        //     self.live_in
        // );

        // 2. Build the interference graph
        self.build_interference_graph();
        // println!("after graph");

        // 3. Order the nodes using maximum cardinality search
        let ordering = self.order_nodes();
        // println!("Ordering: \n{:#?}", ordering);

        // 4. Color the graph greedily according to the elimination ordering
        self.color_nodes(&ordering);

        // println!(
        //     "{}",
        //     Dot::with_config(self.graph.as_ref().unwrap(), &[Config::EdgeNoLabel])
        // );

        // 5. Spill if more colors are needed than registers available.
        self.spill();

        // println!("Helli");
        // println!("{:#?}", self.asm);

        self.asm.clone()
    }

    fn liveness_analysis(&mut self) {
        // Backwards Analysis
        // One pass is okay for now, as we have no jumps (yet)

        // println!("{:#?}", self.asm.len());

        for i in (0..self.asm.len()).rev() {
            let inst = &self.asm[i];
            self.live_in[i] = Vec::new();

            match inst {
                AsmIr::NOP | AsmIr::NEG(_) | AsmIr::PUSH(_) | AsmIr::SUBI(_, _) => {
                    if i < self.asm.len() {
                        let (a, b) = self.live_in.split_at_mut(i + 1);
                        let next_line = &b[0];
                        a[i].extend(next_line);
                    }
                }
                AsmIr::MOVI(dest, _) => {
                    if i < self.asm.len() - 1 {
                        let (a, b) = self.live_in.split_at_mut(i + 1);
                        let next_line = &b[0];
                        a[i].extend(next_line);

                        self.live_in[i].retain(|r| r != dest);
                    }
                }
                AsmIr::ADD(dest, src)
                | AsmIr::SUB(dest, src)
                | AsmIr::MULT(dest, src)
                | AsmIr::DIV(dest, src)
                | AsmIr::MOD(dest, src) => {
                    // L1
                    self.live_in[i].push(*dest);
                    self.live_in[i].push(*src);

                    // L2
                    if i < self.asm.len() - 1 {
                        let (a, b) = self.live_in.split_at_mut(i + 1);
                        let next_line = &b[0];
                        a[i].extend(next_line);
                        // do not need to check for x =/= u
                    }
                }
                AsmIr::RET(reg) | AsmIr::RET_LEAVE(reg) => {
                    // L3
                    self.live_in[i].push(*reg);
                }
                AsmIr::MOV(dest, src) => {
                    if i < self.asm.len() - 1 {
                        self.live_in[i].push(*src);
                        let (a, b) = self.live_in.split_at_mut(i + 1);
                        let next_line = &b[0];
                        a[i].extend(next_line);

                        if !self.live_in[i + 1].contains(dest) && src != dest {
                            self.live_in[i].retain(|r| r != src);
                        }
                    }
                }
            }

            self.live_in[i].sort();
            self.live_in[i].dedup();
        }
    }

    fn build_interference_graph(&mut self) {
        self.graph = Some(Graph::new());
        let graph = self.graph.as_mut().unwrap();

        // Add nodes
        let mut registers = self
            .asm
            .iter()
            .map(AsmIr::registers)
            .flatten()
            .collect::<Vec<_>>();

        registers.sort();
        registers.dedup();

        let mut register_mapping: HashMap<Register, NodeIndex> = HashMap::new();

        for reg in registers.iter() {
            let idx = graph.add_node(RegisterNode::new(*reg));
            register_mapping.insert(*reg, idx);
        }

        // Add edges
        for (i, line) in self.live_in.iter().enumerate().rev() {
            // Temps that are live-in at the current line
            let mut edges: Vec<_> = line
                .iter()
                .enumerate()
                .flat_map(|(i, reg1)| {
                    let register_mapping = &register_mapping;
                    line.iter().skip(i + 1).map(move |reg2| {
                        (
                            *register_mapping.get(reg1).unwrap(),
                            *register_mapping.get(reg2).unwrap(),
                        )
                    })
                })
                .collect();

            // Assigned temps interfer with live-in of next line
            let assigned_temp = match self.asm[i] {
                AsmIr::ADD(reg, _)
                | AsmIr::SUB(reg, _)
                | AsmIr::SUBI(reg, _)
                | AsmIr::MULT(reg, _)
                | AsmIr::DIV(reg, _)
                | AsmIr::MOD(reg, _)
                | AsmIr::MOVI(reg, _)
                | AsmIr::MOV(reg, _)
                | AsmIr::NEG(reg) => Some(reg),
                AsmIr::NOP => None,
                AsmIr::PUSH(_) => None,
                AsmIr::RET(_) => None,
                AsmIr::RET_LEAVE(_) => None,
            };

            if let Some(reg) = assigned_temp {
                if i < self.live_in.len() - 1 {
                    for next_reg in self.live_in[i + 1].iter() {
                        if reg == *next_reg {
                            continue;
                        }

                        edges.push((
                            *register_mapping.get(&reg).unwrap(),
                            *register_mapping.get(&next_reg).unwrap(),
                        ));
                    }
                }
            }

            edges.sort();
            edges.dedup();
            edges.iter().for_each(|(a, b)| {
                graph.try_add_edge(*a, *b, 0).unwrap();
            });
        }
    }

    fn order_nodes(&mut self) -> Vec<NodeIndex> {
        let Some(ref mut graph) = self.graph else {
            panic!("No interference graph built");
        };

        let mut ordering: Vec<NodeIndex> = Vec::new();
        let mut W: Vec<NodeIndex> = graph.node_indices().collect();

        for _ in 0..graph.node_count() {
            W.sort_by(|a, b| {
                let weight_a = graph.node_weight(*a).unwrap().weight;
                let weight_b = graph.node_weight(*b).unwrap().weight;

                if weight_a < weight_b {
                    return Ordering::Less;
                }

                if weight_a > weight_b {
                    return Ordering::Greater;
                }

                Ordering::Equal
            });

            let v = W.pop().unwrap();

            // Update weights for untouched neighbors
            let neighbors: HashSet<_> = graph.neighbors(v).collect();

            let w_set = W.iter().map(|x| *x).collect::<HashSet<_>>();
            let update_candidates = w_set.intersection(&neighbors);

            for v in update_candidates {
                let node = graph.node_weight_mut(*v).unwrap();
                node.weight = node.weight + 1;
            }

            ordering.push(v);
        }

        ordering
    }

    fn color_nodes(&mut self, ordering: &Vec<NodeIndex>) {
        let Some(ref mut graph) = self.graph else {
            panic!("No interference graph built");
        };

        for v in ordering.iter() {
            let neighbor_colors: Vec<_> = graph
                .neighbors(*v)
                .map(|w| graph.node_weight(w).unwrap().color)
                .filter(|c| c.is_some())
                .map(|c| c.unwrap())
                .collect();

            let color = (0..).find(|x| !neighbor_colors.contains(x));

            graph.node_weight_mut(*v).unwrap().color = color;
        }
    }

    fn spill(&mut self) {
        let Some(ref graph) = self.graph else {
            panic!("No interference graph built");
        };

        let register_machine_mappings: HashMap<_, _> = graph
            .node_weights()
            .map(|w| (w.register, w.color.unwrap()))
            .collect();

        let mut max_reg = 0;
        let mut get_reg = |reg: &Register| match reg {
            Register::Machine(_) => *reg,
            Register::Temp(i) => {
                let max_reg = &mut max_reg;
                if i > max_reg {
                    *max_reg = *i;
                }
                Register::Machine(MachineRegister::from_index(*i))
            }
        };

        for inst in self.asm.iter_mut() {
            *inst = match inst {
                AsmIr::NOP => AsmIr::NOP,
                AsmIr::ADD(src, dst) => AsmIr::ADD(get_reg(src), get_reg(dst)),
                AsmIr::SUB(src, dst) => AsmIr::SUB(get_reg(src), get_reg(dst)),
                AsmIr::SUBI(src, i) => AsmIr::SUBI(get_reg(src), *i),
                AsmIr::MULT(src, dst) => AsmIr::MULT(get_reg(src), get_reg(dst)),
                AsmIr::DIV(src, dst) => AsmIr::DIV(get_reg(src), get_reg(dst)),
                AsmIr::MOD(src, dst) => AsmIr::MOD(get_reg(src), get_reg(dst)),
                AsmIr::MOV(src, dst) => AsmIr::MOV(get_reg(src), get_reg(dst)),
                AsmIr::MOVI(src, i) => AsmIr::MOVI(get_reg(src), *i),
                AsmIr::NEG(src) => AsmIr::NEG(get_reg(src)),
                AsmIr::RET(src) => AsmIr::RET(get_reg(src)),
                AsmIr::RET_LEAVE(src) => AsmIr::RET_LEAVE(get_reg(src)),
                AsmIr::PUSH(src) => AsmIr::PUSH(get_reg(src)),
            }
        }

        // add spill prologue
        /*
           push rbp
           mov rbp, rsp
           sub rsp, 208
        */
        let prologue = vec![
            AsmIr::PUSH(Register::Machine(MachineRegister::RBP)),
            AsmIr::MOV(
                Register::Machine(MachineRegister::RBP),
                Register::Machine(MachineRegister::RSP),
            ),
            AsmIr::SUBI(Register::Machine(MachineRegister::RSP), max_reg * 4),
        ];

        self.asm = prologue
            .into_iter()
            .chain(self.asm.clone().into_iter())
            .flat_map(|inst| match inst {
                AsmIr::RET(reg) => vec![AsmIr::RET_LEAVE(reg)],
                _ => vec![inst],
            })
            .collect();
    }
}

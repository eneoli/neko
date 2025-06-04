use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
};

use crate::compile::asm::x86::{regalloc::liveness::LiveIn, Asm, Instruction, Location, MachineRegister, Register};

mod liveness;

pub fn allocate(asm: Vec<Instruction>) -> Asm {
    Allocator::new(asm).allocate()
}

struct Node {
    register: Register,
    weight: usize,
    color: Option<usize>,
}

struct InterferenceGraph {
    nodes: Vec<Node>,
    edges: Vec<Vec<usize>>,
}

impl InterferenceGraph {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            edges: Vec::new(),
        }
    }

    pub fn add_register(&mut self, register: Register) -> usize {
        for (i, node) in self.nodes.iter().enumerate() {
            if node.register == register {
                return i;
            }
        }

        self.nodes.push(Node {
            register,
            weight: 0,
            color: None,
        });
        self.edges.push(Vec::new());

        self.nodes.len() - 1
    }

    pub fn add_edge(&mut self, edge: (usize, usize)) {
        let (a, b) = edge;

        if !self.edges[a].contains(&b) {
            self.edges[a].push(b);
        }

        if !self.edges[b].contains(&a) {
            self.edges[b].push(a);
        }
    }
}

struct Allocator {
    asm: Vec<Instruction>,
    live_in: Option<LiveIn>,
    graph: Option<InterferenceGraph>,
    stack_size: usize,
}

impl Allocator {
    pub fn new(asm: Vec<Instruction>) -> Self {
        Self {
            asm,
            live_in: None,
            graph: None,
            stack_size: 0,
        }
    }

    pub fn allocate(mut self) -> Asm {
        // 1. Liveness Analysis
        self.live_in = liveness::analyze(&self.asm).into();

        // 2. Build the interference graph
        let register_mapping = self.build_interference_graph();

        // 3. Order the nodes using maximum cardinality search
        let ordering = self.order_nodes();

        // 4. Color the graph greedily according to the elimination ordering
        self.color_nodes(&ordering);

        // 5. Spill if more colors are needed than registers available.
        self.spill(register_mapping);

        Asm::new(self.asm, self.stack_size)
    }

    fn build_interference_graph(&mut self) -> HashMap<Register, usize> {
        self.graph = Some(InterferenceGraph::new());
        let graph = self.graph.as_mut().unwrap();

        let mut registers = self
            .asm
            .iter()
            .map(Instruction::registers)
            .flatten()
            .collect::<Vec<_>>();

        registers.sort();
        registers.dedup();

        // Add Nodes
        let mut register_mapping = HashMap::new();
        for reg in registers.iter() {
            let id = graph.add_register(*reg);
            register_mapping.insert(*reg, id);
        }

        // Add Edges
        let Some(ref live_in) = self.live_in else {
            panic!("Liveness Analysis not ran")
        };

        for (i, line) in live_in.iter().enumerate() {
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

            edges.sort();
            edges.dedup();

            // Registers written to interfer with live-in of next line
            let written_regs = self.asm[i].written_registers();
            for reg in written_regs.iter() {
                if i == live_in.len() - 1 {
                    break;
                }

                for next_reg in live_in[i + 1].iter() {
                    let reg_id = register_mapping.get(reg).unwrap();
                    let next_reg_id = register_mapping.get(&next_reg).unwrap();
                    edges.push((*reg_id, *next_reg_id));
                }
            }

            for edge in edges {
                graph.add_edge(edge);
            }
        }

        register_mapping
    }

    fn order_nodes(&mut self) -> Vec<usize> {
        let Some(ref mut graph) = self.graph else {
            panic!("No interference graph built");
        };

        let mut ordering: Vec<usize> = Vec::new();
        let mut W: Vec<usize> = (0..graph.nodes.len()).collect();

        for _ in 0..graph.nodes.len() {
            W.sort_by(|a, b| {
                let weight_a = graph.nodes[*a].weight;
                let weight_b = graph.nodes[*b].weight;

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
            let neighbors: HashSet<_> = graph.edges[v].clone().into_iter().collect();

            let w_set = W.iter().map(|x| *x).collect::<HashSet<_>>();
            let update_candidates = w_set.intersection(&neighbors);

            for v in update_candidates {
                let node = &mut graph.nodes[*v];
                node.weight = node.weight + 1;
            }

            ordering.push(v);
        }

        ordering
    }

    fn color_nodes(&mut self, ordering: &Vec<usize>) {
        let Some(ref mut graph) = self.graph else {
            panic!("No interference graph built");
        };

        for v in ordering.iter() {
            let neighbor_colors: Vec<_> = graph.edges[*v]
                .iter()
                .map(|w| graph.nodes[*w].color)
                .filter(|c| c.is_some())
                .map(|c| c.unwrap())
                .collect();

            let color = (0..).find(|x| !neighbor_colors.contains(x));

            graph.nodes[*v].color = color;
        }
    }

    fn spill(&mut self, register_mapping: HashMap<Register, usize>) {
        // Replace Temps with Registers and Memory
        let mut max_reg = 0;
        let mut get_loc = |loc: &Location| match loc {
            Location::Register(reg@Register::Temp(i)) => {
                let max_reg = &mut max_reg;
                let color = register_mapping.get(reg).unwrap();

                if color > max_reg {
                    *max_reg = *color;
                }

                Location::from_index(*color as u64)
            }
            _=> *loc
        };

        for inst in self.asm.iter_mut() {
            for loc in inst.locations_mut() {
                *loc = get_loc(&loc);
            }
        }

        if max_reg > MachineRegister::num_free_regs() {
            self.stack_size = max_reg - MachineRegister::num_free_regs();
        }
    }
}

///
/// https://pp.ipd.kit.edu/firm/yComp.html
///
use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::compile::ir::graph::{
    IrGraph, NodeId,
    node::{Node, constant::ConstantNode},
};

pub struct YCompExport {
    blocks: HashMap<NodeId, Vec<NodeId>>,
}

impl YCompExport {
    pub fn new() -> Self {
        Self {
            blocks: HashMap::new(),
        }
    }

    pub fn export_to_file(&mut self, ir: &IrGraph, file: &'static str) {
        std::fs::write(file, self.export(ir));
    }

    pub fn export(&mut self, ir: &IrGraph) -> String {
        self.prepare(ir, ir.end, &mut vec![]);

        // Dump
        let mut buf = String::new();
        writeln!(buf, "graph: {{");
        VcgColor::dump_color_definitions(&mut buf);
        Self::dump_header(&mut buf);
        self.dump_blocks(ir, &mut buf);
        writeln!(buf, "}}");

        buf
    }

    fn prepare(&mut self, ir: &IrGraph, id: NodeId, seen: &mut Vec<NodeId>) {
        if seen.contains(&id) {
            return;
        }

        seen.push(id);

        let block = ir.block(id);

        if !self.blocks.contains_key(&block) {
            self.blocks.insert(block, vec![]);
        }

        if !ir.is_block(id) {
            self.blocks.get_mut(&block).unwrap().push(id);
            self.prepare(ir, block, seen);
        }

        let predecessors = &ir.predecessors(id);
        for pred in predecessors.iter() {
            self.prepare(ir, *pred, seen);
        }

        if id == ir.end {
            self.blocks.insert(id, vec![]);
        }
    }

    fn dump_header(buf: &mut String) {
        writeln!(buf, "title: \"IR Graph\"");
        writeln!(buf, "display_edge_labels: yes");
        writeln!(buf, "manhattan_edges: yes");
        writeln!(buf, "layoutalgorithm: mindepth //$ \"Compilergraph\"");
        writeln!(buf, "port_sharing: no");
        writeln!(buf, "orientation: top_to_bottom");
    }

    fn dump_blocks(&self, ir: &IrGraph, buf: &mut String) {
        for (block, nodes) in self.blocks.iter() {
            Self::dump_block(buf, ir, block, nodes);
        }
    }

    fn dump_block(buf: &mut String, ir: &IrGraph, block: &NodeId, nodes: &Vec<NodeId>) {
        let label = Self::node_name(ir, *block);

        writeln!(buf, "graph: {{");
        writeln!(buf, "  title: \"Node-{block}\"");
        writeln!(buf, "  label: \"{label}\"");
        writeln!(buf, "  status: clustered");
        writeln!(buf, "  color: {}", VcgColor::BLOCK);

        Self::dump_nodes(buf, ir, nodes);
        Self::dump_control_edges(buf, ir, *block);

        writeln!(buf, "}}");
    }

    fn dump_nodes(buf: &mut String, ir: &IrGraph, nodes: &Vec<NodeId>) {
        for node in nodes.iter() {
            Self::dump_node(buf, ir, *node);

            if !ir.is_block(*node) {
                Self::dump_input_edges(buf, ir, *node);
            }
        }
    }

    fn dump_node(buf: &mut String, ir: &IrGraph, id: NodeId) {
        let label = Self::node_name(ir, id);
        let color = Self::node_color(ir, id);

        writeln!(buf, "node: {{");
        writeln!(buf, "  title: \"Node-{id}\"");
        writeln!(buf, "  label: \"{label}\"");
        writeln!(buf, "  info1: \"Cat\"");
        writeln!(buf, "  color: {color}");
        writeln!(buf, "}}");
    }

    fn dump_input_edges(buf: &mut String, ir: &IrGraph, id: NodeId) {
        let predecessors = ir.predecessors(id);

        for (i, pred) in predecessors.into_iter().enumerate() {
            writeln!(buf, "edge: {{");
            writeln!(buf, "  sourcename: \"Node-{pred}\"");
            writeln!(buf, "  targetname: \"Node-{id}\"");
            writeln!(buf, "  label:  \"{i}\"");
            writeln!(buf, "  color: {}", VcgColor::CONTROL_FLOW);
            writeln!(buf, "}}");
            writeln!(buf, "");
        }
    }

    fn dump_control_edges(buf: &mut String, ir: &IrGraph, id: NodeId) {
        let predecessors = ir.predecessors(id);

        for (i, pred) in predecessors.into_iter().enumerate() {
            if !ir.is_block(pred) {
                // continue;
            }

            writeln!(buf, "edge: {{");
            writeln!(buf, "  sourcename: \"Node-{pred}\"");
            writeln!(buf, "  targetname: \"Node-{id}\"");
            writeln!(buf, "  label:  \"{i}\"");
            writeln!(buf, "  color: {}", VcgColor::NORMAL);
            writeln!(buf, "  priority: 50");
            writeln!(buf, "}}");
            writeln!(buf, "");
        }
    }

    fn node_name(ir: &IrGraph, id: NodeId) -> String {
        if id == ir.start {
            return "Start".to_string();
        }

        if id == ir.end {
            return "End".to_string();
        }

        if ir.is_block(id) {
            return format!("Block {id}");
        }

        let node = ir.node(id).unwrap();
        match node {
            Node::Return => format!("Return"),
            Node::Jump => format!("Jump"),
            Node::CondJump => format!("CondJump"),
            Node::Constant(ConstantNode::Int(value)) => format!("Int({value})"),
            Node::Constant(ConstantNode::Bool(value)) => format!("Bool({value})"),
            Node::Phi => format!("Phi"),
            Node::Undef => format!("Undef"),
            Node::Binary(op) => format!("{op}"),
            _ => format!("ToDo"),
        }
    }

    fn node_color(ir: &IrGraph, id: NodeId) -> VcgColor {
        let node = ir.node(id).unwrap();

        match node {
            Node::Block => VcgColor::BLOCK,
            Node::Return => VcgColor::CONTROL_FLOW,
            Node::Jump => VcgColor::CONTROL_FLOW,
            Node::CondJump => VcgColor::CONTROL_FLOW,
            Node::Constant(_) => VcgColor::CONST,
            Node::Phi => VcgColor::PHI,
            Node::Undef => VcgColor::SPECIAL,
            Node::Binary(_) => VcgColor::NORMAL,
            Node::Ternary => VcgColor::NORMAL,
        }
    }
}

struct VcgColor(u8, u8, u8);

impl VcgColor {
    const CONTROL_FLOW: VcgColor = VcgColor(255, 153, 153);
    const MEMORY: VcgColor = VcgColor(153, 153, 255);
    const NORMAL: VcgColor = VcgColor(242, 242, 242);
    const SPECIAL: VcgColor = VcgColor(255, 153, 255);
    const CONST: VcgColor = VcgColor(255, 255, 153);
    const PHI: VcgColor = VcgColor(153, 255, 153);
    const ROOT_BLOCK: VcgColor = VcgColor(204, 204, 204);
    const BLOCK: VcgColor = VcgColor(222, 239, 234);
    const SCHEDULE: VcgColor = VcgColor(255, 153, 255);

    const COLORS: [VcgColor; 9] = [
        Self::CONTROL_FLOW,
        Self::MEMORY,
        Self::NORMAL,
        Self::SPECIAL,
        Self::CONST,
        Self::PHI,
        Self::ROOT_BLOCK,
        Self::BLOCK,
        Self::SCHEDULE,
    ];

    pub fn id(&self) -> String {
        let VcgColor(r, g, b) = self;
        format!("{r}{g}{b}")
    }

    pub fn dump_color_definitions(buf: &mut String) {
        for color @ VcgColor(r, g, b) in Self::COLORS {
            let id = color.id();
            writeln!(buf, "colorentry {id}: {r} {g} {b}");
        }
    }
}

impl Display for VcgColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id())
    }
}

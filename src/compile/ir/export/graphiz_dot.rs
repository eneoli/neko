use std::fmt::Write;

use crate::compile::ir::{
    node::{
        NodeId,
        node_kind::{BinaryNodeOp, NodeKind, UnaryNodeOp},
    },
    sea::Sea,
};

pub fn export_to_file(sea: &Sea, path: String) {
    let dot = export(sea);

    std::fs::write(path, dot).unwrap();
}

pub fn export(sea: &Sea) -> String {
    let mut dot = String::from("digraph G {\n");

    // Node ranks
    for node in sea.nodes.iter() {
        if let NodeKind::Deleted = node.kind {
            continue;
        }

        let node_id = node.id;
        let rank = get_node_rank(&node.kind);

        if let Some(rank) = rank {
            writeln!(dot, "{{rank={rank}; {node_id}}}").unwrap();
        }
    }

    // Node descriptions
    for node in sea.nodes.iter() {
        if let NodeKind::Deleted = node.kind {
            continue;
        }

        let node_id = node.id;
        let node_style = get_node_style(node_id, &node.kind).to_dot();

        writeln!(&mut dot, "{node_id} {node_style}").unwrap();
    }

    writeln!(&mut dot).unwrap();

    // Edges
    for node in sea.nodes.iter() {
        if let NodeKind::Deleted = node.kind {
            continue;
        }

        let node_id = node.id;
        let inputs = node.inputs();

        for (i, input) in inputs.iter().enumerate() {
            write!(&mut dot, "{node_id} -> {input} [label=\"{i}\"]\n").unwrap();
        }
    }

    dot.push_str("\n}");

    dot
}

struct NodeStyle<'a> {
    pub label: String,
    pub shape: &'a str,
    pub style: &'a str,
    pub fillcolor: Option<&'a str>,
}

impl<'a> NodeStyle<'a> {
    pub fn to_dot(&self) -> String {
        let NodeStyle {
            label,
            shape,
            style,
            fillcolor,
        } = self;

        if let Some(fillcolor) = fillcolor {
            format!(
                "[label=\"{label}\", shape=\"{shape}\", style=\"{style}\", fillcolor=\"{fillcolor}\"]"
            )
        } else {
            format!("[label=\"{label}\", shape=\"{shape}\", style=\"{style}\"]")
        }
    }
}

fn get_node_rank(kind: &NodeKind) -> Option<&'static str> {
    match kind {
        NodeKind::Start => Some("source"),
        NodeKind::Return { .. } => Some("sink"),
        _ => None,
    }
}

fn get_node_style<'a>(id: NodeId, kind: &NodeKind) -> NodeStyle<'a> {
    match kind {
        NodeKind::Start => NodeStyle {
            label: "Start".to_string(),
            shape: "box",
            style: "filled",
            fillcolor: Some("yellow"),
        },
        NodeKind::Return { .. } => NodeStyle {
            label: "Return".to_string(),
            shape: "box",
            style: "filled",
            fillcolor: Some("yellow"),
        },
        NodeKind::Binary { op, .. } => {
            let label = match op {
                BinaryNodeOp::Add => "+",
                BinaryNodeOp::Sub => "-",
                BinaryNodeOp::Mul => "*",
                BinaryNodeOp::Div => "/",
                BinaryNodeOp::Mod => "%",
            }
            .to_string();

            NodeStyle {
                label,
                shape: "ellipse",
                style: "solid",
                fillcolor: None,
            }
        }
        NodeKind::Unary { op, .. } => {
            let label = match op {
                UnaryNodeOp::Neg => "-",
            }
            .to_string();

            NodeStyle {
                label,
                shape: "ellipse",
                style: "solid",
                fillcolor: None,
            }
        }
        NodeKind::Constant { ty, .. } => NodeStyle {
            label: ty.to_string(),
            shape: "ellipse",
            style: "solid",
            fillcolor: None,
        },
        _ => NodeStyle {
            label: format!("#{}", id.to_string()),
            shape: "ellipse",
            style: "solid",
            fillcolor: None,
        },
    }
}

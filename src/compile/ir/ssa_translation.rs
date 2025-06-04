use crate::compile::ast::{AST, AssignOp, BinaryOp, Expr, Stmt};

use super::{
    node::{NodeId, node_kind::NodeKind},
    optimize::Optimization,
    sea::Sea,
    types::Type,
};

struct SsaTranslation {
    optimization: Optimization,
    current_side_effect: Option<NodeId>,
}

impl SsaTranslation {
    pub fn new() -> Self {
        SsaTranslation {
            optimization: Optimization::new(),
            current_side_effect: None,
        }
    }

    pub fn translate(&mut self, ast: &AST) -> Sea {
        let mut sea = Sea::new();
        self.traverse_ast(ast, &mut sea);

        sea
    }

    fn traverse_ast(&mut self, ast: &AST, sea: &mut Sea) {
        match ast {
            AST::Block(stmts, _) => {
                for stmt in stmts.iter() {
                    self.traverse_stmt(stmt, sea);

                    if let Stmt::Return(_, _) = stmt {
                        break;
                    }
                }
            }
        }
    }

    fn traverse_stmt(&mut self, stmt: &Stmt, sea: &mut Sea) {
        match stmt {
            Stmt::Decl(_ty, name, expr, _) => {
                let Some(expr) = expr else {
                    return ();
                };

                let child = self.traverse_expr(expr, sea);
                sea.define(name.clone(), child).unwrap();
            }
            Stmt::Assign(name, op, expr, _) => {
                let expr_node = self.traverse_expr(expr, sea);

                let node = match op {
                    AssignOp::Eq => expr_node,
                    AssignOp::Op(op) => {
                        let effects = match op {
                            BinaryOp::Div | BinaryOp::Mod => {
                                if let Some(effect) = self.current_side_effect {
                                    vec![effect]
                                } else {
                                    vec![]
                                }
                            }
                            _ => vec![],
                        };

                        let mut id = sea.add_node(
                            NodeKind::Binary {
                                op: op.into(),
                                lhs: sea.lookup(name).unwrap(),
                                rhs: expr_node,
                            },
                            effects,
                        );

                        id = self.optimization.optimize(id, sea);

                        if op.can_have_side_effects() {
                            self.current_side_effect = Some(id);
                        }

                        id
                    }
                };

                if sea.lookup(name).is_none() {
                    sea.define(name.clone(), node).unwrap();
                } else {
                    sea.write(name.clone(), node);
                }
            }
            Stmt::Return(expr, _) => {
                let value = self.traverse_expr(expr, sea);
                let kind = NodeKind::Return {
                    ctrl: sea.start(), // TODO
                    value,
                };

                let effects = match self.current_side_effect {
                    Some(effect) => vec![effect],
                    None => vec![],
                };

                sea.add_node(kind, effects);
            }
        }
    }

    fn traverse_expr(&mut self, expr: &Expr, sea: &mut Sea) -> NodeId {
        match expr {
            Expr::Int(lit, _) => sea.add_node(
                NodeKind::Constant {
                    ctrl: sea.start(),
                    ty: Type::Int(lit.parse().unwrap()),
                },
                vec![],
            ),
            Expr::Ident(name, _) => sea
                .lookup(name)
                .expect("Reference to unknown identifier, error in semantic analysis?"),
            Expr::Unary(op, expr) => {
                let rhs = self.traverse_expr(expr, sea);
                let kind = NodeKind::Unary { op: op.into(), rhs };

                let node_id = sea.add_node(kind, vec![]);
                self.optimization.optimize(node_id, sea)
            }
            Expr::Binary(op, left_expr, right_expr) => {
                let lhs = self.traverse_expr(left_expr, sea);
                let rhs = self.traverse_expr(right_expr, sea);
                let kind = NodeKind::Binary {
                    op: op.into(),
                    lhs,
                    rhs,
                };

                let mut node_id = sea.add_node(kind, vec![]);
                node_id = self.optimization.optimize(node_id, sea);
                let node = sea.node_mut(node_id);

                if let NodeKind::Binary { ref op, .. } = node.kind {
                    if op.has_side_effect() {
                        if let Some(effect) = self.current_side_effect {
                            node.effects = vec![effect];
                        }

                        self.current_side_effect = Some(node_id);
                    }
                }

                node_id
            }
        }
    }
}

pub fn from_ast_to_sea_of_nodes(ast: &AST) -> Sea {
    SsaTranslation::new().translate(ast)
}

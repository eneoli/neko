use crate::compile::ast::{AST, Expr, Stmt};

use super::{
    node::{NodeId, node_kind::NodeKind},
    optimize::Optimization,
    sea::Sea,
    types::Type,
};

struct SsaTranslation {
    optimization: Optimization,
}

impl SsaTranslation {
    pub fn new() -> Self {
        SsaTranslation {
            optimization: Optimization::new(),
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
                sea.define(name.clone(), child);
            }
            Stmt::Assign(name, op, expr, _) => {}
            Stmt::Return(expr, _) => {
                let value = self.traverse_expr(expr, sea);
                let kind = NodeKind::Return {
                    ctrl: sea.start(), // TODO
                    value,
                };

                sea.add_node(kind);
            }
        }
    }

    fn traverse_expr(&mut self, expr: &Expr, sea: &mut Sea) -> NodeId {
        match expr {
            Expr::Int(lit, _) => sea.add_node(NodeKind::Constant {
                ctrl: sea.start(),
                ty: Type::Int(lit.parse().unwrap() as i64) ,
            }),
            Expr::Ident(name, _) => sea
                .lookup(name)
                .expect("Reference to unknown identifier, error in semantic analysis?"),
            Expr::Unary(op, expr) => {
                let rhs = self.traverse_expr(expr, sea);
                let kind = NodeKind::Unary { op: op.into(), rhs };

                let node_id = sea.add_node(kind);
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

                let node_id = sea.add_node(kind);
                self.optimization.optimize(node_id, sea)
            }
        }
    }
}

pub fn from_ast_to_sea_of_nodes(ast: &AST) -> Sea {
    SsaTranslation::new().translate(ast)
}

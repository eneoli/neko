use crate::compile::ast::{AST, Expr, Stmt};

use super::{
    node::{NodeId, NodeKind},
    sea::Sea,
    types::Type,
};

struct SsaTranslation {}

impl SsaTranslation {
    pub fn new() -> Self {
        SsaTranslation {}
    }

    pub fn translate(&mut self, ast: &AST) -> Sea {
        let mut sea = Sea::new();
        self.traverse_ast(ast, &mut sea);

        sea
    }

    fn traverse_ast(&mut self, ast: &AST, sea: &mut Sea) {
        match ast {
            AST::Block(stmts, _) => {}
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
            Stmt::Assign(name, op, expr, _) => {
                
            }
            Stmt::Return(expr, _) => {
                let child = self.traverse_expr(expr, sea);
                let kind = NodeKind::Return;

                sea.add_node(kind, vec![child]);
            }
        }
    }

    fn traverse_expr(&mut self, expr: &Expr, sea: &mut Sea) -> NodeId {
        match expr {
            Expr::Int(lit, _) => sea.add_def(NodeKind::Constant(Type::Int(lit.parse().unwrap()))),
            Expr::Ident(name, _) => sea
                .lookup(name)
                .expect("Reference to unknown identifier, error in semantic analysis?"),
            Expr::Unary(op, expr) => {
                let child = self.traverse_expr(expr, sea);
                let kind = NodeKind::Unary(op.into());

                sea.add_node(kind, vec![child])
            }
            Expr::Binary(op, left_expr, right_expr) => {
                let left_node = self.traverse_expr(left_expr, sea);
                let right_node = self.traverse_expr(right_expr, sea);
                let kind = NodeKind::Binary(op.into());

                sea.add_node(kind, vec![left_node, right_node])
            }
        }
    }
}

pub fn from_ast_to_sea_of_nodes(ast: &AST) -> Sea {
    SsaTranslation::new().translate(ast)
}

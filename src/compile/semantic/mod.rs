use std::collections::HashMap;

use thiserror::Error;

use super::ast::{AST, Expr, Stmt};

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Variable with name {0} is already declared.")]
    VariableRedeclared(String),

    #[error("Variable {0} is used without being declared.")]
    VariableUndeclared(String),

    #[error("Variable {0} is used without being initialized.")]
    VariableUninitialized(String),

    #[error("Program does not return a value.")]
    MissingReturn,

    #[error("Integer literal out of bounds.")]
    IntLiteralOutOfBounds,
}

#[derive(Debug, PartialEq, Eq)]
enum VariableState {
    Declared,
    Initialized,
}

pub struct SemanticAnalysis {
    has_return: bool,
    declared_variables: HashMap<String, VariableState>,
}

impl SemanticAnalysis {
    fn new() -> SemanticAnalysis {
        SemanticAnalysis {
            has_return: false,
            declared_variables: HashMap::new(),
        }
    }

    pub fn analyze(ast: &AST) -> Result<(), SemanticError> {
        Self::new()._analyze(ast)
    }

    pub fn _analyze(&mut self, ast: &AST) -> Result<(), SemanticError> {
        let AST::Block(stmts, _) = ast;

        for stmt in stmts.iter() {
            match stmt {
                Stmt::Decl(_, name, expr, _) => {
                    if self.declared_variables.contains_key(name) {
                        return Err(SemanticError::VariableRedeclared(name.to_string()));
                    }

                    if let Some(expr) = expr {
                        self.analyze_expr(expr)?;
                        self.declared_variables
                            .insert(name.clone(), VariableState::Initialized);
                    } else {
                        self.declared_variables
                            .insert(name.clone(), VariableState::Declared);
                    }
                }

                Stmt::Assign(name, _, expr, _) => {
                    if !self.declared_variables.contains_key(name) {
                        return Err(SemanticError::VariableUndeclared(name.to_string()));
                    }

                    self.analyze_expr(expr)?;
                    self.declared_variables
                        .insert(name.clone(), VariableState::Initialized);
                }
                Stmt::Return(expr, _) => {
                    self.analyze_expr(expr)?;
                    self.has_return = true;
                }
            }
        }

        // finally check for return
        if !self.has_return {
            return Err(SemanticError::MissingReturn);
        }

        Ok(())
    }

    fn analyze_expr(&self, expr: &Expr) -> Result<(), SemanticError> {
        // TODO: Some day in the future: proper type checking ^w^
        match expr {
            Expr::Ident(name, _) => {
                if !self.declared_variables.contains_key(name) {
                    return Err(SemanticError::VariableUndeclared(name.clone()));
                }

                if VariableState::Initialized != *self.declared_variables.get(name).unwrap() {
                    return Err(SemanticError::VariableUninitialized(name.clone()));
                }

                Ok(())
            }
            Expr::Int(expr, _) => expr.parse().map(|_| ()),
            Expr::Unary(_, expr) => self.analyze_expr(expr),
            Expr::Binary(_, expr1, expr2) => {
                self.analyze_expr(expr1)?;
                self.analyze_expr(expr2)
            }
        }
    }
}

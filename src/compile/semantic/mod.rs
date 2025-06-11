pub mod controlflow;
pub mod lowering;
pub mod typecheck;

use thiserror::Error;

use crate::compile::{
    ast::{Core, Elaborated},
    semantic::{controlflow::ControlFlowError, typecheck::TypeCheckError},
};

use super::ast::Ast;

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Type Checking failed: {0}")]
    TypeCheckingError(#[from] TypeCheckError),

    #[error("Control Flow Analysis returned an error: {0}")]
    ControlFlowError(#[from] ControlFlowError),

    #[error("Integer literal out of bounds")]
    IntLiteralOutOfBounds,
}

pub fn analyze(ast: Ast<Elaborated>) -> Result<Ast<Core>, SemanticError> {
    controlflow::analyze(&ast)?;
    let ast = typecheck::check(ast)?;
    Ok(lowering::lower(ast))
}

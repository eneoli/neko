use thiserror::Error;

use crate::compile::ast::{Ast, Elaborated, FunctionDecl, SourcePos, desugared::Stmt};

#[derive(Error, Debug, Clone)]
pub enum ControlFlowError {
    #[error("Return missing")]
    MissingReturn,

    #[error("Continue can only appear in loops")]
    InvalidContinue(SourcePos),

    #[error("Break can only appear in loops")]
    InvalidBreak(SourcePos),
}

pub fn analyze(ast: &Ast<Elaborated>) -> Result<(), ControlFlowError> {
    let Ast {
        main: FunctionDecl { body, .. },
        ..
    } = ast;

    let root_analyzer = BlockAnalyzer::new(false);

    let mut does_return = false;
    for stmt in body.iter() {
        if root_analyzer.analyze(stmt)? {
            does_return = true;
        }
    }

    if does_return {
        return Ok(());
    }

    Err(ControlFlowError::MissingReturn)
}

struct BlockAnalyzer {
    inside_loop: bool,
}

impl BlockAnalyzer {
    pub fn new(inside_loop: bool) -> Self {
        Self { inside_loop }
    }

    pub fn analyze(&self, stmt: &Stmt) -> Result<bool, ControlFlowError> {
        self.assert_loop(stmt)?;

        match stmt {
            Stmt::Block(stmts) => {
                let analyzer = BlockAnalyzer::new(self.inside_loop);
                for stmt in stmts.iter() {
                    analyzer.analyze(stmt)?;
                }
            }
            Stmt::For(_, _, _, body) | Stmt::While(_, body) => {
                BlockAnalyzer::new(true).analyze(body)?;
            }
            Stmt::If(_, then, otherwise) => {
                self.analyze(then)?;

                if let Some(otherwise) = otherwise {
                    self.analyze(otherwise)?;
                }
            }
            _ => {}
        };

        Ok(self.does_return(stmt))
    }

    fn assert_loop(&self, stmt: &Stmt) -> Result<(), ControlFlowError> {
        if !self.inside_loop {
            if let Stmt::Continue(span) = stmt {
                return Err(ControlFlowError::InvalidContinue(span.clone()));
            }

            if let Stmt::Break(span) = stmt {
                return Err(ControlFlowError::InvalidBreak(span.clone()));
            }
        }

        Ok(())
    }

    fn does_return(&self, stmt: &Stmt) -> bool {
        match stmt {
            Stmt::If(_, then, otherwise) => {
                if let Some(otherwise) = otherwise {
                    return self.does_return(&then) && self.does_return(&otherwise);
                }

                false
            }
            Stmt::Return(_, _) => true,
            Stmt::Block(stmts) => {
                for stmt in stmts.iter() {
                    if self.does_return(stmt) {
                        return true;
                    }
                }

                false
            }
            _ => false,
        }
    }
}

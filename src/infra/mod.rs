use std::process::Termination;

use chumsky::error::Rich;
use thiserror::Error;

use crate::compile::{parser::lex::Token, semantic::SemanticError};

pub struct ExitCode(u8);

impl ExitCode {
    pub const SUCCESS: ExitCode = ExitCode(0);
    pub const FAIL_PARSING: ExitCode = ExitCode(42);
    pub const FAIL_SEMANTIC: ExitCode = ExitCode(7);
}

impl Termination for ExitCode {
    fn report(self) -> std::process::ExitCode {
        std::process::ExitCode::from(self.0)
    }
}

impl From<NekoError> for ExitCode {
    fn from(value: NekoError) -> Self {
        match value {
            NekoError::LexerError(_) => ExitCode::FAIL_PARSING,
            NekoError::ParsingError(_) => ExitCode::FAIL_PARSING,
            NekoError::SemanticError(_) => ExitCode::FAIL_SEMANTIC,
            _ => ExitCode(255),
        }
    }
}

// impl FromResidual<Result<Infallible, NekoError<'_>>> for ExitCode {
//     fn from_residual(residual: Result<Infallible, NekoError<'_>>) -> Self {
//         match residual {
//             Ok(_) => ExitCode::SUCCESS,
//             Err(err) => match err {
//                 NekoError::LexerError(_) => ExitCode::FAIL_PARSING,
//                 NekoError::ParsingError(_) => ExitCode::FAIL_PARSING,
//                 NekoError::SemanticError(_) => ExitCode::FAIL_SEMANTIC,
//                 _ => ExitCode(255),
//             },
//         }
//     }
// }

#[derive(Error, Debug)]
pub enum NekoError {
    #[error("Lexical Analysis failed.")]
    LexerError(Vec<Rich<'static, char>>),

    #[error("Syntactic Analysis failed.")]
    ParsingError(Vec<Rich<'static, Token<'static>, std::ops::Range<usize>>>),

    #[error("Semantical Analysis failed: {0}")]
    SemanticError(#[from] SemanticError),

    #[error("There was an I/O error: {0}")]
    IOError(#[from] std::io::Error),

    #[error("There was an Formatting error: {0}")]
    FmtError(#[from] std::fmt::Error)
}

use std::{
    fs::{self},
    path::PathBuf,
};

use ast::AST;
use chumsky::{Parser, input::Input};
use parser::{lex::lexer, parse::program_parser};
use semantic::SemanticAnalysis;

use crate::infra::NekoError;

pub mod asm;
pub mod ast;
pub mod parser;
pub mod semantic;

// Custom macro for compiler pipeline errors

macro_rules! pipeline_error {
    ($msg:expr) => {
        panic!("Compiler Pipeline encourred an error: {}", $msg)
    };
}

#[derive(Debug, Clone, Default)]
pub struct Compiler {
    src_path: Option<PathBuf>,
    out_path: Option<PathBuf>,
    ast: Option<AST>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler::default()
    }

    pub fn src(&mut self, src: PathBuf) -> &mut Self {
        self.src_path = Some(src);

        self
    }

    pub fn out(&mut self, out: PathBuf) -> &mut Self {
        self.out_path = Some(out);

        self
    }

    pub fn compile(&mut self) -> Result<&mut Self, NekoError> {
        self.parse()?.analyze()?.assemble()
    }

    fn parse(&mut self) -> Result<&mut Self, NekoError> {
        let Some(ref src_path) = self.src_path else {
            pipeline_error!("No src path provided")
        };

        let src = match fs::read_to_string(&src_path) {
            Ok(src) => src,
            Err(err) => return Err(NekoError::IOError(err)),
        };

        let tokens = lexer().parse(src.as_str()).into_result().map_err(
            |err: Vec<chumsky::prelude::Rich<'_, char>>| {
                let lulz: Vec<chumsky::prelude::Rich<'static, char>> = err
                    .into_iter()
                    .map(chumsky::error::Rich::into_owned)
                    .collect();
                NekoError::LexerError(lulz)
            },
        )?;

        let token_stream = tokens.map(src.len()..src.len(), |(t, s)| (t, s));

        let ast = match program_parser().parse(token_stream).into_result() {
            Ok(ast) => ast,
            Err(err) => {
                let err = err
                    .into_iter()
                    .map(|x| x.map_token(|x| x.into_owned()).into_owned())
                    .collect();

                return Err(NekoError::ParsingError(err));
            }
        };

        self.ast = Some(ast);

        Ok(self)
    }

    fn analyze(&mut self) -> Result<&mut Self, NekoError> {
        let Some(ref ast) = self.ast else {
            pipeline_error!("No AST provided.")
        };

        SemanticAnalysis::analyze(ast)?;

        Ok(self)
    }

    fn assemble(&mut self) -> Result<&mut Self, NekoError> {
        let Some(ref out_path) = self.out_path else {
            pipeline_error!("No output path provided.")
        };

        let Some(ref ast) = self.ast else {
            pipeline_error!("No AST provided")
        };

        let asm = asm::generate_assembly(ast);
        asm::assemble(asm.as_str(), &out_path)?;

        Ok(self)
    }
}

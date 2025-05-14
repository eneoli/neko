use std::path::PathBuf;

use clap::Parser;

use neko::compile::Compiler;
use neko::infra::ExitCode;

#[derive(Parser, Debug)]
#[command()]
struct Args {
    src: String,
    out: String,
}

fn main() -> ExitCode {
    let args = Args::parse();

    let compilation_result = Compiler::new()
        .src(PathBuf::from(args.src.as_str()))
        .out(PathBuf::from(args.out.as_str()))
        .compile()
        .err();

    let Some(err) = compilation_result else {
        return ExitCode::SUCCESS;
    };

    err.into()
}

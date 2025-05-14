use std::{
    fs::File,
    io::{self, Write},
    path::Path,
    process::Command,
};

use rand::distr::{Alphanumeric, SampleString};

type Assembly<'a> = &'a str;

pub fn generate_assembly() -> String {
    let template = include_str!("base.s");

    // TODO actually generate assembly

    template.to_string()
}

pub fn assemble(asm: Assembly, out_path: &Path) -> Result<(), io::Error> {
    let Some(binary_file_name) = out_path.file_name() else {
        panic!("Invalid file name.")
    };

    let Some(binary_path_parent) = out_path.parent() else {
        panic!("Invalid parent.")
    };

    let slug: String = Alphanumeric.sample_string(&mut rand::rng(), 8);
    let asm_file_name = format!("{}_{}.s", binary_file_name.to_str().unwrap(), slug);

    let mut asm_file =
        File::create(binary_path_parent.as_os_str().to_str().unwrap().to_owned() + &asm_file_name)
            .unwrap();
    asm_file.write(asm.as_bytes())?;
    asm_file.flush()?;

    Command::new("gcc")
        .arg(binary_path_parent.as_os_str().to_str().unwrap().to_owned() + &asm_file_name)
        .arg("-o")
        .arg(out_path)
        .status()?;

    Ok(())
}

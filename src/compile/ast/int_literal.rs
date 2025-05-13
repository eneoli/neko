use std::num::IntErrorKind;

use crate::compile::semantic::SemanticError;

#[derive(Debug, Clone)]
pub struct IntLiteral {
    value: String,
    base: u32,
}

impl IntLiteral {
    pub fn new(value: String, base: u32) -> IntLiteral {
        IntLiteral { value, base }
    }

    pub fn parse(&self) -> Result<u32, SemanticError> {
        u32::from_str_radix(&self.value, self.base).map_err(|err| match err.kind() {
            IntErrorKind::PosOverflow => SemanticError::IntLiteralOutOfBounds,
            _ => unreachable!("Parser returned invalid number: {}", self.value),
        })
    }
}

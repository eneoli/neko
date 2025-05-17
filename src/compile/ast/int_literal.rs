use std::num::IntErrorKind;

use crate::compile::semantic::SemanticError;

#[derive(Debug, Clone)]
pub struct IntLiteral {
    value: String,
    base: u32,
}

pub const MAX_INT_DEC: u32 = 2147483648;
pub const MAX_INT_HEX: u32 = 0xffffffff;

impl IntLiteral {
    pub fn new(value: String, base: u32) -> IntLiteral {
        IntLiteral { value, base }
    }

    pub fn parse(&self) -> Result<u32, SemanticError> {
        let num = u32::from_str_radix(&self.value, self.base).map_err(|err| match err.kind() {
            IntErrorKind::PosOverflow => SemanticError::IntLiteralOutOfBounds,
            _ => unreachable!("Parser returned invalid number: {}", self.value),
        })?;

        let MAX_INT = match self.base {
            10 => MAX_INT_DEC,
            16 => MAX_INT_HEX,
            _ => panic!("Unknown base: {}", self.base),
        };

        if num > MAX_INT {
            return Err(SemanticError::IntLiteralOutOfBounds);
        }

        Ok(num)
    }
}

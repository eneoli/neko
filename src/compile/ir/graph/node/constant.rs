#[derive(Debug, Clone)]
pub enum ConstantNode {
    Int(u32),
    Bool(bool),
}

impl ConstantNode {
    pub fn is_int(&self) -> bool {
        match self {
            Self::Int(_) => true,
            _ => false,
        }
    }

    pub fn is_int_value(&self, value: u32) -> bool {
        match self {
            Self::Int(v) => *v == value,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Bool(_) => true,
            _ => false,
        }
    }

    pub fn is_bool_value(&self, value: bool) -> bool {
        match self {
            Self::Bool(v) => *v == value,
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Top,
    Bot,
    Int(u32),
}

impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Type::Top => "top".to_string(),
            Type::Bot => "bot".to_string(),
            Type::Int(c) => c.to_string(),
        }
    }
}
pub enum Type {
    Top,
    Bot,
    Int(i64),
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
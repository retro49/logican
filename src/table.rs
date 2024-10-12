#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Function,
    Statement,
    Constant,
    Variable,
    Theorem,
    Unkown,
}

impl std::default::Default for Type {
    fn default() -> Self {
        Type::Unkown
    }
}

#[derive(Default, Clone, Debug)]
pub struct SymbolTable {
    pub table: std::collections::HashMap<String, Type>
}

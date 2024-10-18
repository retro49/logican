use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Function,
    Statement,
    Constant,
    Let,
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
    pub table: std::collections::HashMap<std::rc::Rc<crate::token::Token>, Type>,
}

impl SymbolTable {
    pub fn insert(&mut self, k: std::rc::Rc<Token>, v: Type) {
        self.table.insert(k, v);
    }

}

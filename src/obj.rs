#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Theorem(String),
    Integer(i128),
    Real(f64),
    String(std::string::String),
    Boolean(bool),
    Function(crate::ast::ASTLiteral,  Vec<crate::ast::ASTParameter>, crate::ast::ASTExpression),
}

impl std::fmt::Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Theorem(t) => { f.write_fmt(format_args!("{}", t)) }
            Object::Integer(t) => { f.write_fmt(format_args!("{}", t)) }
            Object::Real(t) => { f.write_fmt(format_args!("{}", t)) }
            Object::String(t) => { f.write_fmt(format_args!("{}", t)) }
            Object::Boolean(t) => { f.write_fmt(format_args!("{}", t)) }
            Object::Function(literal, _, _) => { f.write_fmt(format_args!("<function {}>", literal.literal)) }
        }
    }
}

//! Token representation
//! Basic building blocks of logican
//! are defined in this module

/// Kinds of tokens
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Eof,
    Invalid,

    Plus,       // +
    Minus,      // -
    Slash,      // /
    Asterisk,   // *
    Assign,     // =
    Semicolon,  // ;
    Modulo,     // %
    Tilde,      // ^
    Comma,      // ,

    LessThan,           // <
    GreaterThan,        // >
    LessThanEqual,      // <=
    GreaterThanEqual,   // >=
    Equal,              // ==
    NotEqual,           // =/= 

    Implication,        // =>
    BiImplication,      // <=>
    Conjunction,        // /\
    Disjunction,        // \/
    Negation,           // !

    LeftBrace,          // {
    RightBrace,         // }
    LeftParenthesis,    // (
    RightParethesis,    // )
    LeftSquare,         // [
    RightSquare,        // ]

    Etcetera,    // ...

    True,        // true
    False,       // false
    Real,
    Integer,
    Literal,
    String,

    Theorem,
    Proof,
    Statement,
    Let,
    Constant,
    Function,
    Is,
    Print,
}

/// Token representation for logican
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
    pub line: u64,
    pub column: u64,
}

impl Token {
    pub fn from(kind: TokenKind, literal: String, line: u64, column: u64) -> Token {
        Token {
            kind,
            literal,
            line,
            column
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            kind: TokenKind::Eof,
            literal: "".to_string(),
            line: 0,
            column: 0
        }
    }
}

/**
 * GRAMMAR OF LOGICAN
 * ---------------------------------------
 *
 * SOLUTION            -> THEOREM+;
 * THEOREM             -> "theorem" "{" DECLERATION* "}"
 * DECLERATION         -> THEOREM | STMT |  EMPTY ;
 *
 * STMTS            -> | STATEMENT_STMT
 *                     | FUNCTION_STMT
 *                     | LET_STMT 
 *                     | CONSTANT_STMT
 *                     | PRINT_STMT
 *                     | EXPRESSION_STMT 
 *                     | PROOF_STMT
 *                     | NULL ;
 *
 * STATEMENT_STMT      -> "statement" ID "=" STATEMENT_DECL ';' ;
 * FUNCTION_STMT       -> "function" ID "(" PARAMS? ")" = EXPRESSION ;
 * LET_STMT            -> "let" ID "=" EXPRESSION ;
 * CONSTANT_STMT       -> "constant" ID "=" EXPRESSION ;
 * PRINT_STMT          -> "print" EXPRESSION ;
 * PROOF_STMT          -> "proof" EXPRESSION ;
 * EXPRESSION_STMT     -> EXPRESSION ";" ;
 *
 * STATEMENT_DECL      -> "..." "is" BOOLEAN
 *                     |  STRING "is" BOOLEAN
 *                     | BOOLEAN ;
 *
 * PARAMS              -> ID ("," ID)*
 * EXPRESSION          -> BI_IMPLICATION;
 * BI_IMPLICATION      -> IMPLICATION ( ("<=>") IMPLICATION)* ;
 * IMPLICATION         -> DISJUNCTION ( ("=>") DISJUNCTION)* ;
 * DISJUNCTION         -> CONJUNCTION ( ("\/") CONJUNCTION)* ;
 * CONJUNCTION         -> EQUALITY ( ("/\") EQUALITY)* ;
 * EQUALITY            -> RELATION ( ("==" | "=/=") RELATION) * ;
 * RELATION            -> MODULO ( (">" | "<" | "<=" | ">=") MODULO) * ;
 * MODULO              -> TERM ( ("%") TERM ) * ;
 * TERM                -> FACTOR ( ("+" | "-") FACTOR )* ;
 * FACTOR              -> POWER ( ("*" | "/") POWER)* ;
 * POWER               -> UNARY ( ("^") UNARY) * ;
 * UNARY               -> ("-") UNARY
 *                        | PRIMARY ;
 * CALL                -> literal ( "(" arguments? ")" )*
 * PRIMARY             -> REAL
 *                        | INTEGER
 *                        | BOOLEAN
 *                        | STRING
 *                        | ( EXPRESSION )
 *                        ;
 *
 * BOOLEAN             -> "true" | "false" ;
 */

#[derive(Debug, Clone, PartialEq)]
pub struct ASTSolution {
    pub theorems: Vec<ASTTheorem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTheorem {
    pub theorem_name: ASTLiteral,
    pub statements: Vec<ASTStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTProof {
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTStatementDecl {
    Etcetera,
    String(ASTString),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTParameter {
    pub literal: ASTLiteral,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunction {
    pub identifier: ASTLiteral,
    pub params: Vec<ASTParameter>,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTStatement {
    pub identifier: ASTLiteral,
    pub decl: ASTStatementDecl,
    pub expression: ASTExpression,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ASTLetStmt {
    pub identifier: ASTLiteral,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTConstantStmt {
    pub identifier: ASTLiteral,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTPrintStmt {
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLiteral {
    pub literal: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTInteger {
    pub value: i128,
}

#[derive(Debug, Clone, PartialEq )]
pub struct ASTReal {
    pub value: f64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBoolean {
    pub value: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTString {
    pub string: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTGroup {
    pub expression: std::boxed::Box<ASTExpression>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ASTBinary {
    pub left: std::boxed::Box<ASTExpression>,
    pub op: crate::token::TokenKind,
    pub right: std::boxed::Box<ASTExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTUnary {
    pub op: crate::token::TokenKind,
    pub right: std::boxed::Box<ASTExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTCall {
    pub literal: std::rc::Rc<ASTExpression>,
    pub expressions: Vec<ASTExpression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTExpression {
    BinaryExpreesion(ASTBinary),
    UnaryExpreesion(ASTUnary),
    Boolean(ASTBoolean),
    Real(ASTReal),
    Integer(ASTInteger),
    Literal(ASTLiteral),
    String(ASTString),
    Group(ASTGroup),
    Call(ASTCall),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTStmt {
    Theorem(ASTTheorem),
    Let(ASTLetStmt),
    Constant(ASTConstantStmt),
    Proof(ASTProof),
    Print(ASTPrintStmt),
    Statement(ASTStatement),
    Function(ASTFunction),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Stmt(ASTStmt),
    Expr(ASTExpression),
}

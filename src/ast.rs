use crate::token::Token;

/**
 * GRAMMAR OF LOGICAN
 * ---------------------------------------
 *
 * SOLUTION            -> (DECLERATION | STMT)* ;
 *
 * DECLERATION         -> STATEMENT_DECLERATION
 *                        | FUNCTION_DECLERATION
 *                        | LET_DECLERATION
 *                        | CONSTANT_DECLERATION
 *                        | THEOREM_DECLERATION
 *                        ;
 *
 * THEOREM_DECLERATION    -> "theorem" BLOCK ;
 * STATEMENT_DECLERATION  -> "statement" ID "=" STATEMENT_SPEC_EXPRESSION ";" ;
 * FUNCTION_DECLERATION   -> "function" ID "(" PARAMS? ")" = EXPRESSION ";" ;
 * LET_DECLERATION        -> "let" ID "=" EXPRESSION ";" ;
 * CONSTANT_DECLERATION   -> "constant" ID "=" EXPRESSION ";" ;
 *
 * STATEMENT_SPEC_EXPRESSION -> "..." "is" BOOLEAN
 *                              | STRING "is" BOOLEAN
 *                              | BOOLEAN ;
 *
 * STMT                -> PRINT_STMT
 *                        | PROOF_STMT
 *                        | EXPRESSION_STMT
 *                        | BLOCK
 *                        ;
 *
 * BLOCK               -> "{" (DECLERATION|STMT)* "}" 
 * PRINT_STMT          -> "print" EXPRESSION ;
 * PROOF_STMT          -> "proof" EXPRESSION ;
 * EXPRESSION_STMT     -> EXPRESSION ";" ;
 *
 * PARAMS              -> ID ("," ID)*
 *
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
    pub theorems: Vec<ASTDeclStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTDeclStmt {
    Decleration(ASTDecleration),
    Statement(ASTStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBlockStmt {
    pub statements: Vec<ASTDeclStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTheorem {
    pub token: Token,
    pub theorem_name: ASTLiteral,
    pub statements: Vec<ASTDeclStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTStatement {
    pub token: Token,
    pub identifier: ASTLiteral,
    pub decl: ASTStatementSpecExpression,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTStatementSpecExpression{
    Etcetera,
    String(ASTString),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunction {
    pub token: Token,
    pub identifier: ASTLiteral,
    pub params: Vec<ASTParameter>,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTParameter {
    pub literal: ASTLiteral,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ASTLetStmt {
    pub token: Token,
    pub identifier: ASTLiteral,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTConstantStmt {
    pub token: Token,
    pub identifier: ASTLiteral,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTPrintStmt {
    pub token: Token,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTProof {
    pub token: Token,
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
    Proof(ASTProof),
    Print(ASTPrintStmt),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTDecleration {
    Theorem(ASTTheorem),
    Let(ASTLetStmt),
    Constant(ASTConstantStmt),
    Statement(ASTStatement),
    Function(ASTFunction),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    Statement(ASTStmt),
    Decleration(ASTDecleration),
    Expression(ASTExpression),
}

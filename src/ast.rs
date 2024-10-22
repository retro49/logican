use std::rc::Rc;
use std::cell::RefCell;

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
 * STATEMENT_DECLERATION  -> "statement" ID "=" STATEMENT_SPEC_EXPRESSION ";" ;
 * FUNCTION_DECLERATION   -> "function" ID "(" PARAMS? ")" = EXPRESSION ";" ;
 * LET_DECLERATION        -> "let" ID "=" EXPRESSION ";" ;
 * CONSTANT_DECLERATION   -> "constant" ID "=" EXPRESSION ";" ;
 * THEOREM_DECLERATION    -> "theorem" BLOCK_STMT;
 *
 * STATEMENT_SPEC_EXPRESSION -> "..." "is" BOOLEAN
 *                              | STRING "is" BOOLEAN
 *                              | BOOLEAN ;
 *
 * STMT                -> PRINT_STMT
 *                        | PROOF_STMT
 *                        | EXPRESSION_STMT
 *                        | BLOCK_STMT
 *                        | NULL_STMT
 *                        ;
 *
 * BLOCK_STMT          -> "{" (DECLERATION|STMT)* "}" 
 * PRINT_STMT          -> "print" EXPRESSION ;
 * PROOF_STMT          -> "proof" EXPRESSION ;
 * EXPRESSION_STMT     -> EXPRESSION ";" ;
 * NULL_STMT           -> ";" ;
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
 *
 */

#[derive(Debug, Clone, PartialEq)]
pub struct ASTSolution {
    pub solutions: Vec<ASTDeclStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTDeclStmt {
    Decleration(ASTDecleration),
    Statement(ASTStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBlockStmt {
    pub statements: Vec<ASTDeclStmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTTheorem {
    pub token: Rc<Token>,
    pub theorem_name: ASTLiteral,
    pub block: ASTBlockStmt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTStatement {
    pub token: Rc<Token>,
    pub identifier: ASTLiteral,
    pub spec_expression: ASTStatementSpecExpression,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTStatementSpecExpression {
    Etcetera,
    String(ASTExpression),
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunction {
    pub token: Rc<Token>,
    pub identifier: ASTLiteral,
    pub params: Vec<ASTParameter>,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTParameter {
    pub literal: ASTLiteral,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ASTLet{
    pub token: Rc<Token>,
    pub identifier: ASTLiteral,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTConstant{
    pub token: Rc<Token>,
    pub identifier: ASTLiteral,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTPrint{
    pub token: Rc<Token>,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTProof {
    pub token: Rc<Token>,
    pub expression: ASTExpression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLiteral {
    pub token: Rc<Token>,
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
    pub expression: std::rc::Rc<RefCell<ASTExpression>>,
}


#[derive(Debug, Clone, PartialEq)]
pub struct ASTBinary {
    pub left: std::rc::Rc<RefCell<ASTExpression>>,
    pub op: crate::token::TokenKind,
    pub right: std::rc::Rc<RefCell<ASTExpression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTUnary {
    pub op: crate::token::TokenKind,
    pub right: std::rc::Rc<RefCell<ASTExpression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTCall {
    pub literal: std::rc::Rc<RefCell<ASTExpression>>,
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
    Print(ASTPrint),
    Block(ASTBlockStmt),
    ExpressionStmt(ASTExpression),
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTDecleration {
    Theorem(ASTTheorem),
    Let(ASTLet),
    Constant(ASTConstant),
    Statement(ASTStatement),
    Function(ASTFunction),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTNode {
    Statement(ASTStmt),
    Decleration(ASTDecleration),
    Expression(ASTExpression),
}

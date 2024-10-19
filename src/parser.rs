use std::rc::Rc;
use std::cell::RefCell;

use crate::ast::ASTBinary;
use crate::ast::ASTBlockStmt;
use crate::ast::ASTBoolean;
use crate::ast::ASTCall;
use crate::ast::ASTConstant;
use crate::ast::ASTDeclStmt;
use crate::ast::ASTDecleration;
use crate::ast::ASTExpression;
use crate::ast::ASTFunction;
use crate::ast::ASTGroup;
use crate::ast::ASTInteger;
use crate::ast::ASTLet;
use crate::ast::ASTLiteral;
use crate::ast::ASTParameter;
use crate::ast::ASTPrint;
use crate::ast::ASTProof;
use crate::ast::ASTReal;
use crate::ast::ASTSolution;
use crate::ast::ASTStatement;
use crate::ast::ASTStatementSpecExpression;
use crate::ast::ASTStmt;
use crate::ast::ASTString;
use crate::ast::ASTTheorem;
use crate::ast::ASTUnary;

use crate::table::SymbolTable;
use crate::table::Type;

#[derive(Debug)]
pub struct ParserError {
    pub msg: String,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("ParserError {{ error: {} }}", self.msg))
    }
}

impl std::error::Error for ParserError {}

pub type Anyhow<T> = Result<T, ParserError>;

#[derive(Debug)]
pub struct Parser<'a> {
    current: Rc<crate::token::Token>,
    peek: Rc<crate::token::Token>,
    lexer: &'a mut crate::lexer::Lexer<'a>,
    errors: Vec<String>,
    table: SymbolTable,
}

impl<'a> Parser<'a> {
    /// creates a new instance of the parser
    pub fn new(lxr: &'a mut crate::lexer::Lexer<'a>) -> Parser<'a> {
        let current = Rc::new(lxr.next());
        let peek = Rc::new(lxr.next());

        Parser {
            current,
            peek,
            lexer: lxr,
            errors: Vec::new(),
            table: SymbolTable::default(),
        }
    }

    fn advance(&mut self) {
        self.current = Rc::clone(&self.peek);
        self.peek = Rc::new(self.lexer.next());
    }

    pub fn parse_solution(&mut self) -> Anyhow<ASTSolution> {
        use crate::token::TokenKind as kind;
        let mut solutions = Vec::<ASTDeclStmt>::new();

        while self.current.kind != kind::Eof {
            solutions.push(self.parse_decl_stmt()?);
        }

        return Ok(ASTSolution { solutions });
    }

    fn parse_decl_stmt(&mut self) -> Anyhow<ASTDeclStmt> {
        use crate::token::TokenKind as kind;
        match self.current.kind {
            kind::Statement | kind::Function | kind::Let | kind::Constant | kind::Theorem => {
                return Ok(ASTDeclStmt::Decleration(self.parse_decl()?));
            }

            _ => {
                return Ok(ASTDeclStmt::Statement(self.parse_stmt()?));
            }
        };
    }

    fn parse_decl(&mut self) -> Anyhow<ASTDecleration> {
        use crate::token::TokenKind as kind;

        match self.current.kind {
            kind::Statement => {
                return Ok(ASTDecleration::Statement(self.parse_statement_decl()?));
            }
            kind::Function => {
                return Ok(ASTDecleration::Function(self.parse_function_decl()?));
            }
            kind::Let => {
                return Ok(ASTDecleration::Let(self.parse_let_decl()?));
            }
            kind::Constant => {
                return Ok(ASTDecleration::Constant(self.parse_constant_decl()?));
            }
            kind::Theorem => {
                return Ok(ASTDecleration::Theorem(self.parse_theorem_decl()?));
            }

            _ => {
                return Err(ParserError {
                    msg: "".to_string(),
                });
            }
        };
    }

    fn parse_statement_decl(&mut self) -> Anyhow<ASTStatement> {
        use crate::token::TokenKind as kind;

        self.look_for(kind::Statement)?;
        let token = Rc::clone(&self.current);
        self.advance();

        self.look_for(kind::Literal)?;
        let identifier = self.parse_literal()?;

        self.expect(kind::Assign)?;

        let spec_expression = self.parse_statement_spec_expression()?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;

        self.table.insert(Rc::clone(&token), Type::Statement);

        Ok(ASTStatement {
            token,
            identifier,
            spec_expression,
            expression,
        })
    }

    fn parse_statement_spec_expression(&mut self) -> Anyhow<ASTStatementSpecExpression> {
        use crate::token::TokenKind as kind;

        match self.current.kind {
            kind::Etcetera => {
                self.expect(kind::Etcetera)?;
                self.expect(kind::Is)?;
                return Ok(ASTStatementSpecExpression::Etcetera);
            }

            kind::String => {
                self.look_for(kind::String)?;
                let string = self.parse_expression()?;
                self.expect(kind::Is)?;
                return Ok(ASTStatementSpecExpression::String(string));
            }

            _ => {
                return Ok(ASTStatementSpecExpression::None);
            }
        };
    }

    fn parse_function_decl(&mut self) -> Anyhow<ASTFunction> {
        use crate::token::TokenKind as kind;

        self.look_for(kind::Function)?;
        let token = Rc::clone(&self.current);
        self.advance();
        self.look_for(kind::Literal)?;
        let identifier = self.parse_literal()?;
        self.expect(kind::LeftParenthesis)?;
        let params: Vec<ASTParameter>;
        if self.current.kind == kind::RightParethesis {
            self.advance();
            params = Vec::new();
        } else {
            params = self.parse_function_decl_params()?;
            self.expect(kind::RightParethesis)?;
        }
        self.expect(kind::Assign)?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;

        self.table.insert(Rc::clone(&token), Type::Function);

        Ok(ASTFunction {
            token,
            identifier,
            params,
            expression,
        })
    }

    fn parse_function_decl_params(&mut self) -> Anyhow<Vec<ASTParameter>> {
        use crate::token::TokenKind as kind;

        let mut params = Vec::new();
        loop {
            if self.current.kind != kind::Literal {
                let msg = std::fmt::format(format_args!(
                    "expected literal buf found: {:?}",
                    self.current.kind
                ));
                self.record_error_current(msg.clone());
                return Err(ParserError { msg });
            }

            if self.current.kind == kind::RightParethesis {
                break;
            }

            let literal = self.parse_literal()?;
            params.push(ASTParameter { literal });
            if self.current.kind == kind::RightParethesis {
                break;
            }

            self.expect(kind::Comma)?;
        }

        Ok(params)
    }

    fn parse_let_decl(&mut self) -> Anyhow<ASTLet> {
        use crate::token::TokenKind as kind;

        self.look_for(kind::Let)?;
        let token = Rc::clone(&self.current);
        self.advance();
        let identifier = self.parse_literal()?;
        self.expect(kind::Assign)?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;
        self.table.insert(Rc::clone(&token), Type::Let);

        Ok(ASTLet {
            token,
            identifier,
            expression,
        })
    }

    fn parse_constant_decl(&mut self) -> Anyhow<ASTConstant> {
        use crate::token::TokenKind as kind;

        self.look_for(kind::Constant)?;
        let token = Rc::clone(&self.current);
        self.advance();
        let identifier = self.parse_literal()?;
        self.expect(kind::Assign)?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;
        self.table.insert(Rc::clone(&token), Type::Constant);

        Ok(ASTConstant {
            token,
            identifier,
            expression,
        })
    }

    fn parse_theorem_decl(&mut self) -> Anyhow<ASTTheorem> {
        use crate::token::TokenKind as kind;

        self.look_for(kind::Theorem)?;
        let token = Rc::clone(&self.current);
        self.advance();
        self.look_for(kind::Literal)?;
        let theorem_name = self.parse_literal()?;

        let block = self.parse_block_stmt()?;

        Ok(ASTTheorem {
            token,
            theorem_name,
            block,
        })
    }

    fn parse_stmt(&mut self) -> Anyhow<ASTStmt> {
        use crate::token::TokenKind as kind;

        match self.current.kind {
            kind::Print => {
                return Ok(ASTStmt::Print(self.parse_print_stmt()?));
            }
            kind::Proof => {
                return Ok(ASTStmt::Proof(self.parse_proof_stmt()?));
            }

            kind::LeftBrace => {
                return Ok(ASTStmt::Block(self.parse_block_stmt()?));
            }

            kind::Semicolon => {
                return self.parse_null_stmt();
            }
            _ => {
                let expression = self.parse_expression()?;
                self.expect(kind::Semicolon)?;
                return Ok(ASTStmt::ExpressionStmt(expression));
            }
        }
    }

    fn parse_null_stmt(&mut self) -> Anyhow<ASTStmt> {
        use crate::token::TokenKind as kind;
        self.expect(kind::Semicolon)?;
        Ok(ASTStmt::Null)
    }

    fn parse_print_stmt(&mut self) -> Anyhow<ASTPrint> {
        use crate::token::TokenKind as kind;

        self.look_for(kind::Print)?;
        let token = Rc::clone(&self.current);
        self.advance();
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;

        Ok(ASTPrint { token, expression })
    }

    fn parse_proof_stmt(&mut self) -> Anyhow<ASTProof> {
        use crate::token::TokenKind as kind;

        self.look_for(kind::Proof)?;
        let token = Rc::clone(&self.current);
        self.advance();

        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;

        Ok(ASTProof { token, expression })
    }

    fn parse_block_stmt(&mut self) -> Anyhow<ASTBlockStmt> {
        use crate::token::TokenKind as kind;
        self.expect(kind::LeftBrace)?;
        let mut statements = Vec::new();
        while self.current.kind != kind::RightBrace {
            statements.push(self.parse_decl_stmt()?);
        }
        self.expect(kind::RightBrace)?;
        Ok(ASTBlockStmt { statements })
    }

    fn parse_expression(&mut self) -> Anyhow<ASTExpression> {
        return self.parse_exp_bi_implication();
    }

    fn parse_exp_bi_implication(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;
        let mut left = self.parse_exp_implication()?;

        while self.current.kind == kind::BiImplication {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_implication()?;
            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                right: std::rc::Rc::new(RefCell::new(right)),
                op,
            });
        }

        return Ok(left);
    }

    fn parse_exp_implication(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;
        let mut left = self.parse_exp_disjunction()?;

        while self.current.kind == kind::Implication {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_disjunction()?;
            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }

        return Ok(left);
    }

    fn parse_exp_disjunction(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;
        let mut left = self.parse_exp_conjunction()?;

        while self.current.kind == kind::Disjunction {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_conjunction()?;
            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }

        return Ok(left);
    }

    fn parse_exp_conjunction(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;
        let mut left = self.parse_exp_equality()?;
        while self.current.kind == kind::Conjunction {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_equality()?;
            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }

        return Ok(left);
    }

    fn parse_exp_equality(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;

        let mut left = self.parse_exp_relation()?;
        while self.current.kind == kind::Equal || self.current.kind == kind::NotEqual {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_relation()?;

            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }

        return Ok(left);
    }

    fn parse_exp_relation(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;

        let mut left = self.parse_exp_modulo()?;
        while self.current.kind == kind::LessThan
            || self.current.kind == kind::GreaterThan
            || self.current.kind == kind::LessThanEqual
            || self.current.kind == kind::GreaterThanEqual
        {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_modulo()?;

            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }

        return Ok(left);
    }

    fn parse_exp_modulo(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;

        let mut left = self.parse_exp_term()?;
        while self.current.kind == kind::Modulo {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_term()?;

            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }
        return Ok(left);
    }

    fn parse_exp_term(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;

        let mut left = self.parse_exp_factor()?;
        while self.current.kind == kind::Plus || self.current.kind == kind::Minus {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_factor()?;

            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }

        return Ok(left);
    }

    fn parse_exp_factor(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;

        let mut left = self.parse_exp_power()?;
        while self.current.kind == kind::Asterisk || self.current.kind == kind::Slash {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_power()?;
            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }

        return Ok(left);
    }

    fn parse_exp_power(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;

        let mut left = self.parse_exp_unary()?;
        while self.current.kind == kind::Tilde {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_unary()?;
            left = ASTExpression::BinaryExpreesion(ASTBinary {
                left: std::rc::Rc::new(RefCell::new(left)),
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            });
        }
        return Ok(left);
    }

    fn parse_exp_unary(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;

        if self.current.kind == kind::Minus || self.current.kind == kind::Negation {
            let op = self.current.kind.clone();
            self.advance();
            let right = self.parse_exp_unary()?;
            return Ok(ASTExpression::UnaryExpreesion(ASTUnary {
                op,
                right: std::rc::Rc::new(RefCell::new(right)),
            }));
        } else {
            return self.parse_call();
        }
    }

    fn parse_call(&mut self) -> Anyhow<ASTExpression> {
        let literal_expression = self.parse_exp_primary()?;
        match literal_expression {
            crate::ast::ASTExpression::Literal(_) => {
                if self.current.kind == crate::token::TokenKind::LeftParenthesis {
                    self.advance();
                    // parse expressions
                    let mut expressions = Vec::new();
                    while self.current.kind != crate::token::TokenKind::RightParethesis {
                        let exps = self.parse_expression()?;
                        expressions.push(exps);
                        if self.current.kind == crate::token::TokenKind::RightParethesis {
                            break;
                        }
                        self.expect(crate::token::TokenKind::Comma)?;
                    }
                    self.expect(crate::token::TokenKind::RightParethesis)?;
                    return Ok(crate::ast::ASTExpression::Call(ASTCall {
                        literal: std::rc::Rc::new(RefCell::new(literal_expression)),
                        expressions,
                    }));
                } else {
                    return Ok(literal_expression);
                }
            }
            _ => {
                return Ok(literal_expression);
            }
        };
    }

    pub fn parse_literal(&mut self) -> Anyhow<ASTLiteral> {
        use crate::token::TokenKind as kind;

        self.look_for(kind::Literal)?;
        let literal = self.current.literal.clone();
        self.advance();
        return Ok(ASTLiteral { literal });
    }

    fn parse_exp_primary(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;
        match self.current.kind {
            kind::LeftParenthesis => {
                self.advance();
                let res = Ok(self.parse_expression()?)?;
                self.expect(kind::RightParethesis)?;
                return Ok(ASTExpression::Group(ASTGroup {
                    expression: std::rc::Rc::new(RefCell::new(res)),
                }));
            }

            kind::Real => {
                let res = Ok(ASTExpression::Real(ASTReal {
                    value: self.current.literal.clone().parse::<f64>().unwrap(),
                }));
                self.advance();
                return res;
            }

            kind::Integer => {
                let res = Ok(ASTExpression::Integer(ASTInteger {
                    value: self.current.literal.clone().parse::<i128>().unwrap(),
                }));
                self.advance();
                return res;
            }

            kind::True => {
                let res = Ok(ASTExpression::Boolean(ASTBoolean { value: true }));
                self.advance();
                return res;
            }

            kind::False => {
                let res = Ok(ASTExpression::Boolean(ASTBoolean { value: false }));
                self.advance();
                return res;
            }

            kind::String => {
                let res = Ok(ASTExpression::String(ASTString {
                    string: self.current.literal.clone(),
                }));
                self.advance();
                return res;
            }

            kind::Literal => {
                return Ok(ASTExpression::Literal(self.parse_literal()?));
            }

            _ => {
                // TODO: enhance error message
                return Err(ParserError {
                    msg: "unkown primary".to_string(),
                });
            }
        };
    }

    fn expect(&mut self, kind: crate::token::TokenKind) -> Anyhow<bool> {
        if self.current.kind != kind {
            let msg = std::fmt::format(format_args!(
                "expected {:?} found {:?}",
                kind, self.current.kind
            ));
            self.record_error_current(std::fmt::format(format_args!(
                "expected {:?} found {:?}",
                kind, self.current.kind
            )));
            return Err(ParserError { msg });
        }

        self.advance();
        Ok(true)
    }

    fn look_for(&mut self, kind: crate::token::TokenKind) -> Anyhow<bool> {
        if self.current.kind != kind {
            let msg = std::fmt::format(format_args!(
                "expected {:?} found {:?}",
                kind, self.current.kind
            ));

            self.record_error_current(std::fmt::format(format_args!(
                "expected {:?} found {:?}",
                kind, self.current.kind
            )));
            return Err(ParserError { msg });
        }

        Ok(true)
    }

    fn record_error_current(&mut self, msg: String) {
        self.errors.push(std::fmt::format(format_args!(
            "line: {}, column: {} error: {}",
            self.current.line, self.current.column, msg
        )));
    }
}

#[cfg(test)]
mod parser_test {
    #[test]
    fn proof_test() {
        let input = r#"
            theorem genesis {
                constant first_count = 0 + 0 * 0 / 0 ;

                let total_population = 0;
                let total_death = first_count;

                statement adam_is_first_man = 'the first person' is true ;
                statement what_eve_heared_first = 'madamimadam' is true ;
                statement and_everythig_else = ... is true ;

                proof adam_is_first_man /\ eve_is_the_first_woman;

                function add_one(x) = x + 1 ;
                function add_both(x, y) = x + y ;
                function hyp(x, y, z) = (x^2) + (y^2);
            }
        "#;

        let mut lxr = crate::lexer::Lexer::new(input.as_bytes());
        let mut prsr = super::Parser::new(&mut lxr);
        let sol = prsr.parse_solution();
        assert_eq!(0, prsr.errors.len());

        match sol {
            Ok(ref r) => {
                for theorem in &r.solutions {
                    println!("{:#?}", theorem);
                }
            }

            Err(_) => {
                for err in &prsr.errors {
                    println!(">>> error: {}", err);
                }
                assert!(false);
            }
        }

    }
}

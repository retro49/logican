use crate::ast::{
    ASTBinary, ASTBoolean, ASTCall, ASTConstantStmt, ASTExpression, ASTFunction, ASTGroup,
    ASTInteger, ASTLetStmt, ASTLiteral, ASTParameter, ASTPrintStmt, ASTProof, ASTReal, ASTSolution,
    ASTStatement, ASTStatementDecl, ASTStmt, ASTString, ASTTheorem, ASTUnary,
};

use crate::table::{SymbolTable, Type};

#[derive(Debug)]
pub struct ParserError {
    pub msg: String,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("ParserError {{ msg: {} }}", self.msg))
    }
}

impl std::error::Error for ParserError {}

type Anyhow<T> = Result<T, ParserError>;

pub struct Parser<'a> {
    current: crate::token::Token,
    peek: crate::token::Token,
    lexer: &'a mut crate::lexer::Lexer<'a>,
    errors: Vec<String>,
    table: SymbolTable,
}

impl<'a> Parser<'a> {
    pub fn new(lxr: &'a mut crate::lexer::Lexer<'a>) -> Parser<'a> {
        let current = lxr.next();
        let peek = lxr.next();

        Parser {
            current,
            peek,
            lexer: lxr,
            errors: Vec::new(),
            table: SymbolTable::default(),
        }
    }

    pub fn advance(&mut self) {
        self.current = std::mem::take(&mut self.peek);
        self.peek = self.lexer.next();
    }

    pub fn parse_solution(&mut self) -> Anyhow<ASTSolution> {
        let mut sol = Vec::new();
        while self.current.kind != crate::token::TokenKind::Eof {
            match self.parse_thorem() {
                Ok(r) => {
                    sol.push(r);
                }

                Err(e) => {
                    return Err(ParserError { msg: e.to_string() });
                }
            };
        }

        Ok(ASTSolution { theorems: sol })
    }

    pub fn parse_thorem(&mut self) -> Anyhow<ASTTheorem> {
        use crate::token::TokenKind as kind;

        let mut statements = Vec::new();

        self.expect(kind::Theorem)?;
        let theorem_name = self.parse_literal()?;
        self.expect(kind::LeftBrace)?;

        while self.current.kind != kind::RightBrace {
            statements.push(self.parse_stmt()?);
        }

        self.expect(kind::RightBrace)?;
        self.table
            .table
            .insert(theorem_name.literal.clone(), Type::Theorem);
        Ok(ASTTheorem {
            theorem_name,
            statements,
        })
    }

    pub fn parse_literal(&mut self) -> Anyhow<ASTLiteral> {
        use crate::token::TokenKind as kind;

        let literal = self.current.literal.clone();
        self.expect(kind::Literal)?;
        return Ok(ASTLiteral { literal });
    }

    pub fn parse_stmt(&mut self) -> Anyhow<ASTStmt> {
        use crate::token::TokenKind as kind;

        while self.current.kind != kind::Semicolon {
            match self.current.kind {
                kind::Theorem => {
                    return Ok(ASTStmt::Theorem(self.parse_thorem()?));
                }

                kind::Let => {
                    return Ok(ASTStmt::Let(self.parse_let_stmt()?));
                }
                kind::Constant => {
                    return Ok(ASTStmt::Constant(self.parse_constant_stmt()?));
                }

                kind::Proof => {
                    return Ok(ASTStmt::Proof(self.parse_proof_stmt()?));
                }

                kind::Statement => {
                    return Ok(ASTStmt::Statement(self.parse_statement_stmt()?));
                }

                kind::Function => {
                    return Ok(ASTStmt::Function(self.parse_function_stmt()?));
                }

                kind::Print => {
                    return Ok(ASTStmt::Print(self.parse_print_stmt()?));
                }

                _ => {
                    self.record_error_current(std::fmt::format(format_args!(
                        "expected any statement but found {:?}",
                        self.current
                    )));
                    return Err(ParserError {
                        msg: "no statement type found".to_string(),
                    });
                }
            };
        }

        Ok(ASTStmt::Null)
    }

    fn parse_let_stmt(&mut self) -> Anyhow<ASTLetStmt> {
        use crate::token::TokenKind as kind;
        self.expect(kind::Let)?;
        let identifier = self.parse_literal()?;
        self.expect(kind::Assign)?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;
        self.table
            .table
            .insert(identifier.literal.clone(), Type::Variable);
        Ok(ASTLetStmt {
            identifier,
            expression,
        })
    }

    fn parse_constant_stmt(&mut self) -> Anyhow<ASTConstantStmt> {
        use crate::token::TokenKind as kind;
        self.expect(kind::Constant)?;
        let identifier = self.parse_literal()?;
        self.expect(kind::Assign)?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;
        self.table
            .table
            .insert(identifier.literal.clone(), Type::Constant);
        Ok(ASTConstantStmt {
            identifier,
            expression,
        })
    }

    fn parse_print_stmt(&mut self) -> Anyhow<ASTPrintStmt> {
        use crate::token::TokenKind as kind;
        self.expect(kind::Print)?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;
        Ok(ASTPrintStmt { expression })
    }

    fn parse_statement_stmt(&mut self) -> Anyhow<ASTStatement> {
        use crate::token::TokenKind as kind;

        self.expect(kind::Statement)?;
        let identifier = ASTLiteral {
            literal: self.current.literal.clone(),
        };
        self.expect(kind::Literal)?;
        self.expect(kind::Assign)?;
        let decl = self.parse_statement_stmt_decl()?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;
        self.table
            .table
            .insert(identifier.literal.clone(), Type::Statement);

        Ok(ASTStatement {
            identifier,
            decl,
            expression,
        })
    }

    fn parse_statement_stmt_decl(&mut self) -> Anyhow<ASTStatementDecl> {
        use crate::token::TokenKind as kind;
        match self.current.kind {
            kind::Etcetera => {
                self.expect(kind::Etcetera)?;
                self.expect(kind::Is)?;
                return Ok(ASTStatementDecl::Etcetera);
            }
            kind::String => {
                let string = ASTString {
                    string: self.current.literal.clone(),
                };
                self.expect(kind::String)?;
                self.expect(kind::Is)?;
                return Ok(ASTStatementDecl::String(string));
            }
            _ => {
                return Ok(ASTStatementDecl::None);
            }
        };
    }

    fn parse_proof_stmt(&mut self) -> Anyhow<ASTProof> {
        use crate::token::TokenKind as kind;
        self.expect(kind::Proof)?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;
        return Ok(ASTProof { expression });
    }

    fn parse_function_stmt_params(&mut self) -> Anyhow<Vec<ASTParameter>> {
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

    fn parse_function_stmt(&mut self) -> Anyhow<ASTFunction> {
        use crate::token::TokenKind as kind;

        self.expect(kind::Function)?;
        let identifier = self.parse_literal()?;
        self.expect(kind::LeftParenthesis)?;
        let params: Vec<ASTParameter>;

        if self.current.kind == kind::RightParethesis {
            self.advance();
            params = Vec::new();
        } else {
            params = self.parse_function_stmt_params()?;
            self.expect(kind::RightParethesis)?;
        }

        self.expect(kind::Assign)?;
        let expression = self.parse_expression()?;
        self.expect(kind::Semicolon)?;
        self.table
            .table
            .insert(identifier.literal.clone(), Type::Function);
        Ok(ASTFunction {
            identifier,
            params,
            expression,
        })
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                left: std::boxed::Box::new(left),
                right: std::boxed::Box::new(right),
                op,
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
                right: std::boxed::Box::new(right),
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
                        literal: std::rc::Rc::new(literal_expression),
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

    fn parse_exp_primary(&mut self) -> Anyhow<ASTExpression> {
        use crate::token::TokenKind as kind;
        match self.current.kind {
            kind::LeftParenthesis => {
                self.advance();
                let res = Ok(self.parse_expression()?)?;
                self.expect(kind::RightParethesis)?;
                return Ok(ASTExpression::Group(ASTGroup {
                    expression: std::boxed::Box::new(res),
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
        let input = "
            theorem genesis {
                constant first_count = 0 + 0 * 0 / 0 ;

                let total_population = 0;
                let total_death = first_count;

                statement adam_is_first_man = 'the first person' is true ;
                statement what_eve_heared_first = 'madamimadam' is true ;
                statement and_everythig_else = ... is true ; 

                proof adam_is_first_man /\\ eve_is_the_first_woman;

                function add_one(x) = x + 1 ;
                function add_both(x, y) = x + y ;
                function hyp(x, y, z) = (x^2) + (y^2);
            }
        ";

        let mut lxr = crate::lexer::Lexer::new(input.as_bytes());
        let mut prsr = super::Parser::new(&mut lxr);
        let sol = prsr.parse_solution();

        match sol {
            Ok(ref r) => {
                for theorem in &r.theorems {
                    println!("{:#?}", theorem);
                }
            }

            Err(_) => {
                for err in &prsr.errors {
                    println!("error: {}", err);
                }
            }
        }

        for (k, v) in &prsr.table.table {
            println!("{}, {:?}", k, v);
        }
    }
}

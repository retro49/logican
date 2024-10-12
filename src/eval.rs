use crate::env::Environment;

#[derive(Debug, Clone)]
pub struct EvaluationError {
    pub msg: String,
}

pub type EvaluationResult<T> = Result<T, EvaluationError>;

pub trait Evaluate<T> {
    fn evaluate(&mut self, env: &mut Environment) -> EvaluationResult<T>
    where
        T: Sized;
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTReal {
    fn evaluate(&mut self, _env: &mut Environment) -> EvaluationResult<crate::obj::Object> {
        return Ok(crate::obj::Object::Real(self.value));
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTInteger {
    fn evaluate(&mut self, _env: &mut Environment) -> EvaluationResult<crate::obj::Object> {
        return Ok(crate::obj::Object::Integer(self.value));
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTBoolean {
    fn evaluate(&mut self, _env: &mut Environment) -> EvaluationResult<crate::obj::Object> {
        return Ok(crate::obj::Object::Boolean(self.value));
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTString {
    fn evaluate(&mut self, _env: &mut Environment) -> EvaluationResult<crate::obj::Object> {
        return Ok(crate::obj::Object::String(self.string.clone()));
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTUnary {
    fn evaluate(&mut self, env: &mut Environment) -> EvaluationResult<crate::obj::Object> {
        let right = self.right.evaluate(env)?;
        match right {
            crate::obj::Object::Real(r) => {
                if self.op == crate::token::TokenKind::Minus {
                    return Ok(crate::obj::Object::Real(r * -1f64));
                } else {
                    unreachable!()
                }
            }

            crate::obj::Object::Integer(i) => {
                if self.op == crate::token::TokenKind::Minus {
                    return Ok(crate::obj::Object::Integer(i * -1));
                } else {
                    unreachable!()
                }
            }

            crate::obj::Object::Boolean(b) => {
                if self.op == crate::token::TokenKind::Negation {
                    return Ok(crate::obj::Object::Boolean(!b));
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        };
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTBinary {
    fn evaluate(&mut self, env: &mut Environment) -> EvaluationResult<crate::obj::Object> {
        let left = self.left.evaluate(env)?;
        let right = self.right.evaluate(env)?;
        match (left, right) {
            (crate::obj::Object::Integer(i), crate::obj::Object::Integer(j)) => {
                match self.op {
                    crate::token::TokenKind::Plus => {
                        return Ok(crate::obj::Object::Integer(i + j));
                    }

                    crate::token::TokenKind::Minus => {
                        return Ok(crate::obj::Object::Integer(i - j));
                    }

                    crate::token::TokenKind::Slash => {
                        return Ok(crate::obj::Object::Integer(i / j));
                    }

                    crate::token::TokenKind::Asterisk => {
                        return Ok(crate::obj::Object::Integer(i * j));
                    }

                    crate::token::TokenKind::Modulo => {
                        return Ok(crate::obj::Object::Integer(i % j));
                    }

                    crate::token::TokenKind::Tilde => {
                        return Ok(crate::obj::Object::Integer(i.pow(j as u32)));
                    }

                    crate::token::TokenKind::LessThan => {
                        return Ok(crate::obj::Object::Boolean(i < j));
                    }

                    crate::token::TokenKind::LessThanEqual => {
                        return Ok(crate::obj::Object::Boolean(i <= j));
                    }

                    crate::token::TokenKind::GreaterThan => {
                        return Ok(crate::obj::Object::Boolean(i > j));
                    }

                    crate::token::TokenKind::GreaterThanEqual => {
                        return Ok(crate::obj::Object::Boolean(i >= j));
                    }

                    crate::token::TokenKind::Equal => {
                        return Ok(crate::obj::Object::Boolean(i == j));
                    }

                    crate::token::TokenKind::NotEqual => {
                        return Ok(crate::obj::Object::Boolean(i != j));
                    }

                    _ => {
                        return Err(EvaluationError {
                            msg: "Runtime error: invalid operation".to_string(),
                        });
                    }
                };
            }

            (crate::obj::Object::Real(i), crate::obj::Object::Integer(j)) => {
                match self.op {
                    crate::token::TokenKind::Plus => {
                        return Ok(crate::obj::Object::Real(i + j as f64));
                    }

                    crate::token::TokenKind::Minus => {
                        return Ok(crate::obj::Object::Real(i - j as f64));
                    }

                    crate::token::TokenKind::Slash => {
                        return Ok(crate::obj::Object::Real(i / j as f64));
                    }

                    crate::token::TokenKind::Asterisk => {
                        return Ok(crate::obj::Object::Real(i * j as f64));
                    }

                    crate::token::TokenKind::LessThan => {
                        return Ok(crate::obj::Object::Boolean(i < j as f64));
                    }

                    crate::token::TokenKind::LessThanEqual => {
                        return Ok(crate::obj::Object::Boolean(i <= j as f64));
                    }

                    crate::token::TokenKind::GreaterThan => {
                        return Ok(crate::obj::Object::Boolean(i > j as f64));
                    }

                    crate::token::TokenKind::GreaterThanEqual => {
                        return Ok(crate::obj::Object::Boolean(i >= j as f64));
                    }

                    crate::token::TokenKind::Equal => {
                        return Ok(crate::obj::Object::Boolean(i == j as f64));
                    }

                    crate::token::TokenKind::NotEqual => {
                        return Ok(crate::obj::Object::Boolean(i != j as f64));
                    }

                    _ => {
                        unreachable!()
                    }
                };
            }

            (crate::obj::Object::Integer(i), crate::obj::Object::Real(j)) => {
                match self.op {
                    crate::token::TokenKind::Plus => {
                        return Ok(crate::obj::Object::Real(i as f64 + j));
                    }

                    crate::token::TokenKind::Minus => {
                        return Ok(crate::obj::Object::Real(i as f64 - j ));
                    }

                    crate::token::TokenKind::Slash => {
                        return Ok(crate::obj::Object::Real(i as f64 / j ));
                    }

                    crate::token::TokenKind::Asterisk => {
                        return Ok(crate::obj::Object::Real(i as f64 * j ));
                    }

                    crate::token::TokenKind::LessThan => {
                        return Ok(crate::obj::Object::Boolean((i as f64) < j));
                    }

                    crate::token::TokenKind::LessThanEqual => {
                        return Ok(crate::obj::Object::Boolean((i as f64) <= j ));
                    }

                    crate::token::TokenKind::GreaterThan => {
                        return Ok(crate::obj::Object::Boolean((i as f64) > j ));
                    }

                    crate::token::TokenKind::GreaterThanEqual => {
                        return Ok(crate::obj::Object::Boolean((i as f64) >= j));
                    }

                    crate::token::TokenKind::Equal => {
                        return Ok(crate::obj::Object::Boolean((i as f64) == j));
                    }

                    crate::token::TokenKind::NotEqual => {
                        return Ok(crate::obj::Object::Boolean((i as f64) != j));
                    }

                    _ => {
                        unreachable!()
                    }
                };
            }

            (crate::obj::Object::Real(i), crate::obj::Object::Real(j)) => {
                match self.op {
                    crate::token::TokenKind::Plus => {
                        return Ok(crate::obj::Object::Real(i + j));
                    }

                    crate::token::TokenKind::Minus => {
                        return Ok(crate::obj::Object::Real(i - j ));
                    }

                    crate::token::TokenKind::Slash => {
                        return Ok(crate::obj::Object::Real(i / j ));
                    }

                    crate::token::TokenKind::Asterisk => {
                        return Ok(crate::obj::Object::Real(i * j ));
                    }

                    crate::token::TokenKind::LessThan => {
                        return Ok(crate::obj::Object::Boolean(i < j));
                    }

                    crate::token::TokenKind::LessThanEqual => {
                        return Ok(crate::obj::Object::Boolean(i <= j));
                    }

                    crate::token::TokenKind::GreaterThan => {
                        return Ok(crate::obj::Object::Boolean(i > j));
                    }

                    crate::token::TokenKind::GreaterThanEqual => {
                        return Ok(crate::obj::Object::Boolean(i >= j));
                    }

                    crate::token::TokenKind::Equal => {
                        return Ok(crate::obj::Object::Boolean(i == j));
                    }

                    crate::token::TokenKind::NotEqual => {
                        return Ok(crate::obj::Object::Boolean(i != j));
                    }

                    _ => {
                        unreachable!()
                    }
                };
            }

            (crate::obj::Object::Boolean(l), crate::obj::Object::Boolean(r)) => {
                match self.op {
                    crate::token::TokenKind::Equal => {
                        return Ok(crate::obj::Object::Boolean(l == r)); 
                    }

                    crate::token::TokenKind::NotEqual => {
                        return Ok(crate::obj::Object::Boolean(l != r));
                    }

                    crate::token::TokenKind::Conjunction => {
                        return Ok(crate::obj::Object::Boolean(l && r));
                    }

                    crate::token::TokenKind::Disjunction => {
                        return Ok(crate::obj::Object::Boolean(l || r));
                    }

                    crate::token::TokenKind::Implication => {
                        if l == true && r == false {
                            return Ok(crate::obj::Object::Boolean(false));
                        } else {
                            return Ok(crate::obj::Object::Boolean(true));
                        }
                    }

                    crate::token::TokenKind::BiImplication => {
                        if (l == true && r == true) || (l == false && r == false) {
                            return Ok(crate::obj::Object::Boolean(true));
                        } else {
                            return Ok(crate::obj::Object::Boolean(false));
                        }

                    }

                    _ => { unreachable!()}
                };
            }

            (_, _) => {
                unreachable!()
            }
        }
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTExpression {
    fn evaluate(&mut self, env: &mut Environment) -> EvaluationResult<crate::obj::Object> {
        match self {
            crate::ast::ASTExpression::BinaryExpreesion(bin) => { return bin.evaluate(env); }
            crate::ast::ASTExpression::UnaryExpreesion(u) => { return u.evaluate(env); }
            crate::ast::ASTExpression::Boolean(b) => { return b.evaluate(env); }
            crate::ast::ASTExpression::Real(r) => { return r.evaluate(env); }
            crate::ast::ASTExpression::Integer(i) => { return i.evaluate(env); }
            crate::ast::ASTExpression::String(s) => { return s.evaluate(env); }
            crate::ast::ASTExpression::Group(g) => { return g.expression.evaluate(env); }

            crate::ast::ASTExpression::Literal(l) => {
                if env.env.contains_key(&l.literal) {
                    return Ok(env.env.get(&l.literal).unwrap().clone());
                } else {
                    panic!("unkown literal, {}", l.literal);
                }
            }
        };
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTStmt {
    fn evaluate(&mut self, env: &mut Environment) -> EvaluationResult<crate::obj::Object> {
        match self {
            crate::ast::ASTStmt::Statement(s) => { 
                let exp = s.expression.evaluate(env)?;
                match exp {
                    crate::obj::Object::Boolean(b) => { return Ok(crate::obj::Object::Boolean(b)); }
                    _ => {
                        unreachable!()
                    }
                };
            }
            _ => { unreachable!() }
        };
    }
}

#[derive(Debug, Default)]
pub struct Evaluator {
    pub env: crate::env::Environment,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator::default()
    }

    pub fn eval(&mut self, solution: &mut crate::ast::ASTSolution) {
        for theorem in &mut solution.theorems {
            self.eval_theorem(theorem);
        }
    }

    fn eval_theorem(&mut self, theorem: &mut crate::ast::ASTTheorem) {
        if self.env.env.contains_key(&theorem.theorem_name.literal) {
            println!("theorem {} already exist", &theorem.theorem_name.literal);
            panic!()
        }

        self.env.env.insert(theorem.theorem_name.literal.clone(), crate::obj::Object::Theorem(theorem.theorem_name.literal.clone()));
        let stmts = &mut theorem.statements;
        for stmt in stmts {
            self.eval_stmt(stmt);
        }
    }

    fn eval_stmt(&mut self, stmt: &mut crate::ast::ASTStmt) {
        match stmt {
            crate::ast::ASTStmt::Null => { return; }

            crate::ast::ASTStmt::Let(l) => {
                let exp = &l.expression.evaluate(&mut self.env).unwrap();
                self.env.env.insert(l.identifier.literal.clone(), exp.clone());
            }

            crate::ast::ASTStmt::Constant(c) => {
                let exp = &c.expression.evaluate(&mut self.env).unwrap();
                if self.env.env.contains_key(&c.identifier.literal) {
                    println!("constant {} already defined", &c.identifier.literal);
                    panic!();
                } else {
                    self.env.env.insert(c.identifier.literal.clone(), exp.clone());
                }
            }

            crate::ast::ASTStmt::Proof(p) => {
                let exp = p.expression.evaluate(&mut self.env).unwrap();

                match exp {
                    crate::obj::Object::Boolean(b)=> {
                        if !b {
                            println!("proof failed {:?}", p);
                        }
                    }

                    _ => {
                        panic!("cannot proof non boolean expression {:?}", p);
                    }
                };
            }

            crate::ast::ASTStmt::Print(e) => {
                let exp = e.expression.evaluate(&mut self.env).unwrap();
                println!("{}", exp);
            }

            crate::ast::ASTStmt::Statement(s) => {
                let exp = s.expression.evaluate(&mut self.env).unwrap();
                match exp {
                    crate::obj::Object::Boolean(b)=> {
                        if self.env.env.contains_key(&s.identifier.literal) {
                            println!("statement {} declared before", &s.identifier.literal);
                            panic!();
                        }

                        match &s.decl {
                            crate::ast::ASTStatementDecl::None | crate::ast::ASTStatementDecl::Etcetera => {
                                self.env.env.insert(s.identifier.literal.clone(), crate::obj::Object::Boolean(b));
                            }
                            crate::ast::ASTStatementDecl::String(_sd) => {
                                self.env.env.insert(s.identifier.literal.clone(), crate::obj::Object::Boolean(b));
                            }
                        };
                    }

                    _ => {
                        panic!("cannot declare a non boolean statement {:?}", s);
                    }

                };
            }

            crate::ast::ASTStmt::Theorem(t) => {
                self.eval_theorem(t);
            }

            _ => { 
                println!("hey there, I have no idea what I am doing!");
            }

        };
    }
}

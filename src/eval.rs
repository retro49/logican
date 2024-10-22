use crate::ast::ASTExpression;
use crate::error::Anyhow;
use crate::env::Environment;
use crate::obj::Object;

#[derive(Debug, Clone)]
pub struct EvaluationError {
    pub msg: String,
}
impl std::fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("EvaluationError {{ error: {} }}", self.msg))
    }
}

impl std::error::Error for EvaluationError{}

pub trait Evaluate<T> {
    fn evaluate(&mut self, env: *mut Environment) -> Anyhow<T>
    where
        T: Sized;
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTReal {
    fn evaluate(&mut self, _env: *mut Environment) -> Anyhow<crate::obj::Object> {
        return Ok(crate::obj::Object::Real(self.value));
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTInteger {
    fn evaluate(&mut self, _env: *mut Environment) -> Anyhow<crate::obj::Object> {
        return Ok(crate::obj::Object::Integer(self.value));
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTBoolean {
    fn evaluate(&mut self, _env: *mut Environment) -> Anyhow<crate::obj::Object> {
        return Ok(crate::obj::Object::Boolean(self.value));
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTString {
    fn evaluate(&mut self, _env: *mut Environment) -> Anyhow<crate::obj::Object> {
        return Ok(crate::obj::Object::String(self.string.clone()));
    }
}

impl Evaluate<crate::obj::Object> for crate::ast::ASTUnary {
    fn evaluate(&mut self, env: *mut Environment) -> Anyhow<crate::obj::Object> {
        let mut rr = self.right.borrow_mut();
        let right = rr.evaluate(env)?;
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
    fn evaluate(&mut self, env: *mut Environment) -> Anyhow<crate::obj::Object> {
        let mut ll = self.left.borrow_mut();
        let mut rr = self.right.borrow_mut();

        let left = ll.evaluate(env)?;
        let right = rr.evaluate(env)?;

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
                        return Err(Box::new(EvaluationError{ 
                            msg: "Runtime error: invalid operation".to_string(),
                        }));
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
    fn evaluate(&mut self, env: *mut Environment) -> Anyhow<crate::obj::Object> {
        match self {
            crate::ast::ASTExpression::BinaryExpreesion(bin) => { return bin.evaluate(env); }
            crate::ast::ASTExpression::UnaryExpreesion(u) => { return u.evaluate(env); }
            crate::ast::ASTExpression::Boolean(b) => { return b.evaluate(env); }
            crate::ast::ASTExpression::Real(r) => { return r.evaluate(env); }
            crate::ast::ASTExpression::Integer(i) => { return i.evaluate(env); }
            crate::ast::ASTExpression::String(s) => { return s.evaluate(env); }
            crate::ast::ASTExpression::Group(g) => { 
                let mut r = g.expression.borrow_mut();
                return r.evaluate(env);
            }

            crate::ast::ASTExpression::Literal(l) => {
                unsafe {
                    let res = env.as_mut().unwrap().get(&l.literal);
                    match res {
                        Some(r) => { return Ok(r.clone()); }
                        None => {
                            panic!("unkown literal, {}", l.literal);
                        }
                    };
                }
            }

            crate::ast::ASTExpression::Call(ref call) => {
                unsafe {
                    let lit = call.literal.as_ref();
                    let blit = lit.borrow_mut();

                    match *blit {
                        ASTExpression::Literal(ref l) => {
                            let mut function = env.as_mut().unwrap().get(&l.literal);

                            match &mut function {
                                Some(mut obj) => {
                                    match &mut obj {
                                        Object::Function(lit, params, exp) => {
                                            let mut new_env = Environment::from(env);

                                            if params.len() != call.expressions.len() {
                                                let msg = std::fmt::format(format_args!("on line {} column {} function {} requires {} args but found {}", 
                                                        l.token.line,
                                                        l.token.column,
                                                        lit.literal, 
                                                        params.len(), 
                                                        call.expressions.len()));

                                                return Err(Box::new(EvaluationError{msg}));
                                            }

                                            let len = call.expressions.len();
                                            for i in 0..len {
                                                let exp_pos = &call.expressions[i]
                                                    .clone()
                                                    .evaluate(env)
                                                    .unwrap();

                                                let par_name = (&params[i]).literal.literal.clone();
                                                new_env.set(par_name, exp_pos.clone());
                                            }

                                            return exp.clone().evaluate(&mut new_env);
                                        }

                                        _ => {
                                            panic!("expected function call expression but not found: {}", &l.literal);
                                        }
                                    };
                                }

                                None => {
                                    let msg = std::fmt::format(format_args!("function {} not found", l.literal));
                                    panic!("{}", msg);
                                }
                            };
                        }

                        _ => {  
                            let msg = std::fmt::format(format_args!("expected literal but found non literal for call: {:?}", *blit));
                            return Err(Box::new(EvaluationError{msg}));
                        }
                    };
                }
            }
        };
    }
}

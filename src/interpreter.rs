use crate::ast::ASTBlockStmt;
use crate::ast::ASTPrint;
use crate::ast::ASTProof;
use crate::ast::ASTStatement;
use crate::error::Anyhow;

use crate::eval::Evaluate;

use crate::obj::Object;
use crate::parser::Parser;

use crate::ast::ASTConstant;
use crate::ast::ASTDeclStmt;
use crate::ast::ASTDecleration;
use crate::ast::ASTLet;
use crate::ast::ASTStmt;

use crate::env::Environment;

#[derive(Debug)]
pub struct InterpreterError {
    pub msg: String,
}

impl std::fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("InterpreterError {{ error: {} }}", self.msg))
    }
}

impl std::error::Error for InterpreterError {}

#[derive(Debug)]
pub struct Interpreter<'a> {
    pub parser: &'a mut Parser<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn interpret(parser: &'a mut Parser<'a>) -> Anyhow<()> {
        let solution = parser.parse_solution();
        let mut env = Environment::default();

        match solution {
            Ok(solution) => {
                let mut result = solution.solutions;
                for res in &mut result {
                    Interpreter::interpret_solution(res, &mut env)?;
                }
                return Ok(());
            }

            Err(e) => {
                return Err(Box::new(InterpreterError { msg: e.msg }));
            }
        }
    }

    fn interpret_solution(s: &mut ASTDeclStmt, env: *mut Environment) -> Anyhow<()> {
        match s {
            ASTDeclStmt::Decleration(decl) => {
                return Interpreter::interpret_decl(decl, env);
            }

            ASTDeclStmt::Statement(stmt) => {
                return Interpreter::interpret_stmt(stmt, env);
            }
        }
    }

    fn interpret_decl(decl: &mut ASTDecleration, env: *mut Environment) -> Anyhow<()> {
        match decl {
            ASTDecleration::Let(l) => {
                return Interpreter::interpret_let_decl(l, env);
            }

            ASTDecleration::Constant(c) => {
                return Interpreter::interpret_const_decl(c, env);
            }
            ASTDecleration::Statement(s) => {
                return Interpreter::interpert_statement_stmt(s, env);
            }
            _ => {}
        };

        return Ok(());
    }

    fn interpret_let_decl(l: &mut ASTLet, env: *mut Environment) -> Anyhow<()> {
        unsafe {
            let obj = l.expression.evaluate(env)?;
            env.as_mut().unwrap().set(l.identifier.literal.clone(), obj);
            return Ok(());
        }
    }

    fn interpret_const_decl(c: &mut ASTConstant, env: *mut Environment) -> Anyhow<()> {
        unsafe {
            if env.as_ref().unwrap().contains(&c.identifier.literal) {
                panic!("constant \"{}\" on line: {} column: {}, is already defined", &c.identifier.literal, c.token.line, c.token.column);
            }

            let value = c.expression.evaluate(env)?;
            env.as_mut().unwrap().set(c.identifier.literal.clone(), value);
            return Ok(());

        }
    }

    fn interpert_statement_stmt(s: &mut ASTStatement, env: *mut Environment) -> Anyhow<()> {
        let obj = s.expression.evaluate(env)?;
        match obj {
            Object::Boolean(_) => {
                unsafe {
                    env.as_mut().unwrap().set(s.identifier.literal.clone(), obj);
                    return Ok(());
                }
            }
            _ => {
                let msg = std::fmt::format(
                    format_args!("line: {} column: {} on statement {} expected boolean expression but found non boolean expression", 
                        s.token.line, 
                        s.token.column, 
                        s.identifier.literal
                    )
                );

                return Err(Box::new(InterpreterError { msg }));
            }
        };
    }

    fn interpret_stmt(stmt: &mut ASTStmt, env: *mut Environment) -> Anyhow<()> {
        match stmt {
            ASTStmt::Null => {return Ok(()); }
            ASTStmt::ExpressionStmt(s) => { 
                // TODO:
                // could set an option to this
                let exp = s.evaluate(env)?;
                println!("{}", exp);
                return Ok(());
            }

            ASTStmt::Print(p) => { return Interpreter::interpret_print_stmt(p, env); }
            ASTStmt::Proof(p) => { return Interpreter::interpret_proof_stmt(p, env); }
            ASTStmt::Block(b) => { return Interpreter::interpret_block_stmt(b, env); }
        };
    }

    fn interpret_print_stmt(p: &mut ASTPrint, env: *mut Environment) -> Anyhow<()> {
        let expression = p.expression.evaluate(env)?;
        println!("{}", expression);
        return Ok(());
    }

    fn interpret_proof_stmt(p: &mut ASTProof, env: *mut Environment) -> Anyhow<()> {
        let obj = p.expression.evaluate(env)?;
        match obj {
            Object::Boolean(b) => {
                if b == true { 
                    return Ok(());
                }

                let msg = std::fmt::format(
                    format_args!("line: {} column: {} failed proof", 
                        p.token.line, 
                        p.token.column, 
                    )
                );
                return Err(Box::new(InterpreterError { msg }));
            }

            _ => {
                let msg = std::fmt::format(
                    format_args!("line: {} column: {} on proof expected boolean expression but found non boolean expression", 
                        p.token.line, 
                        p.token.column, 
                    )
                );

                return Err(Box::new(InterpreterError { msg }));
            }
        };
    }

    fn interpret_block_stmt(b: &mut ASTBlockStmt, env: *mut Environment) -> Anyhow<()> {
        let mut new_env = Environment::from(env);
        for v in &mut b.statements {
            Interpreter::interpret_solution(v, &mut new_env)?;
        }

        Ok(())
    }
}

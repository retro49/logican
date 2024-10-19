use crate::parser::Parser;
use crate::ast::ASTDeclStmt;
use crate::ast::ASTDecleration;
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

impl std::error::Error for InterpreterError{}

pub type Anyhow<T> = Result<T, InterpreterError>;
#[derive(Debug)]
pub struct Interpreter<'a>{
    pub globals: Environment<'a>,
    pub parser: Parser<'a>
}

impl<'a> Interpreter<'a> {
    pub fn new(parser: Parser<'a>) -> Interpreter<'a> {
        let globals = Environment::default();

        Interpreter{
            globals,
            parser
        }
    }

    pub fn interpret(&mut self) -> Anyhow<()> {
        let solution = self.parser.parse_solution();

        match solution  {
            Ok(solution) => {
                let result = solution.solutions;
                for res in &result {
                    self.interpret_solution(res)?;
                }

                return Ok(());
            }

            Err(e) => {
                return Err(InterpreterError{msg: e.msg});
            }
        }
    }

    fn interpret_solution(&mut self, s: &ASTDeclStmt) -> Anyhow<()> {
        match s {
            ASTDeclStmt::Decleration(decl) => { return self.interpret_decl(decl); }
            ASTDeclStmt::Statement(stmt) => { return self.interpret_stmt(stmt); }
        }
    }

    fn interpret_decl(&mut self, _decl: &ASTDecleration) -> Anyhow<()> {
        Ok(())
    }

    fn interpret_stmt(&mut self, _stmt: &ASTStmt) -> Anyhow<()> {
        Ok(())
    }
}

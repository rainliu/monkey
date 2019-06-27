use crate::token::*;

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
}
/*
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{};",
            match self {
                Statement::Let(stmt) => format!("{}", stmt),
                Statement::Return(ret) => format!("{}", ret),
                //Statement::Expression(exp) => format!("{};", exp),
            }
        )
    }
}*/

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Ident(Identifier),
}
/*
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expression::Ident(ident) => format!("{}", ident),
            }
        )
    }
}*/

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
/*
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let statements: Vec<String> = (&self.statements)
            .iter()
            .map(|stmt| stmt.to_string())
            .collect();
        write!(f, "{}", statements.join("\n"))
    }
}*/

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier(pub String);

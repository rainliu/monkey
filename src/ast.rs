use crate::token::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(Identifier, Expression),
    Return(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Ident(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub token: Token,
}

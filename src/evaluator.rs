use crate::ast::*;
use std::fmt;

#[cfg(test)]
mod evaluator_test;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Null,
    Int(i64),
    Boolean(bool),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Object::Null => "null".to_string(),
            Object::Int(int) => format!("{}", int),
            Object::Boolean(boolean) => format!("{}", boolean),
        };
        write!(f, "{}", s)
    }
}

pub fn eval(program: &Program) -> Object {
    eval_statements(&program.statements)
}

fn eval_statements(stmts: &[Statement]) -> Object {
    let mut result = Object::Null;

    for stmt in stmts {
        result = eval_statement(stmt)
    }

    result
}

fn eval_statement(stmt: &Statement) -> Object {
    match stmt {
        //Statement::Let(Identifier, Expression),
        //Statement::Return(Expression),
        Statement::Expression(expr) => eval_expression(expr),
        _ => Object::Null,
    }
}

fn eval_expression(expr: &Expression) -> Object {
    match expr {
        //Expression::Ident(Identifier),
        Expression::Int(int) => Object::Int(int.0),
        /*Expression::Boolean(Boolean),
        Expression::Prefix(Prefix, Box<Expression>),
        Expression::Infix(Box<Expression>, Infix, Box<Expression>),
        Expression::If(Box<Expression>, BlockStatement, Option<BlockStatement>),
        Expression::Function(Vec<Identifier>, BlockStatement),
        Expression::Call(Box<Expression>, Vec<Expression>),*/
        _ => Object::Null,
    }
}

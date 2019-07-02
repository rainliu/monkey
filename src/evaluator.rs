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
        //Expression::Ident(ident),
        Expression::Int(int) => Object::Int(int.0),
        Expression::Boolean(boolean) => Object::Boolean(boolean.0),
        Expression::Prefix(prefix, expr) => {
            let right = eval_expression(expr);
            eval_prefix_expression(prefix, right)
        }
        //Expression::Infix(Box<Expression>, Infix, Box<Expression>),
        //Expression::If(Box<Expression>, BlockStatement, Option<BlockStatement>),
        //Expression::Function(Vec<Identifier>, BlockStatement),
        //Expression::Call(Box<Expression>, Vec<Expression>),*/
        _ => Object::Null,
    }
}

fn eval_prefix_expression(prefix: &Prefix, right: Object) -> Object {
    match prefix {
        &Prefix::BANG => eval_bang_operator_expression(right),
        &Prefix::MINUS => eval_minus_operator_expression(right),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(boolean) => Object::Boolean(!boolean),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_operator_expression(right: Object) -> Object {
    match right {
        Object::Int(int) => Object::Int(-int),
        _ => Object::Null,
    }
}
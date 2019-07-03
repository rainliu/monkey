use crate::ast::*;
use std::fmt;

#[cfg(test)]
mod evaluator_test;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Null,
    Int(i64),
    Boolean(bool),
    Return(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Object::Null => "null".to_string(),
            Object::Int(int) => format!("{}", int),
            Object::Boolean(boolean) => format!("{}", boolean),
            Object::Return(obj) => format!("{}", obj),
        };
        write!(f, "{}", s)
    }
}

pub fn eval(program: &Program) -> Object {
    eval_statements(&program.statements)
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(boolean) => *boolean,
        _ => true,
    }
}

fn eval_statements(stmts: &[Statement]) -> Object {
    let mut result = Object::Null;

    for stmt in stmts {
        result = eval_statement(stmt);

        match result {
            Object::Return(obj) => return *obj,
            _ => {}
        };
    }

    result
}

fn eval_block_statements(stmts: &[Statement]) -> Object {
    let mut result = Object::Null;

    for stmt in stmts {
        result = eval_statement(stmt);

        match result {
            Object::Return(_) => return result,
            _ => {}
        };
    }

    result
}

fn eval_statement(stmt: &Statement) -> Object {
    match stmt {
        //Statement::Let(Identifier, Expression),
        Statement::Return(expr) => Object::Return(Box::new(eval_expression(expr))),
        Statement::Expression(expr) => eval_expression(expr),
        _ => Object::Null,
    }
}

fn eval_expression(expr: &Expression) -> Object {
    match expr {
        //Expression::Ident(ident),
        Expression::Int(int) => Object::Int(int.0),
        Expression::Boolean(boolean) => Object::Boolean(boolean.0),
        Expression::Prefix(prefix, right) => {
            let right = eval_expression(right);
            eval_prefix_expression(prefix, &right)
        }
        Expression::Infix(left, infix, right) => {
            let left = eval_expression(left);
            let right = eval_expression(right);
            eval_infix_expression(&left, infix, &right)
        }
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(condition);
            if is_truthy(&condition) {
                eval_block_statements(&consequence.statements)
            } else if let Some(alternative) = alternative {
                eval_block_statements(&alternative.statements)
            } else {
                Object::Null
            }
        }
        //Expression::Function(Vec<Identifier>, BlockStatement),
        //Expression::Call(Box<Expression>, Vec<Expression>),*/
        _ => Object::Null,
    }
}

fn eval_prefix_expression(prefix: &Prefix, right: &Object) -> Object {
    match prefix {
        &Prefix::BANG => eval_bang_operator_expression(right),
        &Prefix::MINUS => eval_minus_operator_expression(right),
    }
}

fn eval_bang_operator_expression(right: &Object) -> Object {
    match right {
        Object::Boolean(boolean) => Object::Boolean(!*boolean),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_operator_expression(right: &Object) -> Object {
    match right {
        Object::Int(int) => Object::Int(-*int),
        _ => Object::Null,
    }
}

fn eval_infix_expression(left: &Object, infix: &Infix, right: &Object) -> Object {
    match (left, infix, right) {
        (Object::Int(left), _, Object::Int(right)) => {
            eval_integer_infix_expression(*left, infix, *right)
        }
        (_, &Infix::EQ, _) => Object::Boolean(*left == *right),
        (_, &Infix::NEQ, _) => Object::Boolean(*left != *right),
        _ => Object::Null,
    }
}

fn eval_integer_infix_expression(left: i64, infix: &Infix, right: i64) -> Object {
    match infix {
        &Infix::PLUS => Object::Int(left + right),
        &Infix::MINUS => Object::Int(left - right),
        &Infix::ASTERISK => Object::Int(left * right),
        &Infix::SLASH => Object::Int(left / right),
        &Infix::LT => Object::Boolean(left < right),
        &Infix::GT => Object::Boolean(left > right),
        &Infix::EQ => Object::Boolean(left == right),
        &Infix::NEQ => Object::Boolean(left != right),
    }
}

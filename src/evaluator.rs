use crate::ast::*;

use std::collections::HashMap;
use std::fmt;

#[cfg(test)]
mod evaluator_test;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Null,
    Int(i64),
    Boolean(bool),
    Return(Box<Object>),
    Error(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Object::Null => "null".to_string(),
            Object::Int(int) => format!("{}", int),
            Object::Boolean(boolean) => format!("{}", boolean),
            Object::Return(obj) => format!("{}", obj),
            Object::Error(msg) => format!("{}", msg),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }
    pub fn get(&self, key: &str) -> Option<&Object> {
        self.store.get(key)
    }

    pub fn set(&mut self, key: String, value: Object) -> Option<Object> {
        self.store.insert(key, value)
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(boolean) => *boolean,
        _ => true,
    }
}

fn is_error(obj: &Object) -> bool {
    match obj {
        Object::Error(_) => true,
        _ => false,
    }
}

pub fn eval(program: &Program, env: &mut Environment) -> Object {
    eval_statements(&program.statements, env)
}

fn eval_statements(stmts: &[Statement], env: &mut Environment) -> Object {
    let mut result = Object::Null;

    for stmt in stmts {
        result = eval_statement(stmt, env);

        match result {
            Object::Return(obj) => return *obj,
            Object::Error(_) => return result,
            _ => {}
        };
    }

    result
}

fn eval_block_statements(stmts: &[Statement], env: &mut Environment) -> Object {
    let mut result = Object::Null;

    for stmt in stmts {
        result = eval_statement(stmt, env);

        match result {
            Object::Return(_) | Object::Error(_) => return result,
            _ => {}
        };
    }

    result
}

fn eval_statement(stmt: &Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::Let(ident, expr) => {
            let obj = eval_expression(expr, env);
            if is_error(&obj) {
                return obj;
            }
            env.set(ident.0.clone(), obj.clone());
            obj
        }
        Statement::Return(expr) => {
            let obj = eval_expression(expr, env);
            if is_error(&obj) {
                return obj;
            }
            Object::Return(Box::new(obj))
        }
        Statement::Expression(expr) => eval_expression(expr, env),
    }
}

fn eval_expression(expr: &Expression, env: &mut Environment) -> Object {
    match expr {
        Expression::Ident(ident) => {
            if let Some(obj) = env.get(&ident.0) {
                obj.clone()
            } else {
                Object::Error(format!("identifier not found: {}", ident))
            }
        }
        Expression::Int(int) => Object::Int(int.0),
        Expression::Boolean(boolean) => Object::Boolean(boolean.0),
        Expression::Prefix(prefix, right) => {
            let right = eval_expression(right, env);
            if is_error(&right) {
                return right;
            }
            eval_prefix_expression(prefix, &right)
        }
        Expression::Infix(left, infix, right) => {
            let left = eval_expression(left, env);
            if is_error(&left) {
                return left;
            }
            let right = eval_expression(right, env);
            if is_error(&right) {
                return right;
            }
            eval_infix_expression(&left, infix, &right)
        }
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(condition, env);
            if is_error(&condition) {
                return condition;
            }
            if is_truthy(&condition) {
                eval_block_statements(&consequence.statements, env)
            } else if let Some(alternative) = alternative {
                eval_block_statements(&alternative.statements, env)
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
        _ => Object::Error(format!("illegal operator: -{}", right)),
    }
}

fn eval_infix_expression(left: &Object, infix: &Infix, right: &Object) -> Object {
    match (left, infix, right) {
        (Object::Int(left), _, Object::Int(right)) => {
            eval_integer_infix_expression(*left, infix, *right)
        }
        (_, &Infix::EQ, _) => Object::Boolean(*left == *right),
        (_, &Infix::NEQ, _) => Object::Boolean(*left != *right),
        _ => Object::Error(format!("illegal operator: {} {} {}", left, infix, right)),
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

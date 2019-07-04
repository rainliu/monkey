use crate::ast::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[cfg(test)]
mod evaluator_test;

#[derive(Debug)]
pub struct EvalError {
    pub message: String,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for EvalError {
    fn description(&self) -> &str {
        &self.message
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Null,
    Int(i64),
    Boolean(bool),
    Return(Rc<Object>),
    Function(Vec<Identifier>, BlockStatement, Rc<RefCell<Environment>>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Object::Null => "null".to_string(),
            Object::Int(int) => format!("{}", int),
            Object::Boolean(boolean) => format!("{}", boolean),
            Object::Return(obj) => format!("{}", obj),
            Object::Function(parameters, body, _env) => {
                let params: Vec<String> =
                    parameters.iter().map(|param| param.to_string()).collect();
                format!("fn({}) {{\n{}\n}}", params.join(", "), *body)
            }
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Rc<Object>>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn from(outer: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
        Rc::new(RefCell::new(Environment {
            store: HashMap::new(),
            outer: Some(outer),
        }))
    }

    pub fn get(&self, key: &str) -> Option<Rc<Object>> {
        match self.store.get(key) {
            Some(obj) => Some(Rc::clone(obj)),
            None => match &self.outer {
                Some(outer) => outer.borrow().get(key),
                None => None,
            },
        }
    }

    pub fn set(&mut self, key: String, value: Rc<Object>) {
        self.store.insert(key, value);
    }
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(boolean) => *boolean,
        _ => true,
    }
}

pub fn eval(program: &Program, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
    eval_statements(&program.statements, env)
}

fn eval_statements(
    stmts: &[Statement],
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let mut result = Rc::new(Object::Null);

    for stmt in stmts {
        result = eval_statement(stmt, Rc::clone(&env))?;

        match &*result {
            Object::Return(obj) => return Ok(Rc::clone(obj)),
            _ => {}
        };
    }

    Ok(result)
}

fn eval_block_statements(
    stmts: &[Statement],
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let mut result = Rc::new(Object::Null);

    for stmt in stmts {
        result = eval_statement(stmt, Rc::clone(&env))?;

        match &*result {
            Object::Return(_) => return Ok(result),
            _ => {}
        };
    }

    Ok(result)
}

fn eval_statement(
    stmt: &Statement,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    match stmt {
        Statement::Let(ident, expr) => {
            let obj = eval_expression(expr, Rc::clone(&env))?;
            env.borrow_mut().set(ident.0.clone(), Rc::clone(&obj));
            Ok(obj)
        }
        Statement::Return(expr) => {
            let obj = eval_expression(expr, env)?;
            Ok(Rc::new(Object::Return(obj)))
        }
        Statement::Expression(expr) => eval_expression(expr, env),
    }
}

fn eval_expression(
    expr: &Expression,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    match expr {
        Expression::Ident(ident) => {
            if let Some(obj) = env.borrow().get(&ident.0) {
                Ok(obj)
            } else {
                Err(EvalError {
                    message: format!("identifier not found: {}", ident),
                })
            }
        }
        Expression::Int(int) => Ok(Rc::new(Object::Int(int.0))),
        Expression::Boolean(boolean) => Ok(Rc::new(Object::Boolean(boolean.0))),
        Expression::Prefix(prefix, right) => {
            let right = eval_expression(right, env)?;
            eval_prefix_expression(prefix, right)
        }
        Expression::Infix(left, infix, right) => {
            let left = eval_expression(left, Rc::clone(&env))?;
            let right = eval_expression(right, env)?;
            eval_infix_expression(left, infix, right)
        }
        Expression::If(condition, consequence, alternative) => {
            let condition = eval_expression(condition, Rc::clone(&env))?;
            if is_truthy(&condition) {
                eval_block_statements(&consequence.statements, env)
            } else if let Some(alternative) = alternative {
                eval_block_statements(&alternative.statements, env)
            } else {
                Ok(Rc::new(Object::Null))
            }
        }
        Expression::Function(parameters, body) => Ok(Rc::new(Object::Function(
            parameters.clone(),
            body.clone(),
            env,
        ))),
        Expression::Call(function, arguments) => {
            let function = eval_expression(function, Rc::clone(&env))?;
            let args = eval_expressions(arguments, env)?;
            apply_function(function, args)
        }
    }
}

fn eval_expressions(
    exprs: &[Expression],
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Rc<Object>>, EvalError> {
    let mut results = vec![];

    for expr in exprs {
        let evaluated = eval_expression(expr, Rc::clone(&env))?;
        results.push(evaluated);
    }

    Ok(results)
}

fn eval_prefix_expression(prefix: &Prefix, right: Rc<Object>) -> Result<Rc<Object>, EvalError> {
    match prefix {
        &Prefix::BANG => eval_bang_operator_expression(right),
        &Prefix::MINUS => eval_minus_operator_expression(right),
    }
}

fn eval_bang_operator_expression(right: Rc<Object>) -> Result<Rc<Object>, EvalError> {
    match &*right {
        Object::Boolean(boolean) => Ok(Rc::new(Object::Boolean(!*boolean))),
        Object::Null => Ok(Rc::new(Object::Boolean(true))),
        _ => Ok(Rc::new(Object::Boolean(false))),
    }
}

fn eval_minus_operator_expression(right: Rc<Object>) -> Result<Rc<Object>, EvalError> {
    match &*right {
        Object::Int(int) => Ok(Rc::new(Object::Int(-*int))),
        _ => Err(EvalError {
            message: format!("illegal operator: -{}", right),
        }),
    }
}

fn eval_infix_expression(
    left: Rc<Object>,
    infix: &Infix,
    right: Rc<Object>,
) -> Result<Rc<Object>, EvalError> {
    match (&*left, infix, &*right) {
        (Object::Int(left), _, Object::Int(right)) => {
            eval_integer_infix_expression(*left, infix, *right)
        }
        (_, &Infix::EQ, _) => Ok(Rc::new(Object::Boolean(*left == *right))),
        (_, &Infix::NEQ, _) => Ok(Rc::new(Object::Boolean(*left != *right))),
        _ => Err(EvalError {
            message: format!("illegal operator: {} {} {}", left, infix, right),
        }),
    }
}

fn eval_integer_infix_expression(
    left: i64,
    infix: &Infix,
    right: i64,
) -> Result<Rc<Object>, EvalError> {
    let obj = match infix {
        &Infix::PLUS => Object::Int(left + right),
        &Infix::MINUS => Object::Int(left - right),
        &Infix::ASTERISK => Object::Int(left * right),
        &Infix::SLASH => Object::Int(left / right),
        &Infix::LT => Object::Boolean(left < right),
        &Infix::GT => Object::Boolean(left > right),
        &Infix::EQ => Object::Boolean(left == right),
        &Infix::NEQ => Object::Boolean(left != right),
    };

    Ok(Rc::new(obj))
}

fn apply_function(function: Rc<Object>, args: Vec<Rc<Object>>) -> Result<Rc<Object>, EvalError> {
    match &*function {
        Object::Function(parameters, body, env) => {
            let extended_env = Environment::from(Rc::clone(env));
            for (param, arg) in parameters.iter().zip(args.iter()) {
                extended_env.borrow_mut().set(param.0.clone(), arg.clone());
            }
            let evaluated = eval(&body, extended_env)?;
            match &*evaluated {
                Object::Return(obj) => Ok(Rc::clone(obj)),
                _ => Ok(evaluated),
            }
        }
        _ => Err(EvalError {
            message: format!("not a function: {}", function),
        }),
    }
}

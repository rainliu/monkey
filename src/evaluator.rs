use crate::ast::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
    Push,
    Print,
}

impl Builtin {
    pub fn lookup(name: &str) -> Option<Rc<Object>> {
        match name {
            "len" => Some(Rc::new(Object::Builtin(Builtin::Len))),
            "first" => Some(Rc::new(Object::Builtin(Builtin::First))),
            "last" => Some(Rc::new(Object::Builtin(Builtin::Last))),
            "rest" => Some(Rc::new(Object::Builtin(Builtin::Rest))),
            "push" => Some(Rc::new(Object::Builtin(Builtin::Push))),
            "print" => Some(Rc::new(Object::Builtin(Builtin::Print))),
            _ => None,
        }
    }

    pub fn apply(&self, args: &[Rc<Object>]) -> Result<Rc<Object>, EvalError> {
        match self {
            Builtin::Len => {
                if args.len() != 1 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => Ok(Rc::new(Object::Integer(array.len() as i64))),
                    Object::String(string) => Ok(Rc::new(Object::Integer(string.len() as i64))),
                    _ => Err(EvalError {
                        message: format!("argument to \"len\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::First => {
                if args.len() != 1 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => {
                        if array.len() > 0 {
                            Ok(Rc::clone(&array[0]))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    _ => Err(EvalError {
                        message: format!("argument to \"first\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::Last => {
                if args.len() != 1 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => {
                        if array.len() > 0 {
                            Ok(Rc::clone(&array[array.len() - 1]))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    _ => Err(EvalError {
                        message: format!("argument to \"last\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::Rest => {
                if args.len() != 1 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => {
                        if array.len() > 0 {
                            let mut rest = vec![];
                            for i in 1..array.len() {
                                rest.push(Rc::new((*array[i]).clone()));
                            }
                            Ok(Rc::new(Object::Array(rest)))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    _ => Err(EvalError {
                        message: format!("argument to \"rest\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::Push => {
                if args.len() != 2 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=2", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => {
                        if array.len() > 0 {
                            let mut push = vec![];
                            for i in 0..array.len() {
                                push.push(Rc::new((*array[i]).clone()));
                            }
                            push.push(Rc::new((*args[1]).clone()));
                            Ok(Rc::new(Object::Array(push)))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    _ => Err(EvalError {
                        message: format!("argument to \"push\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::Print => {
                for arg in args {
                    println!("{}", arg);
                }
                Ok(Rc::new(Object::Null))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Object {
    Null,
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Rc<Object>>),
    Return(Rc<Object>),
    Function(Function),
    Builtin(Builtin),
    Hash(HashObject),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Object::Null => "null".to_string(),
            Object::Integer(integer) => format!("{}", integer),
            Object::Boolean(boolean) => format!("{}", boolean),
            Object::String(string) => format!("{}", string),
            Object::Array(array) => {
                let elements: Vec<String> =
                    array.iter().map(|element| element.to_string()).collect();
                format!("[{}]", elements.join(", "))
            }
            Object::Return(object) => format!("{}", object),
            Object::Function(function) => format!("{}", function),
            Object::Builtin(builtin) => format!("{:?}", builtin),
            Object::Hash(hash) => format!("{}", hash),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub parameters: Vec<String>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: Vec<String> = self
            .parameters
            .iter()
            .map(|param| param.to_string())
            .collect();
        write!(f, "fn({}) {{\n{}\n}}", params.join(", "), self.body)
    }
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("hash for function not supported");
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HashObject {
    pub pairs: HashMap<Rc<Object>, Rc<Object>>,
}

impl fmt::Display for HashObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pairs: Vec<String> = (&self.pairs)
            .iter()
            .map(|(key, value)| format!("{}: {}", key.to_string(), value.to_string()))
            .collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

impl Hash for HashObject {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("hash for HashObject not supported");
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
            env.borrow_mut().set(ident.clone(), Rc::clone(&obj));
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
        Expression::Identifier(ident) => {
            if let Some(obj) = env.borrow().get(&ident) {
                return Ok(obj);
            }

            if let Some(obj) = Builtin::lookup(&ident) {
                return Ok(obj);
            }

            Err(EvalError {
                message: format!("identifier not found: {}", ident),
            })
        }
        Expression::Integer(integer) => Ok(Rc::new(Object::Integer(*integer))),
        Expression::Boolean(boolean) => Ok(Rc::new(Object::Boolean(*boolean))),
        Expression::String(string) => Ok(Rc::new(Object::String(string.to_string()))),
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
        Expression::Function(parameters, body) => Ok(Rc::new(Object::Function(Function {
            parameters: parameters.clone(),
            body: body.clone(),
            env,
        }))),
        Expression::Call(function, arguments) => {
            let function = eval_expression(function, Rc::clone(&env))?;
            let args = eval_expressions(arguments, env)?;
            apply_function(function, args)
        }
        Expression::Array(array) => {
            let elements = eval_expressions(array, env)?;
            Ok(Rc::new(Object::Array(elements)))
        }
        Expression::Index(left, right) => {
            let array = eval_expression(left, Rc::clone(&env))?;
            let index = eval_expression(right, env)?;
            eval_index_expression(array, index)
        }
        Expression::Hash(hash) => eval_hash_expression(hash, env),
        //_ => Err(EvalError {
        //    message: format!("to be implemented {}", expr),
        //}),
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
        Object::Integer(integer) => Ok(Rc::new(Object::Integer(-*integer))),
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
        (Object::Integer(left), _, Object::Integer(right)) => {
            eval_integer_infix_expression(*left, infix, *right)
        }
        (Object::String(left), _, Object::String(right)) => {
            eval_string_infix_expression(left, infix, right)
        }
        (_, &Infix::EQ, _) => Ok(Rc::new(Object::Boolean(*left == *right))),
        (_, &Infix::NEQ, _) => Ok(Rc::new(Object::Boolean(*left != *right))),
        _ => Err(EvalError {
            message: format!("illegal operator: {} {} {}", left, infix, right),
        }),
    }
}

fn eval_index_expression(array: Rc<Object>, index: Rc<Object>) -> Result<Rc<Object>, EvalError> {
    match (&*array, &*index) {
        (Object::Array(array), Object::Integer(index)) => {
            eval_array_index_expression(array, *index)
        }
        (Object::Hash(hash), _) => eval_hash_index_expression(hash, index),
        _ => Err(EvalError {
            message: format!("index operator not supported: {}", array),
        }),
    }
}

fn eval_hash_expression(
    hash: &HashLiteral,
    env: Rc<RefCell<Environment>>,
) -> Result<Rc<Object>, EvalError> {
    let mut pairs: HashMap<Rc<Object>, Rc<Object>> = HashMap::new();

    for (key, val) in &hash.pairs {
        let key = eval_expression(key, Rc::clone(&env))?;
        let val = eval_expression(val, Rc::clone(&env))?;
        pairs.insert(key, val);
    }

    Ok(Rc::new(Object::Hash(HashObject { pairs })))
}

fn eval_integer_infix_expression(
    left: i64,
    infix: &Infix,
    right: i64,
) -> Result<Rc<Object>, EvalError> {
    let obj = match infix {
        &Infix::PLUS => Object::Integer(left + right),
        &Infix::MINUS => Object::Integer(left - right),
        &Infix::ASTERISK => Object::Integer(left * right),
        &Infix::SLASH => Object::Integer(left / right),
        &Infix::LT => Object::Boolean(left < right),
        &Infix::GT => Object::Boolean(left > right),
        &Infix::EQ => Object::Boolean(left == right),
        &Infix::NEQ => Object::Boolean(left != right),
    };

    Ok(Rc::new(obj))
}

fn eval_string_infix_expression(
    left: &str,
    infix: &Infix,
    right: &str,
) -> Result<Rc<Object>, EvalError> {
    let obj = match infix {
        &Infix::PLUS => Object::String(left.to_string() + right),
        _ => {
            return Err(EvalError {
                message: format!("illegal operator: {} {} {}", left, infix, right),
            })
        }
    };

    Ok(Rc::new(obj))
}

fn eval_array_index_expression(array: &[Rc<Object>], index: i64) -> Result<Rc<Object>, EvalError> {
    if index < 0 || index >= array.len() as i64 {
        Ok(Rc::new(Object::Null))
    } else {
        Ok(Rc::clone(&array[index as usize]))
    }
}

fn eval_hash_index_expression(
    hash: &HashObject,
    index: Rc<Object>,
) -> Result<Rc<Object>, EvalError> {
    match &*index {
        Object::String(_) | Object::Boolean(_) | Object::Integer(_) => {
            if let Some(val) = hash.pairs.get(&*index) {
                Ok(Rc::clone(val))
            } else {
                Ok(Rc::new(Object::Null))
            }
        }
        _ => Err(EvalError {
            message: format!("unusable as hash key: {}", index),
        }),
    }
}

fn apply_function(function: Rc<Object>, args: Vec<Rc<Object>>) -> Result<Rc<Object>, EvalError> {
    match &*function {
        Object::Function(function) => {
            let extended_env = Environment::from(Rc::clone(&function.env));
            for (param, arg) in function.parameters.iter().zip(args.iter()) {
                extended_env.borrow_mut().set(param.clone(), arg.clone());
            }
            let evaluated = eval(&function.body, extended_env)?;
            match &*evaluated {
                Object::Return(obj) => Ok(Rc::clone(obj)),
                _ => Ok(evaluated),
            }
        }
        Object::Builtin(builtin) => builtin.apply(&args),
        _ => Err(EvalError {
            message: format!("not a function: {}", function),
        }),
    }
}

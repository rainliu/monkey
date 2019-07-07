use super::*;
use crate::lexer::*;
use crate::parser::*;

fn is_object_integer(obj: Rc<Object>, test: &str, expected: i64) -> bool {
    match &*obj {
        Object::Integer(int) => {
            assert!(
                *int == expected,
                "Eval {}, expected Integer({}), but got {}",
                test,
                expected,
                obj
            );
            true
        }
        _ => {
            assert!(
                false,
                "Eval {}, expected Integer({}), but got {}",
                test, expected, obj
            );
            false
        }
    }
}

fn is_object_boolean(obj: Rc<Object>, test: &str, expected: bool) -> bool {
    match &*obj {
        Object::Boolean(boolean) => {
            assert!(
                *boolean == expected,
                "Eval {}, expected Boolean({}), but got {}",
                test,
                expected,
                obj
            );
            true
        }
        _ => {
            assert!(
                false,
                "Eval {}, expected Boolean({}), but got {}",
                test, expected, obj
            );
            false
        }
    }
}

fn is_object_string(obj: Rc<Object>, test: &str, expected: &str) -> bool {
    match &*obj {
        Object::String(string) => {
            assert!(
                string == expected,
                "Eval {}, expected String({}), but got {}",
                test,
                expected,
                obj
            );
            true
        }
        _ => {
            assert!(
                false,
                "Eval {}, expected String({}), but got {}",
                test, expected, obj
            );
            false
        }
    }
}

#[test]
fn test_eval_expression_integer() -> Result<(), EvalError> {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5+5+5+5-10", 10),
        ("2*2*2*2*2", 32),
        ("-50+100+-50", 0),
        ("5*2+10", 20),
        ("20+2*-10", 0),
        ("2*(5+10)", 30),
        ("3*3*3+10", 37),
        ("3*(3*3)+10", 37),
        ("(5+10*2+15/3)*2+-10", 50),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_integer(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_eval_expression_boolean() -> Result<(), EvalError> {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1<2", true),
        ("1>2", false),
        ("1<1", false),
        ("1>1", false),
        ("1==1", true),
        ("1!=1", false),
        ("1==2", false),
        ("1!=2", true),
        ("true==true", true),
        ("false==false", true),
        ("true==false", false),
        ("true!=false", true),
        ("false!=true", true),
        ("(1<2)==true", true),
        ("(1<2)==false", false),
        ("(1>2)==true", false),
        ("(1>2)==false", true),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_boolean(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_eval_expression_string() -> Result<(), EvalError> {
    let tests = vec![("\"Hello World\"", "Hello World")];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_string(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_eval_expression_string_concatenation() -> Result<(), EvalError> {
    let tests = vec![("\"Hello\" + \" \" + \"World\"", "Hello World")];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_string(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_eval_operator_bang() -> Result<(), EvalError> {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_boolean(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_eval_expression_ifelse() -> Result<(), EvalError> {
    let tests = vec![
        ("if(true) {10}", Some(10)),
        ("if(false) {10}", None),
        ("if(1) {10}", Some(10)),
        ("if(1<2) {10}", Some(10)),
        ("if(1>2) {10}", None),
        ("if(1>2) {10} else {20}", Some(20)),
        ("if(1<2) {10} else {20}", Some(10)),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        if let Some(expected) = tt.1 {
            assert_eq!(is_object_integer(evaluated, tt.0, expected), true);
        } else {
            assert_eq!(&*evaluated, &Object::Null);
        }
    }
    Ok(())
}

#[test]
fn test_eval_statement_return() -> Result<(), EvalError> {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2*5; 9;", 10),
        ("9; return 2*5; 9;", 10),
        ("if(10>1) { if(10>1) {return 10;} return 1; }", 10),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_integer(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_error_handling() -> Result<(), EvalError> {
    let tests = vec![
        ("-true", "illegal operator: -true"),
        ("5+true;", "illegal operator: 5 + true"),
        ("5+true; 5;", "illegal operator: 5 + true"),
        ("true + false;", "illegal operator: true + false"),
        ("5; true+false; 5}", "illegal operator: true + false"),
        ("if (10>1) {true+false;}", "illegal operator: true + false"),
        (
            "if(10>1) { if(10>1) {return true+false;} return 1; }",
            "illegal operator: true + false",
        ),
        ("\"Hello\" - \"World\"", "illegal operator: Hello - World"),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env));
        match evaluated {
            Err(msg) => assert!(
                &msg.message == tt.1,
                "wrong error msg in {}, expected={}, got={}",
                tt.0,
                tt.1,
                msg
            ),
            Ok(evaluated) => assert!(
                false,
                "wrong error msg in {}, expected={}, got={}",
                tt.0, tt.1, evaluated
            ),
        };
    }
    Ok(())
}

#[test]
fn test_let_statements() -> Result<(), EvalError> {
    let tests = vec![
        ("let a=5; a;", 5),
        ("let a=5*5; a;", 25),
        ("let a=5; let b=a; b;", 5),
        ("let a=5; let b=a; let c=a+b+5; c;", 15),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_integer(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_function_object() -> Result<(), EvalError> {
    let input = "fn(x) { x+2; };";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    let env = Environment::new();
    let evaluated = eval(&program, env)?;
    match &*evaluated {
        Object::Function(parameters, body, _env) => {
            assert_eq!(parameters.len(), 1);
            assert_eq!(parameters[0].0, "x");
            assert_eq!(body.to_string(), "(x + 2)")
        }
        _ => assert!(false, "object is not Function. got {}", evaluated),
    };
    Ok(())
}

#[test]
fn test_function_application() -> Result<(), EvalError> {
    let tests = vec![
        ("let identity = fn(x) {x;}; identity(5);", 5),
        ("let identity = fn(x) {return x;}; identity(5);", 5),
        ("let double = fn(x) {x*2;}; double(5);", 10),
        ("let add = fn(x,y) {x+y;}; add(5,5);", 10),
        ("let add = fn(x,y) {x+y;}; add(5+5, add(5,5));", 20),
        ("fn(x){x;}(5)", 5),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_integer(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_builtin_functions() -> Result<(), EvalError> {
    let tests = vec![
        ("len(\"\")", 0),
        ("len(\"four\")", 4),
        ("len(\"hello world\")", 11),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, Rc::clone(&env))?;
        assert_eq!(is_object_integer(evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_illegal_builtin_functions() -> Result<(), EvalError> {
    let tests = vec![
        ("len(1)", "argument to \"len\" not supported, got 1"),
        (
            "len(\"one\", \"two\")",
            "wrong number of arguments. got=2, want=1",
        ),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        match eval(&program, Rc::clone(&env)) {
            Err(error) => assert_eq!(error.message, tt.1),
            _ => {}
        }
    }
    Ok(())
}

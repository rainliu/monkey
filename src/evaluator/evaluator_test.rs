use super::*;
use crate::lexer::*;
use crate::parser::*;

fn is_object_integer(obj: &Object, test: &str, expected: i64) -> bool {
    match obj {
        Object::Integer(integer) => {
            assert!(
                *integer == expected,
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

fn is_object_boolean(obj: &Object, test: &str, expected: bool) -> bool {
    match obj {
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

fn is_object_string(obj: &Object, test: &str, expected: &str) -> bool {
    match obj {
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_boolean(&*evaluated, tt.0, tt.1), true);
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_string(&*evaluated, tt.0, tt.1), true);
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_string(&*evaluated, tt.0, tt.1), true);
    }
    Ok(())
}

#[test]
fn test_eval_expression_array() -> Result<(), EvalError> {
    let input = "[1, 2*2, 3+3]";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    let env = Environment::new();
    let evaluated = eval(&program, env)?;
    match &*evaluated {
        Object::Array(array) => {
            assert_eq!(array.len(), 3);
            assert_eq!(is_object_integer(&*array[0], input, 1), true);
            assert_eq!(is_object_integer(&*array[1], input, 4), true);
            assert_eq!(is_object_integer(&*array[2], input, 6), true);
        }
        _ => assert!(false, "Object is not Array"),
    };

    Ok(())
}

#[test]
fn test_eval_expression_array_index() -> Result<(), EvalError> {
    let tests = vec![
        ("[1, 2, 3][0]", Some(1)),
        ("[1, 2, 3][1]", Some(2)),
        ("[1, 2, 3][2]", Some(3)),
        ("let i = 0; [1][i]", Some(1)),
        ("[1, 2, 3][1+1]", Some(3)),
        ("let myArray = [1,2,3]; myArray[2];", Some(3)),
        (
            "let myArray = [1,2,3]; myArray[0]+myArray[1]+myArray[2];",
            Some(6),
        ),
        (
            "let myArray = [1,2,3]; let i = myArray[0]; myArray[i]",
            Some(2),
        ),
        ("[1,2,3][3]", None),
        ("[1,2,3][-1]", None),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, env)?;
        match &*evaluated {
            Object::Null => assert_eq!(tt.1, None),
            _ => {
                if let Some(result) = tt.1 {
                    assert_eq!(is_object_integer(&*evaluated, tt.0, result), true);
                } else {
                    assert!(false, "Wrong result: expected null, but got {}", evaluated);
                }
            }
        }
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_boolean(&*evaluated, tt.0, tt.1), true);
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
        let evaluated = eval(&program, env)?;
        if let Some(expected) = tt.1 {
            assert_eq!(is_object_integer(&*evaluated, tt.0, expected), true);
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
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
        (
            "{\"name\": \"Monkey\"}[fn(x) {x}];",
            "unusable as hash key: fn(x) {\nx\n}",
        ),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, env);
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
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
        Object::Function(function) => {
            assert_eq!(function.parameters.len(), 1);
            assert_eq!(function.parameters[0], "x");
            assert_eq!(function.body.to_string(), "(x + 2)")
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
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
        let evaluated = eval(&program, env)?;
        assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
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
        match eval(&program, env) {
            Err(error) => assert_eq!(error.message, tt.1),
            _ => {}
        }
    }
    Ok(())
}

#[test]
fn test_hash_object() -> Result<(), EvalError> {
    let input = "let two = \"two\";
        {
            \"one\": 10 - 9,
            two: 1+1,
            4:4,
            true: 5,
            false: 6
        }
    ";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    let env = Environment::new();
    let evaluated = eval(&program, env)?;
    match &*evaluated {
        Object::Hash(hash_object) => {
            assert_eq!(hash_object.pairs.len(), 5);
            for (key, val) in hash_object.pairs.iter() {
                match &**key {
                    Object::String(string) => {
                        match string.as_str() {
                            "one" => assert_eq!(is_object_integer(val, input, 1), true),
                            "two" => assert_eq!(is_object_integer(val, input, 2), true),
                            _ => assert!(false, "{} not found in {}", string, input),
                        };
                    }
                    Object::Integer(4) => assert_eq!(is_object_integer(val, input, 4), true),
                    Object::Boolean(true) => assert_eq!(is_object_integer(val, input, 5), true),
                    Object::Boolean(false) => assert_eq!(is_object_integer(val, input, 6), true),
                    _ => assert!(false, "{}: {} not found in {}", key, val, input),
                };
            }
        }
        _ => assert!(false, "object is not Hash. got {}", evaluated),
    };
    Ok(())
}

#[test]
fn test_hash_index_expressions() -> Result<(), EvalError> {
    let tests = vec![
        ("{\"foo\": 5}[\"foo\"]", Some(5)),
        ("{\"foo\": 5}[\"bar\"]", None),
        ("let key = \"foo\"; {\"foo\": 5}[key]", Some(5)),
        ("{}[\"foo\"]", None),
        ("{5:5}[5]", Some(5)),
        ("{true:5}[true]", Some(5)),
        ("{false:5}[false]", Some(5)),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = eval(&program, env)?;
        if let Some(expected) = tt.1 {
            assert_eq!(is_object_integer(&*evaluated, tt.0, expected), true);
        } else {
            assert_eq!(&*evaluated, &Object::Null);
        }
    }
    Ok(())
}

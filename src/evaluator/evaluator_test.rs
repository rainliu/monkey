use super::*;
use crate::lexer::*;
use crate::parser::*;

fn is_object_integer(obj: &Object, test: &str, expected: i64) -> bool {
    match obj {
        Object::Int(int) => {
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

#[test]
fn test_eval_expression_integer() {
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
        let evaluated = eval(&program);
        assert_eq!(is_object_integer(&evaluated, tt.0, tt.1), true);
    }
}

#[test]
fn test_eval_expression_boolean() {
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
        let evaluated = eval(&program);
        assert_eq!(is_object_boolean(&evaluated, tt.0, tt.1), true);
    }
}

#[test]
fn test_eval_operator_bang() {
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
        let evaluated = eval(&program);
        assert_eq!(is_object_boolean(&evaluated, tt.0, tt.1), true);
    }
}

#[test]
fn test_eval_expression_ifelse() {
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
        let evaluated = eval(&program);
        if let Some(expected) = tt.1 {
            assert_eq!(is_object_integer(&evaluated, tt.0, expected), true);
        } else {
            assert_eq!(evaluated, Object::Null);
        }
    }
}

#[test]
fn test_eval_statement_return() {
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
        let evaluated = eval(&program);
        assert_eq!(is_object_integer(&evaluated, tt.0, tt.1), true);
    }
}

#[test]
fn test_error_handling() {
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
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let evaluated = eval(&program);
        match evaluated {
            Object::Error(msg) => assert!(
                msg == tt.1,
                "wrong error msg in {}, expected={}, got={}",
                tt.0,
                tt.1,
                msg
            ),
            _ => assert!(
                false,
                "wrong error msg in {}, expected={}, got={}",
                tt.0, tt.1, evaluated
            ),
        };
    }
}

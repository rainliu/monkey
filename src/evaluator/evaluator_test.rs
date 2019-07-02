use super::*;
use crate::lexer::*;
use crate::parser::*;

fn is_object_integer(obj: &Object, expected: i64) -> bool {
    match obj {
        Object::Int(int) => {
            assert_eq!(*int, expected);
            true
        }
        _ => {
            assert!(false, "Expected Integer, but got {}", obj);
            false
        }
    }
}

fn is_object_boolean(obj: &Object, expected: bool) -> bool {
    match obj {
        Object::Boolean(boolean) => {
            assert_eq!(*boolean, expected);
            true
        }
        _ => {
            assert!(false, "Expected Boolean, but got {}", obj);
            false
        }
    }
}

#[test]
fn test_eval_expression_integer() {
    let tests = vec![("5", 5), ("10", 10)];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let evaluated = eval(&program);
        assert_eq!(is_object_integer(&evaluated, tt.1), true);
    }
}

#[test]
fn test_eval_expression_boolean() {
    let tests = vec![("true", true), ("false", false)];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let evaluated = eval(&program);
        assert_eq!(is_object_boolean(&evaluated, tt.1), true);
    }
}

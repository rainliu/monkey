use super::*;
use crate::lexer::*;
use crate::parser::*;

fn is_integer_object(obj: &Object, expected: i64) -> bool {
    match obj {
        Object::Int(int) => {
            assert_eq!(*int, expected);
            true
        }
        _ => false,
    }
}

#[test]
fn test_eval_expression_integer() {
    let tests = vec![("5;", 5), ("10", 10)];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let evaluated = eval(&program);
        assert_eq!(is_integer_object(&evaluated, tt.1), true);
    }
}

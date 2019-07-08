use super::*;

#[test]
fn test_string() {
    let program = Program {
        statements: vec![Statement::Let(
            "myVar".to_string(),
            Expression::Identifier("anotherVar".to_string()),
        )],
    };

    assert_eq!(&program.to_string(), "let myVar = anotherVar;");
}

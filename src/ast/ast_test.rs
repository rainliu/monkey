use super::*;

#[test]
fn test_string() {
    let program = Program {
        statements: vec![Statement::Let(
            Identifier("myVar".to_string()),
            Expression::Ident(Identifier("anotherVar".to_string())),
        )],
    };

    assert_eq!(&program.to_string(), "let myVar = anotherVar;");
}

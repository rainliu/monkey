use super::*;

#[test]
fn test_string() {
    let program = Program {
        statements: vec![Statement::Let(
            IdentifierLiteral("myVar".to_string()),
            Expression::Identifier(IdentifierLiteral("anotherVar".to_string())),
        )],
    };

    assert_eq!(&program.to_string(), "let myVar = anotherVar;");
}

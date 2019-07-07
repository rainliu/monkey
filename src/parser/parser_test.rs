use super::*;

fn check_parser_errors(p: &Parser) {
    let errors = p.errors();
    if errors.len() == 0 {
        return;
    }

    println!("parser has {} errors", errors.len());
    for error in errors {
        println!("parser error: {}", error);
    }
    assert_eq!(errors.len(), 0);
}

fn is_statement_let(stmt: &Statement, name: &str) -> bool {
    match stmt {
        Statement::Let(ident, _expr) => {
            assert_eq!(ident.0, name);
            true
        }
        _ => false,
    }
}

fn is_expression_integer(expr: &Expression, value: i64) -> bool {
    match expr {
        Expression::Integer(int) => {
            assert_eq!(int.0, value);
            true
        }
        _ => false,
    }
}

fn is_expression_identifier(expr: &Expression, value: &str) -> bool {
    match expr {
        Expression::Identifier(ident) => {
            assert_eq!(ident.0, value);
            true
        }
        _ => false,
    }
}

fn is_expression_boolean(expr: &Expression, value: bool) -> bool {
    match expr {
        Expression::Boolean(boolean) => {
            assert_eq!(boolean.0, value);
            true
        }
        _ => false,
    }
}

fn is_expression_infix(
    expr: &Expression,
    left_expected: &Literal,
    infix_expected: &Infix,
    right_expected: &Literal,
) -> bool {
    match expr {
        Expression::Infix(left, infix, right) => {
            if !is_expression_literal(left, left_expected) {
                return false;
            }
            assert_eq!(*infix, *infix_expected);

            if !is_expression_literal(right, right_expected) {
                return false;
            }

            true
        }
        _ => false,
    }
}

enum Literal {
    Int(i64),
    Ident(String),
    Boolean(bool),
}

fn is_expression_literal(expr: &Expression, expected: &Literal) -> bool {
    match expected {
        Literal::Int(int) => is_expression_integer(expr, *int),
        Literal::Ident(ident) => is_expression_identifier(expr, ident),
        Literal::Boolean(boolean) => is_expression_boolean(expr, *boolean),
    }
}

#[test]
fn test_statement_let() {
    let input = "let x = 5;
    let y = 10;
    let foobar = 838383";

    let tests = vec!["x", "y", "foobar"];

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 3);

    for (tt, stmt) in tests.iter().zip(program.statements.iter()) {
        assert_eq!(is_statement_let(stmt, tt), true);
    }
}

#[test]
fn test_statement_lets() {
    let tests = vec![
        ("let x = 5;", "x", Literal::Int(5)),
        ("let y = true;", "y", Literal::Boolean(true)),
        ("let foobar = y", "foobar", Literal::Ident("y".to_string())),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];
        match stmt {
            Statement::Let(ident, expr) => {
                assert_eq!(ident.0, tt.1);
                assert_eq!(is_expression_literal(expr, &tt.2), true);
            }
            _ => assert!(false, "Statement is not Let"),
        }
    }
}

#[test]
fn test_statement_let_errors() {
    let input = "let x 5;
    let  = 10;
    let  838383";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    //check_parser_errors(&p);
    assert_eq!(program.statements.len(), 3);
}

#[test]
fn test_statement_return() {
    let input = "return 5;
    return 10;
    return 993322;";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 3);

    for stmt in &program.statements {
        match stmt {
            Statement::Return(_) => {}
            _ => assert!(false, "Statement is not Return"),
        };
    }
}

#[test]
fn test_expression_identifier() {
    let input = "foobar;";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Identifier(ident) => assert_eq!(ident.0, "foobar"),
                _ => assert!(false, "Expression is not Ident"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_expression_integer() {
    let input = "5;";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Integer(int) => assert_eq!(int.0, 5),
                _ => assert!(false, "Expression is not Int"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_expression_string() {
    let input = "\"hello world\";";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::String(string) => assert_eq!(string.0, "hello world"),
                _ => assert!(false, "Expression is not String"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_expression_array() {
    let input = "[1, 2*2, 3+3]";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Array(array) => {
                    assert_eq!(array.len(), 3);
                    assert_eq!(is_expression_literal(&array[0], &Literal::Int(1)), true);
                    assert_eq!(
                        is_expression_infix(
                            &array[1],
                            &Literal::Int(2),
                            &Infix::ASTERISK,
                            &Literal::Int(2)
                        ),
                        true
                    );
                    assert_eq!(
                        is_expression_infix(
                            &array[2],
                            &Literal::Int(3),
                            &Infix::PLUS,
                            &Literal::Int(3)
                        ),
                        true
                    );
                }
                _ => assert!(false, "Expression is not Array"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_expression_if() {
    let input = "if (x<y) {x}";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::If(condition, consequence, alternative) => {
                    assert_eq!(
                        is_expression_infix(
                            condition,
                            &Literal::Ident("x".to_string()),
                            &Infix::LT,
                            &Literal::Ident("y".to_string())
                        ),
                        true
                    );
                    assert_eq!(consequence.statements.len(), 1);
                    if let Some(stmt) = consequence.statements.first() {
                        match stmt {
                            Statement::Expression(expr) => assert_eq!(
                                is_expression_literal(expr, &Literal::Ident("x".to_string())),
                                true
                            ),
                            _ => assert!(false, "Statement is not Expression"),
                        };

                        assert!(alternative.is_none(), "alternative should be none");
                    } else {
                        assert!(false, "no statement in consequence");
                    }
                }
                _ => assert!(false, "Expression is not If"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_expression_ifelse() {
    let input = "if (x<y) {x} else {y}";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::If(condition, consequence, alternative) => {
                    assert_eq!(
                        is_expression_infix(
                            condition,
                            &Literal::Ident("x".to_string()),
                            &Infix::LT,
                            &Literal::Ident("y".to_string())
                        ),
                        true
                    );
                    assert_eq!(consequence.statements.len(), 1);
                    if let Some(stmt) = consequence.statements.first() {
                        match stmt {
                            Statement::Expression(expr) => assert_eq!(
                                is_expression_literal(expr, &Literal::Ident("x".to_string())),
                                true
                            ),
                            _ => assert!(false, "Statement is not Expression"),
                        };

                        if let Some(alternative) = alternative {
                            if let Some(stmt) = alternative.statements.first() {
                                match stmt {
                                    Statement::Expression(expr) => assert_eq!(
                                        is_expression_literal(
                                            expr,
                                            &Literal::Ident("y".to_string())
                                        ),
                                        true
                                    ),
                                    _ => assert!(false, "Statement is not Expression"),
                                };
                            } else {
                                assert!(false, "no statement in alternative");
                            }
                        } else {
                            assert!(false, "alternative should be not none");
                        }
                    } else {
                        assert!(false, "no statement in consequence");
                    }
                }
                _ => assert!(false, "Expression is not If"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_expression_function() {
    let input = "fn(x,y) {x+y;}";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Function(parameters, body) => {
                    assert_eq!(parameters.len(), 2);
                    assert_eq!(parameters[0], IdentifierLiteral("x".to_string()));
                    assert_eq!(parameters[1], IdentifierLiteral("y".to_string()));
                    assert_eq!(body.statements.len(), 1);
                    if let Some(stmt) = body.statements.first() {
                        match stmt {
                            Statement::Expression(expr) => assert_eq!(
                                is_expression_infix(
                                    expr,
                                    &Literal::Ident("x".to_string()),
                                    &Infix::PLUS,
                                    &Literal::Ident("y".to_string())
                                ),
                                true
                            ),
                            _ => assert!(false, "Statement is not Expression"),
                        };
                    } else {
                        assert!(false, "no statement in body");
                    }
                }
                _ => assert!(false, "Expression is not Function"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_expression_function_parameters() {
    let tests = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec![IdentifierLiteral("x".to_string())]),
        (
            "fn(x,y,z) {x+y;}",
            vec![
                IdentifierLiteral("x".to_string()),
                IdentifierLiteral("y".to_string()),
                IdentifierLiteral("z".to_string()),
            ],
        ),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Function(parameters, _body) => {
                        assert_eq!(parameters.len(), tt.1.len());
                        for (t, p) in tt.1.iter().zip(parameters.iter()) {
                            assert_eq!(*t, *p);
                        }
                    }
                    _ => assert!(false, "Expression is not Function"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }
}

#[test]
fn test_expression_call() {
    let input = "add(1, 2*3, 4+5);";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
    assert_eq!(program.statements.len(), 1);

    for stmt in &program.statements {
        match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Call(function, arguments) => {
                    assert_eq!(
                        is_expression_literal(function, &Literal::Ident("add".to_string())),
                        true
                    );

                    assert_eq!(arguments.len(), 3);

                    assert_eq!(
                        is_expression_literal(&arguments[0], &Literal::Int(1),),
                        true
                    );
                    assert_eq!(
                        is_expression_infix(
                            &arguments[1],
                            &Literal::Int(2),
                            &Infix::ASTERISK,
                            &Literal::Int(3)
                        ),
                        true
                    );
                    assert_eq!(
                        is_expression_infix(
                            &arguments[2],
                            &Literal::Int(4),
                            &Infix::PLUS,
                            &Literal::Int(5)
                        ),
                        true
                    );
                }
                _ => assert!(false, "Expression is not Function"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    let prefix_tests = vec![
        ("!5;", "!", Literal::Int(5)),
        ("-15", "-", Literal::Int(15)),
        ("!true", "!", Literal::Boolean(true)),
        ("!false", "!", Literal::Boolean(false)),
    ];

    for tt in prefix_tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Prefix(prefix, right) => {
                        assert_eq!(prefix.to_string(), tt.1);
                        assert_eq!(is_expression_literal(right, &tt.2), true);
                    }
                    _ => assert!(false, "Expression is not Prefix"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let infix_tests = vec![
        ("5+5;", Literal::Int(5), "+", Literal::Int(5)),
        ("5-5;", Literal::Int(5), "-", Literal::Int(5)),
        ("5*5;", Literal::Int(5), "*", Literal::Int(5)),
        ("5/5;", Literal::Int(5), "/", Literal::Int(5)),
        ("5>5;", Literal::Int(5), ">", Literal::Int(5)),
        ("5<5;", Literal::Int(5), "<", Literal::Int(5)),
        ("5==5;", Literal::Int(5), "==", Literal::Int(5)),
        ("5!=5;", Literal::Int(5), "!=", Literal::Int(5)),
        (
            "true==true",
            Literal::Boolean(true),
            "==",
            Literal::Boolean(true),
        ),
        (
            "true!=false",
            Literal::Boolean(true),
            "!=",
            Literal::Boolean(false),
        ),
        (
            "false==false",
            Literal::Boolean(false),
            "==",
            Literal::Boolean(false),
        ),
    ];

    for tt in infix_tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Infix(left, infix, right) => {
                        assert_eq!(is_expression_literal(left, &tt.1), true);
                        assert_eq!(infix.to_string(), tt.2);
                        assert_eq!(is_expression_literal(right, &tt.3), true);
                    }
                    _ => assert!(false, "Expression is not Infix"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        ("-a*b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a+b+c", "((a + b) + c)"),
        ("a+b-c", "((a + b) - c)"),
        ("a*b*c", "((a * b) * c)"),
        ("a*b/c", "((a * b) / c)"),
        ("a+b/c", "(a + (b / c))"),
        ("a+b*c+d/e-f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3+4; -5*5", "(3 + 4)\n((-5) * 5)"),
        ("5>4==3<4", "((5 > 4) == (3 < 4))"),
        ("5>4!=3<4", "((5 > 4) != (3 < 4))"),
        ("3+4*5==3*1+4*5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        ("true", "true"),
        ("false", "false"),
        ("3>5==false", "((3 > 5) == false)"),
        ("3<5==true", "((3 < 5) == true)"),
        ("1+(2+3)+4", "((1 + (2 + 3)) + 4)"),
        ("(5+5)*2", "((5 + 5) * 2)"),
        ("2/(5+5)", "(2 / (5 + 5))"),
        ("-(5+5)", "(-(5 + 5))"),
        ("!(true==true)", "(!(true == true))"),
        ("a + add(b *c) +d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2*3, 4 +5, add(6, 7*8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d/f+g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
        ("a*[1,2,3,4][b*c]*d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
        (
            "add(a*b[2], b[1], 2*[1,2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        ),
    ];

    for tt in tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);

        assert_eq!(program.to_string(), tt.1);
    }
}

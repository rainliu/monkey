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
        Expression::Int(int) => {
            assert_eq!(int.0, value);
            true
        }
        _ => false,
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
fn test_statement_let_errors() {
    let input = "let x 5;
    let  = 10;
    let  838383";

    let l = Lexer::new(input);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(&p);
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
                Expression::Ident(ident) => assert_eq!(ident.0, "foobar"),
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
                Expression::Int(int) => assert_eq!(int.0, 5),
                _ => assert!(false, "Expression is not Int"),
            },
            _ => assert!(false, "Statement is not Expression"),
        };
    }
}

#[test]
fn test_parseing_prefix_expression() {
    let prefix_tests = vec![("!5;", "!", 5), ("-15", "-", 15)];

    for tt in prefix_tests {
        let l = Lexer::new(tt.0);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Prefix(operator, integer) => {
                        assert_eq!(operator.to_string(), tt.1);
                        assert_eq!(is_expression_integer(integer, tt.2), true);
                    }
                    _ => assert!(false, "Expression is not Prefix"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }
}

use super::*;

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
    assert_eq!(program.statements.len(), 0);
}

fn is_statement_let(s: &Statement, name: &str) -> bool {
    match s {
        Statement::Let(ident, _expr) => {
            match &ident.token {
                Token::IDENT(s) => assert_eq!(s, name),
                _ => {},
            };

            true
        },
        //_ => false,
    }
}

fn check_parser_errors(p: &Parser) {
    let errors = p.errors();
    if errors.len() == 0 {
        return;
    }

    println!("parser has {} errors", errors.len());
    for error in errors{
        println!("parser error: {}", error);
    }
}
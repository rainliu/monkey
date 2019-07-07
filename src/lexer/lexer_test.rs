use super::*;

#[test]
fn test_next_token() {
    let input = "=+(){},;";
    let tests = vec![
        Token::ASSIGN,
        Token::PLUS,
        Token::LPAREN,
        Token::RPAREN,
        Token::LBRACE,
        Token::RBRACE,
        Token::COMMA,
        Token::SEMICOLON,
        Token::EOF,
    ];

    let mut l = Lexer::new(input);
    for test in &tests {
        let tok = l.next_token();
        assert_eq!(tok, *test);
    }
}

#[test]
fn test_next_token_statements() {
    let input = "let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);
  ";

    let tests = vec![
        Token::LET,
        Token::IDENT("five".to_string()),
        Token::ASSIGN,
        Token::INT("5".to_string()),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("ten".to_string()),
        Token::ASSIGN,
        Token::INT("10".to_string()),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("add".to_string()),
        Token::ASSIGN,
        Token::FUNCTION,
        Token::LPAREN,
        Token::IDENT("x".to_string()),
        Token::COMMA,
        Token::IDENT("y".to_string()),
        Token::RPAREN,
        Token::LBRACE,
        Token::IDENT("x".to_string()),
        Token::PLUS,
        Token::IDENT("y".to_string()),
        Token::SEMICOLON,
        Token::RBRACE,
        Token::SEMICOLON,
        Token::LET,
        Token::IDENT("result".to_string()),
        Token::ASSIGN,
        Token::IDENT("add".to_string()),
        Token::LPAREN,
        Token::IDENT("five".to_string()),
        Token::COMMA,
        Token::IDENT("ten".to_string()),
        Token::RPAREN,
        Token::SEMICOLON,
        Token::EOF,
    ];

    let mut l = Lexer::new(input);
    for test in &tests {
        let tok = l.next_token();
        assert_eq!(tok, *test);
    }
}

#[test]
fn test_next_token_extended() {
    let input = "!-/*5;
    5 < 10 > 5;
    if ( 5 < 10 ) {
        return true;
    } else {
        return false;
    }

    10 == 10;
    10 != 9;
    ";

    let tests = vec![
        Token::BANG,
        Token::MINUS,
        Token::SLASH,
        Token::ASTERISK,
        Token::INT("5".to_string()),
        Token::SEMICOLON,
        Token::INT("5".to_string()),
        Token::LT,
        Token::INT("10".to_string()),
        Token::GT,
        Token::INT("5".to_string()),
        Token::SEMICOLON,
        Token::IF,
        Token::LPAREN,
        Token::INT("5".to_string()),
        Token::LT,
        Token::INT("10".to_string()),
        Token::RPAREN,
        Token::LBRACE,
        Token::RETURN,
        Token::TRUE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::ELSE,
        Token::LBRACE,
        Token::RETURN,
        Token::FALSE,
        Token::SEMICOLON,
        Token::RBRACE,
        Token::INT("10".to_string()),
        Token::EQ,
        Token::INT("10".to_string()),
        Token::SEMICOLON,
        Token::INT("10".to_string()),
        Token::NEQ,
        Token::INT("9".to_string()),
        Token::SEMICOLON,
        Token::EOF,
    ];

    let mut l = Lexer::new(input);
    for test in &tests {
        let tok = l.next_token();
        assert_eq!(tok, *test);
    }
}

#[test]
fn test_next_token_string() {
    let input = "\"foobar\";
    \"foo bar\";
    ";

    let tests = vec![
        Token::STRING("foobar".to_string()),
        Token::SEMICOLON,
        Token::STRING("foo bar".to_string()),
        Token::SEMICOLON,
        Token::EOF,
    ];

    let mut l = Lexer::new(input);
    for test in &tests {
        let tok = l.next_token();
        assert_eq!(tok, *test);
    }
}

#[test]
fn test_next_token_bracket() {
    let input = "[1,2];";

    let tests = vec![
        Token::LBRACKET,
        Token::INT("1".to_string()),
        Token::COMMA,
        Token::INT("2".to_string()),
        Token::RBRACKET,
        Token::SEMICOLON,
        Token::EOF,
    ];

    let mut l = Lexer::new(input);
    for test in &tests {
        let tok = l.next_token();
        assert_eq!(tok, *test);
    }
}

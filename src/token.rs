use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String), // add, foobar, x, y, ...
    INT(String),   // 123456

    // Operators
    ASSIGN,   // =
    PLUS,     // +
    MINUS,    // -
    BANG,     // !
    ASTERISK, // *
    SLASH,    // /
    LT,       // <
    GT,       // >
    EQ,       // ==
    NEQ,      // !=

    // Delimiters
    COMMA,     // ,
    SEMICOLON, // ;

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }

    // Keywords
    FUNCTION, // fn
    LET,      // let
    TRUE,     // true
    FALSE,    // false
    IF,       // if
    ELSE,     // else
    RETURN,   // return
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, Token> = {
        let mut map = HashMap::new();
        map.insert("fn", Token::FUNCTION);
        map.insert("let", Token::LET);
        map.insert("true", Token::TRUE);
        map.insert("false", Token::FALSE);
        map.insert("if", Token::IF);
        map.insert("else", Token::ELSE);
        map.insert("return", Token::RETURN);
        map
    };
}

impl Token {
    pub fn lookup_ident(ident: String) -> Token {
        if let Some(tok) = KEYWORDS.get(ident.as_str()) {
            tok.clone()
        } else {
            Token::IDENT(ident)
        }
    }
}

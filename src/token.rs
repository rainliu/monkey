#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    ILLEGAL,
    EOF,

    // Identifiers + literals
    IDENT(String),  // add, foobar, x, y, ...
    INT(String),    // 123456
    STRING(String), // string

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

impl Token {
    pub fn lookup_ident(ident: String) -> Token {
        match ident.as_str() {
            "fn" => Token::FUNCTION,
            "let" => Token::LET,
            "true" => Token::TRUE,
            "false" => Token::FALSE,
            "if" => Token::IF,
            "else" => Token::ELSE,
            "return" => Token::RETURN,
            _ => Token::IDENT(ident),
        }
    }
}

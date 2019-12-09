use std::fmt;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Token {
    ILLEGAL(char),
    EOF,

    IDENTIFIER(String),
    INTEGER(i64),
    STRING(String),

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NE,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::ILLEGAL(c) => write!(f, "illegal({})", c),
            Token::EOF => write!(f, "EOF"),

            Token::IDENTIFIER(s) => write!(f, "identifier({})", s),
            Token::INTEGER(i) => i.fmt(f),
            Token::STRING(s) => s.fmt(f),

            Token::ASSIGN => write!(f, "="),
            Token::PLUS => write!(f, "+"),
            Token::MINUS => write!(f, "-"),
            Token::BANG => write!(f, "!"),
            Token::ASTERISK => write!(f, "*"),
            Token::SLASH => write!(f, "/"),
            Token::EQ => write!(f, "=="),
            Token::NE => write!(f, "!="),
            Token::LT => write!(f, "<"),
            Token::GT => write!(f, ">"),

            Token::COMMA => write!(f, ","),
            Token::SEMICOLON => write!(f, ";"),
            Token::LPAREN => write!(f, "("),
            Token::RPAREN => write!(f, ")"),
            Token::LBRACE => write!(f, "{{"),
            Token::RBRACE => write!(f, "}}"),

            Token::FUNCTION => write!(f, "fn"),
            Token::LET => write!(f, "let"),
            Token::TRUE => write!(f, "true"),
            Token::FALSE => write!(f, "false"),
            Token::IF => write!(f, "if"),
            Token::ELSE => write!(f, "else"),
            Token::RETURN => write!(f, "return"),
        }
    }
}

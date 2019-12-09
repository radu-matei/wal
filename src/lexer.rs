use crate::token::Token;

#[derive(Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    current_char: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Result<Self, LexerError> {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            current_char: 0 as char,
        };
        l.read_char()?;

        Ok(l)
    }

    pub fn read_char(&mut self) -> Result<(), LexerError> {
        if self.read_position >= self.input.len() {
            self.current_char = 0 as char;
        } else {
            match self.input.chars().nth(self.read_position) {
                Some(ch) => self.current_char = ch,
                None => {
                    return Err(LexerError::ReadIndexOutOfRange(self.read_position));
                }
            }
        }

        self.position = self.read_position;
        self.read_position += 1;

        Ok(())
    }

    pub fn peek_char(&mut self) -> Result<char, LexerError> {
        if self.read_position >= self.input.len() {
            return Ok(0 as char);
        } else {
            match self.input.chars().nth(self.read_position) {
                Some(ch) => {
                    return Ok(ch);
                }
                None => {
                    return Err(LexerError::ReadIndexOutOfRange(self.read_position));
                }
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        self.skip_whitespace()?;
        let t = match self.current_char {
            '=' => {
                if self.peek_char()? == '=' {
                    self.read_char()?;
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            '+' => Token::PLUS,
            '-' => Token::MINUS,
            '!' => {
                if self.peek_char()? == '=' {
                    self.read_char()?;
                    Token::NE
                } else {
                    Token::BANG
                }
            }
            '/' => Token::SLASH,
            '*' => Token::ASTERISK,
            '<' => Token::LT,
            '>' => Token::GT,
            ';' => Token::SEMICOLON,
            '(' => Token::LPAREN,
            ')' => Token::RPAREN,
            ',' => Token::COMMA,
            '{' => Token::LBRACE,
            '}' => Token::RBRACE,
            '"' => self.read_string()?,
            '\u{0}' => Token::EOF,

            _ => {
                if is_letter(self.current_char) {
                    let ident = self.read_identifier()?;
                    match lookup_identifier(&ident) {
                        Some(keyword) => return Ok(keyword),
                        None => return Ok(Token::IDENTIFIER(ident)),
                    }
                } else if is_digit(self.current_char) {
                    return self.read_number();
                } else {
                    Token::ILLEGAL(self.current_char)
                }
            }
        };
        self.read_char()?;
        Ok(t)
    }

    pub fn read_identifier(&mut self) -> Result<String, LexerError> {
        let pos = self.position;
        while is_letter(self.current_char) {
            self.read_char()?;
        }

        Ok(self
            .input
            .chars()
            .skip(pos)
            .take(self.position - pos)
            .collect())
    }

    pub fn read_number(&mut self) -> Result<Token, LexerError> {
        let pos = self.position;
        while is_digit(self.current_char) && !self.current_char.is_whitespace() {
            self.read_char()?;
        }

        let num_str: String = self
            .input
            .chars()
            .skip(pos)
            .take(self.position - pos)
            .collect();
        Ok(Token::INTEGER(num_str.parse::<i64>()?))
    }

    pub fn read_string(&mut self) -> Result<Token, LexerError> {
        let pos = self.position + 1;

        loop {
            self.read_char()?;
            match self.current_char {
                '"' => break,
                '\u{0000}' => {
                    return Err(LexerError::UnexpectedEOF);
                }
                _ => {}
            }
        }

        Ok(Token::STRING(
            self.input
                .chars()
                .skip(pos)
                .take(self.position - pos)
                .collect(),
        ))
    }

    pub fn skip_whitespace(&mut self) -> Result<(), LexerError> {
        while self.current_char == ' '
            || self.current_char == '\t'
            || self.current_char == '\n'
            || self.current_char == '\r'
        {
            self.read_char()?;
        }

        Ok(())
    }
}

pub fn is_letter(c: char) -> bool {
    'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'
}

pub fn is_digit(c: char) -> bool {
    '0' <= c && c <= '9'
}

pub fn lookup_identifier(s: &str) -> Option<Token> {
    match s {
        "fn" => Some(Token::FUNCTION),
        "let" => Some(Token::LET),
        "true" => Some(Token::TRUE),
        "false" => Some(Token::FALSE),
        "if" => Some(Token::IF),
        "else" => Some(Token::ELSE),
        "return" => Some(Token::RETURN),

        _ => None,
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    ReadIndexOutOfRange(usize),
    ParseIntError(String),
    UnexpectedEOF,
}

impl From<std::num::ParseIntError> for LexerError {
    fn from(_: std::num::ParseIntError) -> LexerError {
        LexerError::ParseIntError(String::from("cannot parse integer"))
    }
}

#[test]
fn test_next_token() {
    let input = r#"
let five = 46;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
     return true;
 } else {
     return false;
}
10 == 10;
10 != 9;
"#;

    let tests = vec![
        Token::LET,
        Token::IDENTIFIER(String::from("five")),
        Token::ASSIGN,
        Token::INTEGER(46),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENTIFIER(String::from("ten")),
        Token::ASSIGN,
        Token::INTEGER(10),
        Token::SEMICOLON,
        Token::LET,
        Token::IDENTIFIER(String::from("add")),
        Token::ASSIGN,
        Token::FUNCTION,
        Token::LPAREN,
        Token::IDENTIFIER(String::from("x")),
        Token::COMMA,
        Token::IDENTIFIER(String::from("y")),
        Token::RPAREN,
        Token::LBRACE,
        Token::IDENTIFIER(String::from("x")),
        Token::PLUS,
        Token::IDENTIFIER(String::from("y")),
        Token::SEMICOLON,
        Token::RBRACE,
        Token::SEMICOLON,
        Token::LET,
        Token::IDENTIFIER(String::from("result")),
        Token::ASSIGN,
        Token::IDENTIFIER(String::from("add")),
        Token::LPAREN,
        Token::IDENTIFIER(String::from("five")),
        Token::COMMA,
        Token::IDENTIFIER(String::from("ten")),
        Token::RPAREN,
        Token::SEMICOLON,
        Token::BANG,
        Token::MINUS,
        Token::SLASH,
        Token::ASTERISK,
        Token::INTEGER(5),
        Token::SEMICOLON,
        Token::INTEGER(5),
        Token::LT,
        Token::INTEGER(10),
        Token::GT,
        Token::INTEGER(5),
        Token::SEMICOLON,
        Token::IF,
        Token::LPAREN,
        Token::INTEGER(5),
        Token::LT,
        Token::INTEGER(10),
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
        Token::INTEGER(10),
        Token::EQ,
        Token::INTEGER(10),
        Token::SEMICOLON,
        Token::INTEGER(10),
        Token::NE,
        Token::INTEGER(9),
        Token::SEMICOLON,
        Token::EOF,
    ];

    let mut l = Lexer::new(input).unwrap();

    for t in tests.iter() {
        let tok = l.next_token().unwrap();
        assert_eq!(
            *t, tok,
            "expected {:?} token but got {:?} at character '{}' and position {}",
            t, tok, l.current_char, l.position
        )
    }
}

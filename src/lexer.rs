use crate::token::Token;

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
                    Token::Assign
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                if self.peek_char()? == '=' {
                    self.read_char()?;
                    Token::NE
                } else {
                    Token::Bang
                }
            }
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => Token::LT,
            '>' => Token::GT,
            ';' => Token::Semicolon,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            ',' => Token::Comma,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '\u{0}' => Token::EOF,

            _ => {
                if is_letter(self.current_char) {
                    let ident = self.read_identifier()?;
                    match lookup_identifier(&ident) {
                        Some(keyword) => return Ok(keyword),
                        None => return Ok(Token::Identifier(ident)),
                    }
                } else if is_digit(self.current_char) {
                    return self.read_number();
                } else {
                    Token::Illegal(self.current_char)
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
        Ok(Token::Integer(num_str.parse::<i64>()?))
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
        "fn" => Some(Token::Function),
        "let" => Some(Token::Let),
        "true" => Some(Token::True),
        "false" => Some(Token::False),
        "if" => Some(Token::If),
        "else" => Some(Token::Else),
        "return" => Some(Token::Return),

        _ => None,
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    ReadIndexOutOfRange(usize),
    ParseIntError(String),
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
        Token::Let,
        Token::Identifier(String::from("five")),
        Token::Assign,
        Token::Integer(46),
        Token::Semicolon,
        Token::Let,
        Token::Identifier(String::from("ten")),
        Token::Assign,
        Token::Integer(10),
        Token::Semicolon,
        Token::Let,
        Token::Identifier(String::from("add")),
        Token::Assign,
        Token::Function,
        Token::LeftParen,
        Token::Identifier(String::from("x")),
        Token::Comma,
        Token::Identifier(String::from("y")),
        Token::RightParen,
        Token::LeftBrace,
        Token::Identifier(String::from("x")),
        Token::Plus,
        Token::Identifier(String::from("y")),
        Token::Semicolon,
        Token::RightBrace,
        Token::Semicolon,
        Token::Let,
        Token::Identifier(String::from("result")),
        Token::Assign,
        Token::Identifier(String::from("add")),
        Token::LeftParen,
        Token::Identifier(String::from("five")),
        Token::Comma,
        Token::Identifier(String::from("ten")),
        Token::RightParen,
        Token::Semicolon,
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Integer(5),
        Token::Semicolon,
        Token::Integer(5),
        Token::LT,
        Token::Integer(10),
        Token::GT,
        Token::Integer(5),
        Token::Semicolon,
        Token::If,
        Token::LeftParen,
        Token::Integer(5),
        Token::LT,
        Token::Integer(10),
        Token::RightParen,
        Token::LeftBrace,
        Token::Return,
        Token::True,
        Token::Semicolon,
        Token::RightBrace,
        Token::Else,
        Token::LeftBrace,
        Token::Return,
        Token::False,
        Token::Semicolon,
        Token::RightBrace,
        Token::Integer(10),
        Token::EQ,
        Token::Integer(10),
        Token::Semicolon,
        Token::Integer(10),
        Token::NE,
        Token::Integer(9),
        Token::Semicolon,
        Token::EOF,
    ];

    let mut l = Lexer::new(input).unwrap();

    for t in tests.iter() {
        let tok = l.next_token().unwrap();
        println!("{:?} at pos {}", tok, l.position);

        assert_eq!(
            *t, tok,
            "expected {:?} token but got {:?} at character '{}' and position {}",
            t, tok, l.current_char, l.position
        )
    }
}

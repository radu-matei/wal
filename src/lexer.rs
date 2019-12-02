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

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        let t = match self.current_char {
            '=' => Token::Assign,
            ';' => Token::Semicolon,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,

            // TODO
            _ => Token::Illegal(self.current_char),
        };

        self.read_char()?;
        Ok(t)
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    ReadIndexOutOfRange(usize),
}

#[test]
fn test_next_token() {
    let input = "=+(){},;";
    let tests = vec![
        Token::Assign,
        Token::Plus,
        Token::LeftParen,
        Token::RightParen,
        Token::LeftBrace,
        Token::RightBrace,
        Token::Comma,
        Token::Semicolon,
    ];

    let mut l = Lexer::new(input).unwrap();

    for t in tests.iter() {
        let tok = l.next_token().unwrap();
        assert_eq!(*t, tok, "expected {:?} token but got {:?}", t, tok)
    }
}

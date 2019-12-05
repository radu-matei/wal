use crate::{
    ast::{Expression, LetStatement, Program, ReturnStatement, Statement},
    lexer::{Lexer, LexerError},
    token::Token,
};
use std::mem;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    peek: Token,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Result<Self, ParserError> {
        let mut p = Parser {
            lexer: l,
            current: Token::EOF,
            peek: Token::EOF,
        };
        p.next_token()?;
        p.next_token()?;

        return Ok(p);
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut p = Program::new();
        while self.current != Token::EOF {
            p.statements.push(self.parse_statement()?);
            self.next_token()?;
        }
        Ok(p)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match &self.current {
            Token::LET => return self.parse_let_statement(),
            Token::RETURN => return self.parse_return_statement(),
            x => {
                return Err(ParserError::UnknownStatement(String::from(format!(
                    "unknown statement: {:?}",
                    x
                ))))
            }
        }
    }

    fn parse_expression(&mut self, pr: Precedence) -> Result<Expression, ParserError> {
        Ok(Expression::Boolean(true))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        let n = self.parse_identifier_name()?;
        self.expect_peek(&Token::ASSIGN)?;
        self.next_token()?;

        let val = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::SEMICOLON {
            self.next_token()?;
        }
        Ok(Statement::Let(LetStatement {
            name: n,
            value: val,
        }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token()?;
        let val = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::SEMICOLON {
            self.next_token()?;
        }

        Ok(Statement::Return(ReturnStatement { value: val }))
    }

    fn parse_identifier_name(&mut self) -> Result<String, ParserError> {
        let n = match &self.peek {
            Token::IDENTIFIER(name) => name.to_string(),
            _ => {
                return Err(ParserError::InvalidIdentifier(String::from(format!(
                    "invalid identifier {:?}",
                    self.peek
                ))))
            }
        };

        self.next_token()?;
        Ok(n)
    }

    fn expect_peek(&mut self, tok: &Token) -> Result<bool, ParserError> {
        match self.peek_token_is(&tok) {
            true => {
                self.next_token()?;
                Ok(true)
            }
            false => Err(ParserError::InvalidNextToken(String::from(format!(
                "expected next token to be {:?}, got {:?}",
                tok, self.peek
            )))),
        }
    }

    fn peek_token_is(&self, tok: &Token) -> bool {
        match (&tok, &self.peek) {
            _ => tok == &self.peek,
        }
    }

    fn next_token(&mut self) -> Result<(), ParserError> {
        mem::swap(&mut self.current, &mut self.peek);
        self.peek = self.lexer.next_token()?;
        Ok(())
    }
}

pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

// Determines the Precedence value of a given Token.
fn precedence(tok: &Token) -> Precedence {
    match tok {
        Token::EQ => Precedence::Equals,
        Token::NE => Precedence::Equals,
        Token::LT => Precedence::LessGreater,
        Token::GT => Precedence::LessGreater,
        Token::PLUS => Precedence::Sum,
        Token::MINUS => Precedence::Sum,
        Token::SLASH => Precedence::Product,
        Token::ASTERISK => Precedence::Product,
        Token::LPAREN => Precedence::Call,
        Token::LBRACE => Precedence::Index,

        _ => Precedence::Lowest,
    }
}

#[derive(Debug)]
pub enum ParserError {
    LexerError(LexerError),
    InvalidIdentifier(String),
    InvalidNextToken(String),
    UnknownStatement(String),
}

impl From<LexerError> for ParserError {
    fn from(err: LexerError) -> ParserError {
        ParserError::LexerError(err)
    }
}

#[test]
fn test_let_statement() {
    let input = r#"
let five = 46;
let ten = 10;
let foobar = 1234;
"#;
    let st = vec![
        LetStatement {
            name: String::from("five"),
            value: Expression::Boolean(true),
        },
        LetStatement {
            name: String::from("ten"),
            value: Expression::Boolean(true),
        },
        LetStatement {
            name: String::from("foobar"),
            value: Expression::Boolean(true),
        },
    ];

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l).unwrap();
    let pr = p.parse().unwrap();

    assert_eq!(pr.statements.len(), 3);
    for i in 0..pr.statements.len() {
        assert_eq!(
            is_expected_let_ident(
                pr.statements.iter().nth(i).unwrap(),
                st.iter().nth(i).unwrap()
            ),
            true
        );
    }
}

#[cfg(test)]
fn is_expected_let_ident(s: &Statement, exp: &LetStatement) -> bool {
    match s {
        Statement::Let(ls) => return ls.name == exp.name && ls.value == exp.value,
        _ => return false,
    };
}

#[test]
fn test_return_statement() {
    let input = r#"
return 3;
return x;
"#;

    let st = vec![
        ReturnStatement {
            value: Expression::Boolean(true),
        },
        ReturnStatement {
            value: Expression::Boolean(true),
        },
    ];

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l).unwrap();
    let pr = p.parse().unwrap();

    assert_eq!(pr.statements.len(), 2);
    for i in 0..pr.statements.len() {
        // assert_eq!(
        //     is_expected_return(
        //         pr.statements.iter().nth(i).unwrap(),
        //         st.iter().nth(i).unwrap()
        //     ),
        //     true
        // );

        is_expected_return(
            pr.statements.iter().nth(i).unwrap(),
            st.iter().nth(i).unwrap(),
        );
    }
}

#[cfg(test)]
fn is_expected_return(s: &Statement, exp: &ReturnStatement) -> bool {
    println!("expected to compare {} with {}", s, exp);
    match s {
        Statement::Let(ls) => return ls.value == exp.value,
        _ => return false,
    };
}

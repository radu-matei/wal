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
            _ => return self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParserError> {
        let exp = self.parse_expression(Precedence::Lowest)?;
        if self.peek == Token::SEMICOLON {
            self.next_token()?;
        }

        Ok(Statement::Expression(exp))
    }

    fn parse_expression(&mut self, pr: Precedence) -> Result<Expression, ParserError> {
        let mut left = self.prefix_parse()?;

        let prec_val = pr as u32;
        while self.peek != Token::SEMICOLON && prec_val < (precedence(&self.peek) as u32) {
            match self.infix_parse(&left) {
                Some(infix) => left = infix?,
                None => {
                    return Ok(left);
                }
            };
        }

        Ok(left)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token()?;

        let name = self.parse_identifier()?;
        self.expect_peek(&Token::ASSIGN)?;
        self.next_token()?;

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::SEMICOLON {
            self.next_token()?;
        }

        Ok(Statement::Let(LetStatement { name, value }))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token()?;
        let val = self.parse_expression(Precedence::Lowest)?;

        if self.peek == Token::SEMICOLON {
            self.next_token()?;
        }

        Ok(Statement::Return(ReturnStatement { value: val }))
    }

    fn parse_identifier(&mut self) -> Result<String, ParserError> {
        if let Token::IDENTIFIER(ident) = &self.current {
            Ok(ident.to_string())
        } else {
            Err(ParserError::InvalidIdentifier(String::from(format!(
                "{:?}
            ",
                self.current
            ))))
        }
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

    fn prefix_parse(&mut self) -> Result<Expression, ParserError> {
        match &self.current {
            Token::IDENTIFIER(_) => Ok(Expression::Identifier(self.parse_identifier()?)),
            Token::INTEGER(i) => Ok(Expression::Integer(*i)),
            _ => Err(ParserError::InvalidNextToken(String::from(""))),
        }
    }

    fn infix_parse(&mut self, left: &Expression) -> Option<Result<Expression, ParserError>> {
        match self.peek {
            // Token::Plus
            // | Token::Minus
            // | Token::Asterisk
            // | Token::Slash
            // | Token::Percent
            // | Token::Equal
            // | Token::NotEqual
            // | Token::LessThan
            // | Token::GreaterThan => Some(self.parse_infix_expression(left)),
            // Token::LeftParen => Some(self.parse_call_expression(left)),
            // Token::LeftBracket => Some(self.parse_index_expression(left)),
            _ => None,
        }
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
            value: Expression::Integer(46),
        },
        LetStatement {
            name: String::from("ten"),
            value: Expression::Integer(10),
        },
        LetStatement {
            name: String::from("foobar"),
            value: Expression::Integer(1234),
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
            value: Expression::Integer(3),
        },
        ReturnStatement {
            value: Expression::Identifier(String::from("x")),
        },
    ];

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l).unwrap();
    let pr = p.parse().unwrap();

    assert_eq!(pr.statements.len(), 2);
    for i in 0..pr.statements.len() {
        assert_eq!(
            is_expected_return(
                pr.statements.iter().nth(i).unwrap(),
                st.iter().nth(i).unwrap()
            ),
            true
        );
    }
}

#[cfg(test)]
fn is_expected_return(s: &Statement, exp: &ReturnStatement) -> bool {
    match s {
        Statement::Return(ls) => return ls.value == exp.value,
        _ => return false,
    };
}

#[test]
fn test_identifier_expression() {
    let input = r#"
foobar;
"#;

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l).unwrap();
    let pr = p.parse().unwrap();

    assert_eq!(pr.statements.len(), 1);
    let exp = pr.statements.iter().nth(0).unwrap();

    match exp {
        Statement::Expression(Expression::Identifier(t)) => assert_eq!(t, "foobar"),
        _ => panic!("expected identifier expression, got {:?}", exp),
    }
}

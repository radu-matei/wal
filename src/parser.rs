use crate::{
    ast::{
        BlockStatement, Expression, FunctionLiteral, IfExpression, InfixExpression, LetStatement,
        PrefixExpression, Program, ReturnStatement, Statement,
    },
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
            // TODO: fix the clone call
            match self.infix_parse(left.clone()) {
                Some(infix) => left = infix?,
                None => {
                    return Ok(left);
                }
            };
        }

        Ok(left)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, ParserError> {
        let op = self.current.clone();
        self.next_token()?;
        let exp = self.parse_expression(Precedence::Prefix)?;
        Ok(Expression::Prefix(PrefixExpression {
            operator: op,
            right: Box::new(exp),
        }))
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        self.next_token()?;
        let op = self.current.clone();
        let prec = precedence(&self.current);
        self.next_token()?;

        let right = Box::new(self.parse_expression(prec)?);
        Ok(Expression::Infix(InfixExpression {
            left: Box::new(left),
            operator: op,
            right: right,
        }))
    }

    fn parse_groupped_expression(&mut self) -> Result<Expression, ParserError> {
        self.next_token()?;
        let exp = self.parse_expression(Precedence::Lowest)?;
        if !self.expect_peek(&Token::RPAREN)? {
            return Err(ParserError::InvalidNextToken(String::from(format!(
                "expected next token to be {}, got {}",
                Token::RPAREN,
                self.current
            ))));
        }

        return Ok(exp);
    }

    fn parse_if_expression(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(&Token::LPAREN)?;
        self.next_token()?;

        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);
        self.expect_peek(&Token::RPAREN)?;
        self.expect_peek(&Token::LBRACE)?;

        let consequence = if let Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(ParserError::InvalidNextToken(String::from(format!(
                "expected {}, got {}",
                "if block statement", self.current
            ))));
        };

        if self.peek != Token::ELSE {
            return Ok(Expression::If(IfExpression {
                condition,
                consequence,
                alternative: None,
            }));
        }

        self.next_token()?;
        self.expect_peek(&Token::LBRACE)?;

        let alternative = if let Statement::Block(block) = self.parse_block_statement()? {
            Some(block)
        } else {
            return Err(ParserError::InvalidNextToken(String::from(format!(
                "expected {}, got {}",
                "if block statement", self.current
            ))));
        };

        Ok(Expression::If(IfExpression {
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_block_statement(&mut self) -> Result<Statement, ParserError> {
        self.next_token()?;

        let mut statements = vec![];

        while self.current != Token::RBRACE && self.current != Token::EOF {
            statements.push(self.parse_statement()?);
            self.next_token()?;
        }

        Ok(Statement::Block(BlockStatement { statements }))
    }

    fn parse_function_literal(&mut self) -> Result<Expression, ParserError> {
        self.expect_peek(&Token::LPAREN)?;
        let parameters = self.parse_function_params()?;
        self.expect_peek(&Token::LBRACE)?;

        let body = if let Statement::Block(block) = self.parse_block_statement()? {
            block
        } else {
            return Err(ParserError::InvalidNextToken(String::from(format!(
                "expected {}, got {}",
                "if block statement", self.current
            ))));
        };

        Ok(Expression::Function(FunctionLiteral { parameters, body }))
    }

    fn parse_function_params(&mut self) -> Result<Vec<String>, ParserError> {
        let mut params = vec![];

        if self.peek == Token::RPAREN {
            self.next_token()?;
            return Ok(params);
        }

        self.next_token()?;
        params.push(self.parse_identifier()?);

        while self.peek == Token::COMMA {
            self.next_token()?;
            self.next_token()?;

            params.push(self.parse_identifier()?);
        }

        Ok(params)
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
            Token::TRUE => Ok(Expression::Boolean(true)),
            Token::FALSE => Ok(Expression::Boolean(false)),
            Token::LPAREN => self.parse_groupped_expression(),
            Token::IF => self.parse_if_expression(),
            Token::FUNCTION => self.parse_function_literal(),
            Token::BANG | Token::MINUS => self.parse_prefix_expression(),
            _ => Err(ParserError::InvalidNextToken(String::from(format!(
                "{:?}",
                self.current
            )))),
        }
    }

    fn infix_parse(&mut self, left: Expression) -> Option<Result<Expression, ParserError>> {
        match self.peek {
            Token::PLUS
            | Token::MINUS
            | Token::ASTERISK
            | Token::SLASH
            | Token::EQ
            | Token::NE
            | Token::LT
            | Token::GT => Some(self.parse_infix_expression(left)),
            // Token::LPAREN => Some(self.parse_call_expression(left)),
            // Token::LBRACKET => Some(self.parse_index_expression(left)),
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

#[test]
fn test_prefix_expression() {
    let input = r#"
!5;
-15;
!true;
!false;
-x;
"#;

    let exp = vec![
        PrefixExpression {
            operator: Token::BANG,
            right: Box::new(Expression::Integer(5)),
        },
        PrefixExpression {
            operator: Token::MINUS,
            right: Box::new(Expression::Integer(15)),
        },
        PrefixExpression {
            operator: Token::BANG,
            right: Box::new(Expression::Boolean(true)),
        },
        PrefixExpression {
            operator: Token::BANG,
            right: Box::new(Expression::Boolean(false)),
        },
        PrefixExpression {
            operator: Token::MINUS,
            right: Box::new(Expression::Identifier(String::from("x"))),
        },
    ];

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l).unwrap();
    let pr = p.parse().unwrap();

    for i in 0..pr.statements.len() {
        assert_eq!(
            is_expected_prefix_expression(
                pr.statements.iter().nth(i).unwrap(),
                exp.iter().nth(i).unwrap()
            ),
            true
        );
    }
}

#[cfg(test)]
fn is_expected_prefix_expression(s: &Statement, exp: &PrefixExpression) -> bool {
    match s {
        Statement::Expression(Expression::Prefix(es)) => {
            println!("expected {:?} to equal {:?}", es, exp);
            return es.operator == exp.operator && es.right == exp.right;
        }
        _ => return false,
    };
}

#[test]
fn test_infix_expression() {
    let input = r#"
5+5;
5-5;
5*5;
5/5;
5>5;
5<5;
5==5;
5!=5;
"#;

    let exp = vec![
        InfixExpression {
            left: Box::new(Expression::Integer(5)),
            operator: Token::PLUS,
            right: Box::new(Expression::Integer(5)),
        },
        InfixExpression {
            left: Box::new(Expression::Integer(5)),
            operator: Token::MINUS,
            right: Box::new(Expression::Integer(5)),
        },
        InfixExpression {
            left: Box::new(Expression::Integer(5)),
            operator: Token::ASTERISK,
            right: Box::new(Expression::Integer(5)),
        },
        InfixExpression {
            left: Box::new(Expression::Integer(5)),
            operator: Token::SLASH,
            right: Box::new(Expression::Integer(5)),
        },
        InfixExpression {
            left: Box::new(Expression::Integer(5)),
            operator: Token::GT,
            right: Box::new(Expression::Integer(5)),
        },
        InfixExpression {
            left: Box::new(Expression::Integer(5)),
            operator: Token::LT,
            right: Box::new(Expression::Integer(5)),
        },
        InfixExpression {
            left: Box::new(Expression::Integer(5)),
            operator: Token::EQ,
            right: Box::new(Expression::Integer(5)),
        },
        InfixExpression {
            left: Box::new(Expression::Integer(5)),
            operator: Token::NE,
            right: Box::new(Expression::Integer(5)),
        },
    ];

    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l).unwrap();
    let pr = p.parse().unwrap();

    for i in 0..pr.statements.len() {
        assert_eq!(
            is_expected_infix_expression(
                pr.statements.iter().nth(i).unwrap(),
                exp.iter().nth(i).unwrap()
            ),
            true
        );
    }
}

#[cfg(test)]
fn is_expected_infix_expression(s: &Statement, exp: &InfixExpression) -> bool {
    match s {
        Statement::Expression(Expression::Infix(ie)) => {
            println!("expected {:?} to equal {:?}", ie, exp);
            return ie.right == exp.right && ie.operator == exp.operator && ie.right == exp.right;
        }
        _ => return false,
    };
}

#[test]
fn test_operator_precedence() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        // (
        //     "a * [1, 2, 3, 4][b * c] * d",
        //     "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        // ),
        // (
        //     "add(a * b[2], b[1], 2 * [1, 2][1])",
        //     "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        // ),
    ];

    for (input, want) in tests {
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l).unwrap();
        let got = format!("{}", p.parse().unwrap());

        assert_eq!(want, got);
    }
}

#[test]
fn parse_if_expression() {
    let tests = vec!["if (x < y) { x }", "if (x > y) { x } else { y }"];

    for input in tests {
        let l = Lexer::new(input).unwrap();
        let mut p = Parser::new(l).unwrap();
        let got = format!("{}", p.parse().unwrap());

        assert_eq!(input, got);
    }
}

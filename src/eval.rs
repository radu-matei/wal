use crate::{
    ast::{Expression, InfixExpression, Node, PrefixExpression, Program, Statement},
    lexer::Lexer,
    object::{Environment, Object},
    parser::Parser,
    token::Token,
};

pub fn eval(node: Node, env: &mut Environment) -> Result<Object, EvaluatorError> {
    match node {
        Node::Program(p) => eval_program(p, env),

        Node::Statement(stmt) => match stmt {
            Statement::Expression(expr) => eval(Node::Expression(expr), env),
            Statement::Let(ls) => {
                let obj = eval(Node::Expression(ls.value), env)?;
                env.set(ls.name, &obj);
                Ok(obj)
            },
            _ => Err(EvaluatorError::InvalidOperation(format!(
                "cannot handle statement {}",
                stmt
            ))),
        },

        Node::Expression(expr) => match expr {
            Expression::Integer(i) => Ok(Object::Integer(i)),
            Expression::Boolean(b) => Ok(Object::Boolean(b)),
            Expression::String(s) => Ok(Object::String(s)),
            Expression::Prefix(p) => eval_prefix_expression(p, env),
            Expression::Infix(i) => eval_infix_expression(i, env),
            Expression::Identifier(id) => eval_identifier(id, env),
            _ => Err(EvaluatorError::InvalidOperation(format!(
                "cannot handle expression {}",
                expr
            ))),
        },
    }
}

pub fn eval_program(prog: Program, env: &mut Environment) -> Result<Object, EvaluatorError> {
    let mut result = Object::Null;

    for stmt in prog.statements {
        result = eval(Node::Statement(stmt.clone()), env)?;
    }

    Ok(result)
}

fn eval_prefix_expression(
    expr: PrefixExpression,
    env: &mut Environment,
) -> Result<Object, EvaluatorError> {
    let right = &eval(Node::Expression(*expr.right), env)?;

    match expr.operator {
        Token::BANG => match right {
            Object::Boolean(b) => Ok(Object::Boolean(!b)),
            _ => Err(EvaluatorError::InvalidOperation(format!(
                "cannot negate non-boolean value {}",
                right
            ))),
        },
        Token::MINUS => match right {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            _ => Err(EvaluatorError::InvalidOperation(format!(
                "cannot negate non-numeric value {}",
                right
            ))),
        },

        x => Err(EvaluatorError::InvalidOperation(format!(
            "cannot handle prefix operation {} {}",
            x, right
        ))),
    }
}

fn eval_infix_expression(
    expr: InfixExpression,
    env: &mut Environment,
) -> Result<Object, EvaluatorError> {
    let left = &eval(Node::Expression(*expr.left), env)?;
    let right = &eval(Node::Expression(*expr.right), env)?;

    match (left, right) {
        (Object::Integer(l), Object::Integer(r)) => match expr.operator {
            Token::PLUS | Token::MINUS | Token::ASTERISK | Token::SLASH => {
                return Ok(Object::Integer(eval_infix_op(expr.operator, *l, *r)?))
            }
            Token::LT => return Ok(Object::Boolean(l < r)),
            Token::GT => return Ok(Object::Boolean(l > r)),
            Token::EQ => return Ok(Object::Boolean(l == r)),
            Token::NE => return Ok(Object::Boolean(l != r)),
            _ => {
                return Err(EvaluatorError::InvalidOperation(format!(
                    "cannot handle operation {} {} {}",
                    l, expr.operator, r
                )))
            }
        },
        (Object::Boolean(l), Object::Boolean(r)) => match expr.operator {
            Token::EQ => return Ok(Object::Boolean(l == r)),
            Token::NE => return Ok(Object::Boolean(l != r)),
            _ => {
                return Err(EvaluatorError::InvalidOperation(format!(
                    "cannot handle operation {} {} {}",
                    l, expr.operator, r
                )))
            }
        },
        (Object::String(l), Object::String(r)) => match expr.operator {
            Token::PLUS => return Ok(Object::String(String::from(format!("{}{}", l, r)))),
            _ => {
                return Err(EvaluatorError::InvalidOperation(format!(
                    "cannot handle operation {} {} {}",
                    l, expr.operator, r
                )))
            }
        },

        _ => {
            return Err(EvaluatorError::InvalidOperation(format!(
                "cannot handle operation {} {} {}",
                left, expr.operator, right
            )))
        }
    }
}

fn eval_identifier(id: String, env: &mut Environment) -> Result<Object, EvaluatorError> {
    Ok(env
        .get(&id)
        .ok_or_else(|| EvaluatorError::UnknownIdentifier(id))?
        .clone())
}

fn eval_infix_op(op: Token, l: i64, r: i64) -> Result<i64, EvaluatorError> {
    match op {
        Token::PLUS => Ok(l + r),
        Token::MINUS => Ok(l - r),
        Token::ASTERISK => Ok(l * r),
        Token::SLASH => Ok(l / r),

        _ => Err(EvaluatorError::InvalidOperation(format!(
            "cannot execute {} {} {}",
            l, op, r
        ))),
    }
}

#[derive(Debug)]
pub enum EvaluatorError {
    InvalidOperation(String),
    UnknownIdentifier(String)
}

#[test]
fn evaluate_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 *2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
    ];

    for (input, want) in tests {
        let n = parse_input(input);
        let got = if let Object::Integer(int) = eval(n, &mut Environment::new()).unwrap() {
            int
        } else {
            panic!("not an integer object");
        };

        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("!true", false),
        ("!false", true),
    ];

    for (input, want) in tests {
        let n = parse_input(input);
        let got = if let Object::Boolean(b) = eval(n, &mut Environment::new()).unwrap() {
            b
        } else {
            panic!("not an integer object");
        };

        assert_eq!(want, got);
    }
}

#[cfg(test)]
fn parse_input(input: &str) -> Node {
    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l).unwrap();

    Node::Program(p.parse().unwrap())
}

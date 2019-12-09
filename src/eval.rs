use crate::{
    ast::{Expression, Node, PrefixExpression, Program, Statement},
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
            _ => Err(EvaluatorError::InvalidOperation(format!(
                "cannot handle expression {}",
                expr
            ))),        },
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
            _ => Err(EvaluatorError::InvalidOperation(
                format! {"cannot negate non-numeric value {}", right},
            )),
        },

        x => Err(EvaluatorError::InvalidOperation(
            format! {"cannot handle prefix operation {} {}", x, right},
        )),
    }
}

#[derive(Debug)]
pub enum EvaluatorError {
    InvalidOperation(String),
}

#[test]
fn evaluate_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        // ("5 + 5 + 5 + 5 - 10", 10),
        // ("2 * 2 * 2 *2 * 2", 32),
        // ("-50 + 100 + -50", 0),
        // ("50 / 2 * 2 + 10", 60),
        // ("2 * (5 + 10)", 30),
        // ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        // ("4 % 3", 1),
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

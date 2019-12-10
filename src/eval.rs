use crate::{
    ast::{
        BlockStatement, Expression, IfExpression, InfixExpression, Node, PrefixExpression, Program,
        Statement,
    },
    lexer::Lexer,
    object::{Environment, Function, Object},
    parser::Parser,
    token::Token,
};

pub fn eval(node: Node, env: &mut Environment) -> Result<Object, EvaluatorError> {
    match node {
        Node::Program(p) => eval_program(p, env),

        Node::Statement(stmt) => match stmt {
            Statement::Expression(expr) => eval(Node::Expression(expr), env),
            Statement::Block(block) => eval_block_statement(block, env),
            Statement::Let(ls) => {
                let obj = eval(Node::Expression(ls.value), env)?;
                env.set(ls.name, &obj);
                Ok(obj)
            }
            Statement::Return(rs) => Ok(Object::ReturnValue(Box::new(eval(
                Node::Expression(rs.value),
                env,
            )?))),
        },

        Node::Expression(expr) => match expr {
            Expression::Integer(i) => Ok(Object::Integer(i)),
            Expression::Boolean(b) => Ok(Object::Boolean(b)),
            Expression::String(s) => Ok(Object::String(s)),
            Expression::Prefix(p) => eval_prefix_expression(p, env),
            Expression::Infix(i) => eval_infix_expression(i, env),
            Expression::Identifier(id) => eval_identifier(id, env),
            Expression::If(ie) => eval_if_expression(ie, env),
            Expression::Function(func) => Ok(Object::Function(Function {
                parameters: func.parameters,
                body: func.body,
                env: env.clone(),
            })),

            Expression::Call(call) => {
                let func = eval(Node::Expression(*call.function), env)?;
                let args = eval_expressions(call.arguments, env)?;

                // TODO - function closures DO NOT WORK yet.
                let function = match func {
                    Object::Function(f) => Function {
                        parameters: f.parameters,
                        body: f.body,
                        env: Environment::new_enclosed(env.clone()),
                    },

                    _ => {
                        return Err(EvaluatorError::InvalidOperation(format!(
                            "cannot apply function call",
                        )));
                    }
                };

                apply_function(function, &args)
            }
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

fn eval_expressions(
    expressions: Vec<Expression>,
    env: &mut Environment,
) -> Result<Vec<Object>, EvaluatorError> {
    let mut results = vec![];

    for expr in expressions {
        results.push(eval(Node::Expression(expr), env)?);
    }

    Ok(results)
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
                "cannot negate non-boolean value {:?}",
                right
            ))),
        },
        Token::MINUS => match right {
            Object::Integer(i) => Ok(Object::Integer(-i)),
            _ => Err(EvaluatorError::InvalidOperation(format!(
                "cannot negate non-numeric value {:?}",
                right
            ))),
        },

        x => Err(EvaluatorError::InvalidOperation(format!(
            "cannot handle prefix operation {:?} {:?}",
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
                    "cannot handle operation {:?} {:?} {:?}",
                    l, expr.operator, r
                )))
            }
        },
        (Object::Boolean(l), Object::Boolean(r)) => match expr.operator {
            Token::EQ => return Ok(Object::Boolean(l == r)),
            Token::NE => return Ok(Object::Boolean(l != r)),
            _ => {
                return Err(EvaluatorError::InvalidOperation(format!(
                    "cannot handle operation {:?} {:?} {:?}",
                    l, expr.operator, r
                )))
            }
        },
        (Object::String(l), Object::String(r)) => match expr.operator {
            Token::PLUS => return Ok(Object::String(String::from(format!("{}{}", l, r)))),
            _ => {
                return Err(EvaluatorError::InvalidOperation(format!(
                    "cannot handle operation {:?} {:?} {:?}",
                    l, expr.operator, r
                )))
            }
        },

        _ => {
            return Err(EvaluatorError::InvalidOperation(format!(
                "cannot handle operation {:?} {:?} {:?}",
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

fn eval_if_expression(expr: IfExpression, env: &mut Environment) -> Result<Object, EvaluatorError> {
    let cond = eval(Node::Expression(*expr.condition), env)?;

    if is_truthy(&cond) {
        eval(Node::Statement(Statement::Block(expr.consequence)), env)
    } else if let Some(alt) = expr.alternative {
        eval(Node::Statement(Statement::Block(alt)), env)
    } else {
        Ok(Object::Null)
    }
}

fn eval_block_statement(
    block: BlockStatement,
    env: &mut Environment,
) -> Result<Object, EvaluatorError> {
    let mut result = Object::Null;
    for stmt in block.statements {
        result = eval(Node::Statement(stmt.clone()), env)?;

        if let Object::ReturnValue(_) = result {
            return Ok(result);
        }
    }

    Ok(result)
}

fn apply_function(function: Function, args: &[Object]) -> Result<Object, EvaluatorError> {
    let mut extended_env = extend_env(&function, &args)?;
    let evaluated = eval(
        Node::Statement(Statement::Block(function.body)),
        &mut extended_env,
    )?;

    if let Object::ReturnValue(ret) = evaluated {
        Ok(*ret)
    } else {
        Ok(evaluated)
    }
}

fn extend_env(func: &Function, args: &[Object]) -> Result<Environment, EvaluatorError> {
    if func.parameters.len() != args.len() {
        return Err(EvaluatorError::InvalidOperation(format!(
            "expected {:?} params to call function, got {}",
            func.parameters.len(),
            args.len()
        )));
    }
    let mut env = Environment::new_enclosed(func.env.clone());

    for (i, param) in func.parameters.iter().enumerate() {
        env.set(param.to_string(), &args[i]);
    }

    Ok(env)
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        Object::Boolean(false) | Object::Null => false,
        Object::Boolean(true) | _ => true,
    }
}

fn eval_infix_op(op: Token, l: i64, r: i64) -> Result<i64, EvaluatorError> {
    match op {
        Token::PLUS => Ok(l + r),
        Token::MINUS => Ok(l - r),
        Token::ASTERISK => Ok(l * r),
        Token::SLASH => Ok(l / r),

        _ => Err(EvaluatorError::InvalidOperation(format!(
            "cannot execute {:?} {:?} {:?}",
            l, op, r
        ))),
    }
}

#[derive(Debug)]
pub enum EvaluatorError {
    InvalidOperation(String),
    UnknownIdentifier(String),
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

#[test]
fn evaluate_eq_expression() {
    let tests = vec![("3==4", false), ("3==4", !true), ("3==3", true)];

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

#[test]
fn evaluate_if_expression() {
    let ten = Object::Integer(10);
    let twenty = Object::Integer(20);
    let null = Object::Null;

    let tests = vec![
        ("if (true) { 10 }", &ten),
        ("if (false) { 10 }", &null),
        ("if (1) { 10 }", &ten),
        ("if (1 < 2) { 10 }", &ten),
        ("if (1 > 2) { 10 }", &null),
        ("if (1 < 2) { 10 } else { 20 }", &ten),
        ("if (1 > 2) { 10 } else { 20 }", &twenty),
    ];
    for (input, want) in tests {
        let n = parse_input(input);
        let got = &eval(n, &mut Environment::new()).unwrap();
        assert_eq!(want, got);
    }
}

#[test]
fn evaluate_return_expression() {
    let ten = Object::ReturnValue(Box::new(Object::Integer(10)));

    let tests = vec![
        "return 10;",
        "return 2*5;",
        "if (10 > 1) {
            if (10 > 1) {
                return 10;
            }
            return 1;
        }",
    ];

    for input in tests {
        let n = parse_input(input);
        let got = eval(n, &mut Environment::new()).unwrap();
        assert_eq!(ten, got);
    }
}

#[test]
fn evaluate_let_statement() {
    let tests = vec![
        ("let a = 5; a;", Object::Integer(5)),
        ("let a = 5 * 5; a;", Object::Integer(25)),
        ("let a = 5; let b = a; b;", Object::Integer(5)),
        (
            "let a = 5; let b = a; let c = a + b + 5; c",
            Object::Integer(15),
        ),
    ];

    for (input, want) in tests {
        let n = parse_input(input);
        let got = eval(n, &mut Environment::new()).unwrap();
        assert_eq!(want, got);
    }
}

#[cfg(test)]
fn parse_input(input: &str) -> Node {
    let l = Lexer::new(input).unwrap();
    let mut p = Parser::new(l).unwrap();

    Node::Program(p.parse().unwrap())
}

#[test]
fn evaluate_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; } add(5, 5);", 10),
        ("fn(x) { x; }(5)", 5),
        // And higher-order functions!
        (
            "
let add = fn(x, y) { x + y };
let apply = fn(func, x, y) { func(x, y) };
apply(add, 2, 2);
",
            4,
        ),
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

use crate::{
    ast::{Expression, Node, Program, Statement},
    lexer::Lexer,
    object::Object,
    parser::Parser,
};

pub fn eval(node: Node) -> Result<Object, EvaluatorError> {
    match node {
        Node::Program(p) => eval_program(p),
        Node::Statement(stmt) => match stmt {
            Statement::Expression(expr) => match expr {
                Expression::Integer(i) => Ok(Object::Integer(i)),
                _ => panic!("cannot handle expression {}", expr),
            },
            _ => panic!("cannot handle statement {}", stmt),
        },
        x => {
            println!("{:?}", x);
            panic!("cannot evaluate 1")
        }
    }
}

fn eval_program(prog: Program) -> Result<Object, EvaluatorError> {
    let mut result = Object::Null;

    for stmt in prog.statements {
        result = eval(Node::Statement(stmt.clone()))?;
    }

    Ok(result)
}

#[derive(Debug)]
pub enum EvaluatorError {}

#[test]
fn evaluate_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        // ("-5", -5),
        // ("-10", -10),
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
        let got = if let Object::Integer(int) = eval(n).unwrap() {
            int
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

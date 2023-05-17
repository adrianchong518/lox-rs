use std::fmt;

use error_stack::ResultExt;

use crate::{ast, object, token};

macro_rules! binary_op {
    ($left:ident $op:tt $right:ident, $op_token:expr, $op_str:literal) => {{
        let object::Object::Number(left) = $left else {
            return Err(error_stack::report!(RuntimeError::new($op_token))
                .attach_printable(
                    format!(concat!("cannot apply binary `", $op_str, "` to {:?} on the left"), $left),
                )
                .attach_printable(concat!("operands to binary `", $op_str, "` must be numbers")));
        };
        let object::Object::Number(right) = $right else {
            return Err(error_stack::report!(RuntimeError::new($op_token))
                .attach_printable(
                    format!(concat!("cannot apply binary `", $op_str, "` to {:?} on the right"), $right),
                )
                .attach_printable(concat!("operands to binary `", $op_str, "` must be numbers")));
        };

        Ok((left $op right).into())
    }};
}

pub struct Interpreter;

#[derive(Debug)]
pub struct RuntimeError {
    token: token::Token<'static>,
}

impl RuntimeError {
    fn new(token: token::Token<'_>) -> Self {
        Self {
            token: token.into_owned(),
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] runtime error", self.token.line)
    }
}

impl error_stack::Context for RuntimeError {}

impl Interpreter {
    pub fn interprete(&mut self, expression: &ast::Expr) -> error_stack::Result<(), RuntimeError> {
        let result = self.evaluate(expression)?;
        println!("{result}");
        Ok(())
    }

    fn evaluate(
        &mut self,
        expression: &ast::Expr,
    ) -> error_stack::Result<object::Object, RuntimeError> {
        expression.accept(self)
    }
}

impl ast::expr::Visitor for Interpreter {
    type Output = error_stack::Result<object::Object, RuntimeError>;

    fn visit_binary(&mut self, v: &ast::expr::Binary<'_>) -> Self::Output {
        let left = self.evaluate(&v.left)?;
        let right = self.evaluate(&v.right)?;

        match v.operator.typ {
            token::Type::Plus => match (&left, &right) {
                (object::Object::Number(l), object::Object::Number(r)) => Ok((l + r).into()),

                (object::Object::String(l), object::Object::String(r)) => {
                    Ok(format!("{l}{r}").into())
                }

                _ => Err(error_stack::report!(RuntimeError::new(v.operator.clone()))
                    .attach_printable(format!("cannot apply binary `+` to {left:?} and {right:?}"))
                    .attach_printable(
                        "operands to binary `+` must either be both numbers or both strings",
                    )),
            },

            token::Type::Minus => binary_op!(left - right, v.operator.clone(), '-'),
            token::Type::Slash => binary_op!(left / right, v.operator.clone(), '/'),
            token::Type::Star => binary_op!(left * right, v.operator.clone(), '*'),

            token::Type::Greater => binary_op!(left > right, v.operator.clone(), '>'),
            token::Type::GreaterEqual => binary_op!(left >= right, v.operator.clone(), '>'),
            token::Type::Less => binary_op!(left < right, v.operator.clone(), '>'),
            token::Type::LessEqual => binary_op!(left <= right, v.operator.clone(), '>'),

            token::Type::EqualEqual => Ok((left == right).into()),
            token::Type::BangEqual => Ok((left != right).into()),

            _ => unreachable!(),
        }
    }

    fn visit_grouping(&mut self, v: &ast::expr::Grouping<'_>) -> Self::Output {
        self.evaluate(&v.expression)
    }

    fn visit_literal(&mut self, v: &ast::expr::Literal<'_>) -> Self::Output {
        Ok(object::Object::from(v.literal.typ.clone()))
    }

    fn visit_unary(&mut self, v: &ast::expr::Unary<'_>) -> Self::Output {
        let right = self.evaluate(&v.right)?;

        match &v.operator.typ {
            token::Type::Minus => {
                if let object::Object::Number(num) = right {
                    Ok((-num).into())
                } else {
                    Err(error_stack::report!(RuntimeError::new(v.operator.clone())))
                        .attach_printable_lazy(|| format!("cannot apply unary `-` to {right:?}"))
                }
            }

            token::Type::Bang => Ok((!right.is_truthy()).into()),

            _ => unreachable!(),
        }
    }
}

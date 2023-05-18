mod env;

use std::fmt;

use error_stack::ResultExt;

use crate::{ast, object, token};

macro_rules! binary_op {
    ($left:ident $op:tt $right:ident, $info:expr, $op_str:literal) => {{
        let object::Object::Number(left) = $left else {
            return Err(error_stack::report!(RuntimeError { info: $info.clone().into_owned() })
                .attach_printable(
                    format!(concat!("cannot apply binary `", $op_str, "` to {:?} on the left"), $left),
                )
                .attach_printable(concat!("operands to binary `", $op_str, "` must be numbers")));
        };
        let object::Object::Number(right) = $right else {
            return Err(error_stack::report!(RuntimeError { info: $info.clone().into_owned() })
                .attach_printable(
                    format!(concat!("cannot apply binary `", $op_str, "` to {:?} on the right"), $right),
                )
                .attach_printable(concat!("operands to binary `", $op_str, "` must be numbers")));
        };

        Ok((left $op right).into())
    }};
}

pub struct Interpreter {
    environment: env::Environment,
}

#[derive(Debug)]
pub struct RuntimeError {
    info: token::Info<'static>,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] runtime error", self.info.line)
    }
}

impl error_stack::Context for RuntimeError {}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: env::Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: &[ast::Stmt]) -> error_stack::Result<(), RuntimeError> {
        for statement in statements {
            self.execute(statement)?;
        }

        Ok(())
    }

    fn execute(&mut self, statement: &ast::Stmt) -> error_stack::Result<(), RuntimeError> {
        statement.accept(self)
    }

    fn execute_block(&mut self, statements: &[ast::Stmt]) -> error_stack::Result<(), RuntimeError> {
        self.environment.push_new_context();

        let result = statements.iter().try_for_each(|stmt| self.execute(stmt));

        self.environment.pop_context();

        result
    }

    fn evaluate(
        &mut self,
        expression: &ast::Expr,
    ) -> error_stack::Result<object::Object, RuntimeError> {
        expression.accept(self)
    }
}

impl ast::expr::Visitor<'_> for &mut Interpreter {
    type Output = error_stack::Result<object::Object, RuntimeError>;

    fn visit_binary(self, v: &ast::expr::Binary) -> Self::Output {
        let left = self.evaluate(&v.left)?;
        let right = self.evaluate(&v.right)?;

        match v.operator.typ {
            token::Type::Plus => match (&left, &right) {
                (object::Object::Number(l), object::Object::Number(r)) => Ok((l + r).into()),

                (l, r)
                    if matches!(l, object::Object::String(_))
                        || matches!(r, object::Object::String(_)) =>
                {
                    Ok(format!("{l}{r}").into())
                }

                _ => Err(error_stack::report!(RuntimeError {
                    info: v.operator.info.clone().into_owned()
                })
                .attach_printable(format!("cannot apply binary `+` to {left:?} and {right:?}"))
                .attach_printable(
                    "operands to binary `+` must either be both numbers or both strings",
                )),
            },

            token::Type::Minus => binary_op!(left - right, v.operator.info, '-'),
            token::Type::Slash => binary_op!(left / right, v.operator.info, '/'),
            token::Type::Star => binary_op!(left * right, v.operator.info, '*'),

            token::Type::Greater => binary_op!(left > right, v.operator.info, '>'),
            token::Type::GreaterEqual => binary_op!(left >= right, v.operator.info, '>'),
            token::Type::Less => binary_op!(left < right, v.operator.info, '>'),
            token::Type::LessEqual => binary_op!(left <= right, v.operator.info, '>'),

            token::Type::EqualEqual => Ok((left == right).into()),
            token::Type::BangEqual => Ok((left != right).into()),

            _ => unreachable!(),
        }
    }

    fn visit_grouping(self, v: &ast::expr::Grouping) -> Self::Output {
        self.evaluate(&v.expression)
    }

    fn visit_literal(self, v: &ast::expr::Literal) -> Self::Output {
        Ok(object::Object::from(v.literal.typ.clone()))
    }

    fn visit_unary(self, v: &ast::expr::Unary) -> Self::Output {
        let right = self.evaluate(&v.right)?;

        match &v.operator.typ {
            token::Type::Minus => {
                if let object::Object::Number(num) = right {
                    Ok((-num).into())
                } else {
                    Err(error_stack::report!(RuntimeError {
                        info: v.operator.info.clone().into_owned()
                    }))
                    .attach_printable_lazy(|| format!("cannot apply unary `-` to {right:?}"))
                }
            }

            token::Type::Bang => Ok((!right.is_truthy()).into()),

            _ => unreachable!(),
        }
    }

    fn visit_variable(self, v: &ast::expr::Variable) -> Self::Output {
        let value = self.environment.get(&v.info).ok_or_else(|| {
            error_stack::report!(RuntimeError {
                info: v.info.clone().into_owned()
            })
            .attach_printable(format!("undefined variable `{}`", v.info.lexeme))
        })?;

        Ok(value.clone())
    }

    fn visit_assign(self, v: &ast::expr::Assign) -> Self::Output {
        let value = self.evaluate(&v.value)?;
        self.environment
            .assign(&v.info, value.clone())
            .change_context(RuntimeError {
                info: v.info.clone().into_owned(),
            })?;

        Ok(value)
    }
}

impl ast::stmt::Visitor<'_> for &mut Interpreter {
    type Output = error_stack::Result<(), RuntimeError>;

    fn visit_expr(self, v: &ast::stmt::Expression) -> Self::Output {
        self.evaluate(&v.expression)?;
        Ok(())
    }

    fn visit_print(self, v: &ast::stmt::Print) -> Self::Output {
        let value = self.evaluate(&v.expression)?;
        println!("{value}");
        Ok(())
    }

    fn visit_var(self, v: &ast::stmt::Var) -> Self::Output {
        let value = v
            .initializer
            .as_ref()
            .map_or(Ok(object::Object::Nil), |init| self.evaluate(init))?;

        self.environment.define(v.info.clone(), value);

        Ok(())
    }

    fn visit_block(self, v: &ast::stmt::Block) -> Self::Output {
        self.execute_block(&v.statements)
    }
}

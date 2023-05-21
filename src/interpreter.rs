use std::fmt;

use error_stack::ResultExt as _;

use crate::{env, resolver};

use crate::{
    ast,
    into_owned::IntoOwned as _,
    object::{self, callable::Callable as _},
    token,
};

macro_rules! binary_op {
    ($left:ident $op:tt $right:ident, $info:expr, $op_str:literal) => {{
        let object::Object::Number(left) = $left else {
            return Err(RuntimeErrorState::Error(
                error_stack::report!(RuntimeError::new($info.clone().into_owned()))
                    .attach_printable(
                        format!(concat!("cannot apply binary `", $op_str, "` to {:?} on the left"), $left),
                    )
                    .attach_printable(concat!("operands to binary `", $op_str, "` must be numbers"))
            ));
        };
        let object::Object::Number(right) = $right else {
            return Err(RuntimeErrorState::Error(
                error_stack::report!(RuntimeError::new($info.clone().into_owned()))
                    .attach_printable(
                        format!(concat!("cannot apply binary `", $op_str, "` to {:?} on the right"), $right),
                    )
                    .attach_printable(concat!("operands to binary `", $op_str, "` must be numbers"))
            ));
        };

        Ok((left $op right).into())
    }};
}

#[derive(Debug)]
pub struct RuntimeError {
    pub info: token::Info<'static>,
}

impl RuntimeError {
    pub fn new(info: token::Info<'static>) -> Self {
        Self { info }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] runtime error", self.info.line)
    }
}

impl error_stack::Context for RuntimeError {}

#[derive(Debug)]
pub enum RuntimeErrorState {
    Error(error_stack::Report<RuntimeError>),
    Break(token::Info<'static>),
    Return(object::Object, token::Info<'static>),
}

impl RuntimeErrorState {
    pub fn into_error(self) -> Self {
        Self::Error(self.into())
    }
}

impl From<error_stack::Report<RuntimeError>> for RuntimeErrorState {
    fn from(value: error_stack::Report<RuntimeError>) -> Self {
        Self::Error(value)
    }
}

impl From<RuntimeErrorState> for error_stack::Report<RuntimeError> {
    fn from(value: RuntimeErrorState) -> Self {
        match value {
            RuntimeErrorState::Error(e) => e,
            RuntimeErrorState::Break(info) => error_stack::report!(RuntimeError { info })
                .attach_printable("`break` not inside any loop"),
            RuntimeErrorState::Return(_, info) => error_stack::report!(RuntimeError { info })
                .attach_printable("`return` not inside any function"),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter<'s> {
    environment: env::Environment,
    resolve_map: resolver::ResolveMap<'s>,
}

impl<'s> Interpreter<'s> {
    pub fn new() -> Self {
        Self {
            environment: env::Environment::new(),
            resolve_map: resolver::ResolveMap::new(),
        }
    }

    pub fn interpret(
        &mut self,
        statements: &[ast::Stmt<'_>],
    ) -> error_stack::Result<(), RuntimeError> {
        for statement in statements {
            self.execute(statement)?
        }

        Ok(())
    }

    pub fn repl_evaluate(
        &mut self,
        expression: &ast::Expr<'_>,
    ) -> error_stack::Result<object::Object, RuntimeError> {
        self.evaluate(expression).map_err(Into::into)
    }

    pub fn execute(&mut self, statement: &ast::Stmt<'_>) -> Result<(), RuntimeErrorState> {
        statement.accept(self)
    }

    fn execute_block(&mut self, block: &ast::stmt::Block<'_>) -> Result<(), RuntimeErrorState> {
        block
            .statements
            .iter()
            .try_for_each(|stmt| self.execute(stmt))
    }

    pub fn execute_block_push_context(
        &mut self,
        block: &ast::stmt::Block<'_>,
        context: env::ContextRef,
    ) -> Result<(), RuntimeErrorState> {
        self.environment.push_context(context);

        let result = self.execute_block(block);

        self.environment.pop_context();
        result
    }

    pub fn execute_block_swap_context(
        &mut self,
        block: &ast::stmt::Block<'_>,
        context: env::ContextRef,
    ) -> Result<(), RuntimeErrorState> {
        let old_context = self.environment.swap_context(Some(context));

        let result = self.execute_block(block);

        self.environment.swap_context(old_context);

        result
    }

    pub fn evaluate(
        &mut self,
        expression: &ast::Expr<'_>,
    ) -> Result<object::Object, RuntimeErrorState> {
        expression.accept(self)
    }

    pub fn resolve(&mut self, name: token::Info<'s>, location: resolver::VariableLocation) {
        self.resolve_map.insert(name, location);
    }

    fn lookup_variable(&self, name: &token::Info<'_>) -> Option<object::Object> {
        match self.resolve_map.get(name) {
            Some(location) => self.environment.get_at(location),
            None => self.environment.get_global(&name.lexeme),
        }
    }
}

impl<'s> Default for Interpreter<'s> {
    fn default() -> Self {
        Self::new()
    }
}

impl ast::expr::Visitor<'_> for &mut Interpreter<'_> {
    type Output = Result<object::Object, RuntimeErrorState>;

    fn visit_assign(self, v: &ast::expr::Assign<'_>) -> Self::Output {
        let value = self.evaluate(&v.value)?;

        match self.resolve_map.get(&v.info) {
            Some(location) => self.environment.assign_at(location, value.clone()),
            None => self
                .environment
                .assign_global(&v.info.lexeme, value.clone()),
        }
        .change_context(RuntimeError::new(v.info.clone().into_owned()))?;

        Ok(value)
    }

    fn visit_binary(self, v: &ast::expr::Binary<'_>) -> Self::Output {
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

                _ => Err(error_stack::report!(RuntimeError::new(
                    v.operator.info.clone().into_owned()
                ))
                .attach_printable(format!("cannot apply binary `+` to {left:?} and {right:?}"))
                .attach_printable(
                    "operands to binary `+` must either be both numbers or both strings",
                )
                .into()),
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

    fn visit_call(self, v: &ast::expr::Call<'_>) -> Self::Output {
        let callee = self.evaluate(&v.callee)?;

        let arguments = v
            .arguments
            .iter()
            .map(|arg| self.evaluate(arg))
            .collect::<Result<Vec<_>, _>>()?;

        if let object::Object::Callable(callable) = callee {
            if callable.arity() != arguments.len() {
                Err(error_stack::report!(RuntimeError::new(
                    v.right_paren.info.clone().into_owned()
                ))
                .attach_printable(format!(
                    "expected {} arguments but got {}",
                    callable.arity(),
                    arguments.len()
                ))
                .into())
            } else {
                callable.call(self, arguments, &v.right_paren.info)
            }
        } else {
            Err(
                error_stack::report!(RuntimeError::new(v.right_paren.info.clone().into_owned()))
                    .attach_printable(format!(
                        "{callee:#} is not callable, only functions and classes are callable",
                    ))
                    .into(),
            )
        }
    }

    fn visit_grouping(self, v: &ast::expr::Grouping<'_>) -> Self::Output {
        self.evaluate(&v.expression)
    }

    fn visit_literal(self, v: &ast::expr::Literal<'_>) -> Self::Output {
        Ok(object::Object::from(v.literal.typ.clone()))
    }

    fn visit_logical(self, v: &ast::expr::Logical<'_>) -> Self::Output {
        let left = self.evaluate(&v.left)?;

        match v.operator.typ {
            token::Type::Or => {
                if left.is_truthy() {
                    return Ok(left);
                }
            }
            token::Type::And => {
                if !left.is_truthy() {
                    return Ok(left);
                }
            }
            _ => unreachable!(),
        }

        self.evaluate(&v.right)
    }

    fn visit_unary(self, v: &ast::expr::Unary<'_>) -> Self::Output {
        let right = self.evaluate(&v.right)?;

        match &v.operator.typ {
            token::Type::Minus => {
                if let object::Object::Number(num) = right {
                    Ok((-num).into())
                } else {
                    Err(error_stack::report!(RuntimeError::new(
                        v.operator.info.clone().into_owned()
                    ))
                    .attach_printable(format!("cannot apply unary `-` to {right:?}"))
                    .into())
                }
            }

            token::Type::Bang => Ok((!right.is_truthy()).into()),

            _ => unreachable!(),
        }
    }

    fn visit_variable(self, v: &ast::expr::Variable<'_>) -> Self::Output {
        let value = self.lookup_variable(&v.info).ok_or_else(|| {
            error_stack::report!(RuntimeError::new(v.info.clone().into_owned()))
                .attach_printable(format!("undefined variable `{}`", v.info.lexeme))
        })?;

        Ok(value)
    }

    fn visit_function(self, v: &ast::expr::Function<'_>) -> Self::Output {
        Ok(object::Function::new(v, self.environment.context().cloned()).into())
    }
}

impl ast::stmt::Visitor<'_> for &mut Interpreter<'_> {
    type Output = Result<(), RuntimeErrorState>;

    fn visit_block(self, v: &ast::stmt::Block<'_>) -> Self::Output {
        self.execute_block_push_context(v, env::Context::new().info_ref())
    }

    fn visit_break(self, v: &ast::stmt::Break) -> Self::Output {
        Err(RuntimeErrorState::Break(v.info.clone().into_owned()))
    }

    fn visit_expr(self, v: &ast::stmt::Expression<'_>) -> Self::Output {
        self.evaluate(&v.expression)?;
        Ok(())
    }

    fn visit_function(self, v: &ast::stmt::Function<'_>) -> Self::Output {
        let function = object::Function::new(&v.function, self.environment.context().cloned())
            .attach_name(v.name.clone().into_owned())
            .into();

        self.environment
            .define(v.name.lexeme.clone().into_owned(), function);

        Ok(())
    }

    fn visit_if(self, v: &ast::stmt::If<'_>) -> Self::Output {
        if self.evaluate(&v.condition)?.is_truthy() {
            self.execute(&v.then_branch)
        } else if let Some(else_branch) = &v.else_branch {
            self.execute(else_branch)
        } else {
            Ok(())
        }
    }

    fn visit_print(self, v: &ast::stmt::Print<'_>) -> Self::Output {
        let value = self.evaluate(&v.expression)?;
        println!("{value}");
        Ok(())
    }

    fn visit_return(self, v: &ast::stmt::Return<'_>) -> Self::Output {
        let value = v
            .value
            .as_ref()
            .map_or(Ok(object::Object::Nil), |expr| self.evaluate(expr))?;

        Err(RuntimeErrorState::Return(
            value,
            v.keyword.clone().into_owned(),
        ))
    }

    fn visit_var(self, v: &ast::stmt::Var<'_>) -> Self::Output {
        let value = v
            .initializer
            .as_ref()
            .map_or(Ok(object::Object::Nil), |init| self.evaluate(init))?;

        self.environment
            .define(v.info.lexeme.clone().into_owned(), value);

        Ok(())
    }

    fn visit_while(self, v: &ast::stmt::While<'_>) -> Self::Output {
        while self.evaluate(&v.condition)?.is_truthy() {
            if let Err(state) = self.execute(&v.body) {
                if let RuntimeErrorState::Break(_) = state {
                    break;
                } else {
                    return Err(state);
                }
            }
        }

        Ok(())
    }
}

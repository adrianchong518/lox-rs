use std::{collections::HashMap, fmt};

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
pub enum RuntimeErrorState<'s> {
    Error(error_stack::Report<RuntimeError>),
    Break(token::Info<'s>),
    Return(object::Object<'s>, token::Info<'s>),
}

impl RuntimeErrorState<'_> {
    pub fn into_error(self) -> Self {
        Self::Error(self.into())
    }
}

impl From<error_stack::Report<RuntimeError>> for RuntimeErrorState<'_> {
    fn from(value: error_stack::Report<RuntimeError>) -> Self {
        Self::Error(value)
    }
}

impl From<RuntimeErrorState<'_>> for error_stack::Report<RuntimeError> {
    fn from(value: RuntimeErrorState) -> Self {
        match value {
            RuntimeErrorState::Error(e) => e,
            RuntimeErrorState::Break(info) => error_stack::report!(RuntimeError {
                info: info.into_owned()
            })
            .attach_printable("`break` not inside any loop"),
            RuntimeErrorState::Return(_, info) => error_stack::report!(RuntimeError {
                info: info.into_owned()
            })
            .attach_printable("`return` not inside any function"),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter<'s> {
    environment: env::Environment<'s>,
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
        statements: &[ast::Stmt<'s>],
    ) -> error_stack::Result<(), RuntimeError> {
        for statement in statements {
            self.execute(statement)?;
        }

        Ok(())
    }

    pub fn repl_evaluate(
        &mut self,
        expression: &ast::Expr<'s>,
    ) -> error_stack::Result<object::Object<'s>, RuntimeError> {
        self.evaluate(expression).map_err(Into::into)
    }

    pub fn execute(&mut self, statement: &ast::Stmt<'s>) -> Result<(), RuntimeErrorState<'s>> {
        statement.accept(self)
    }

    fn execute_block(&mut self, block: &ast::stmt::Block<'s>) -> Result<(), RuntimeErrorState<'s>> {
        block
            .statements
            .iter()
            .try_for_each(|stmt| self.execute(stmt))
    }

    pub fn execute_block_push_context(
        &mut self,
        block: &ast::stmt::Block<'s>,
        context: env::ContextRef<'s>,
    ) -> Result<(), RuntimeErrorState<'s>> {
        self.environment.push_context(context);

        let result = self.execute_block(block);

        self.environment.pop_context();
        result
    }

    pub fn execute_block_swap_context(
        &mut self,
        block: &ast::stmt::Block<'s>,
        context: env::ContextRef<'s>,
    ) -> Result<(), RuntimeErrorState<'s>> {
        let old_context = self.environment.swap_context(Some(context));

        let result = self.execute_block(block);

        self.environment.swap_context(old_context);

        result
    }

    pub fn evaluate(
        &mut self,
        expression: &ast::Expr<'s>,
    ) -> Result<object::Object<'s>, RuntimeErrorState<'s>> {
        expression.accept(self)
    }

    pub fn resolve(&mut self, name: token::Info<'s>, location: resolver::VariableLocation) {
        self.resolve_map.insert(name, location);
    }

    fn lookup_variable(&self, name: &token::Info<'s>) -> Option<object::Object<'s>> {
        match self.resolve_map.get(name) {
            Some(location) => self.environment.get_at(location),
            None => self.environment.get_global(&name.lexeme),
        }
    }

    fn assign_variable(
        &mut self,
        name: &token::Info<'s>,
        value: object::Object<'s>,
    ) -> error_stack::Result<(), RuntimeError> {
        match self.resolve_map.get(name) {
            Some(location) => self.environment.assign_at(location, value),
            None => self.environment.assign_global(&name.lexeme, value),
        }
        .change_context(RuntimeError::new(name.clone().into_owned()))
    }
}

impl<'s> Default for Interpreter<'s> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'s> ast::expr::Visitor<'s> for &mut Interpreter<'s> {
    type Output = Result<object::Object<'s>, RuntimeErrorState<'s>>;

    fn visit_assign(self, v: &ast::expr::Assign<'s>) -> Self::Output {
        let value = self.evaluate(&v.value)?;
        self.assign_variable(&v.info, value.clone())?;
        Ok(value)
    }

    fn visit_binary(self, v: &ast::expr::Binary<'s>) -> Self::Output {
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

    fn visit_call(self, v: &ast::expr::Call<'s>) -> Self::Output {
        let callee = self.evaluate(&v.callee)?;

        let arguments = v
            .arguments
            .iter()
            .map(|arg| self.evaluate(arg))
            .collect::<Result<Vec<_>, _>>()?;

        let object::Object::Callable(callable) = callee  else {
            return Err(
                error_stack::report!(RuntimeError::new(v.right_paren.info.clone().into_owned()))
                    .attach_printable(format!(
                        "{callee:#} is not callable, only functions and classes are callable",
                    ))
                    .into(),
            );
        };

        if callable.arity() == arguments.len() {
            return callable.call(self, arguments, &v.right_paren.info);
        }

        Err(
            error_stack::report!(RuntimeError::new(v.right_paren.info.clone().into_owned()))
                .attach_printable(format!(
                    "expected {} arguments but got {}",
                    callable.arity(),
                    arguments.len()
                ))
                .into(),
        )
    }

    fn visit_get(self, v: &ast::expr::Get<'s>) -> Self::Output {
        let object::Object::Instance(instance) = self.evaluate(&v.object)? else {
            return Err(
                error_stack::report!(RuntimeError::new(v.dot.clone().into_owned()))
                    .attach_printable("only instances have properties")
                    .into(),
            );
        };

        let Some(object) = instance .get(&v.name.lexeme) else {
            return Err(error_stack::report!(RuntimeError::new(v.name.clone().into_owned()))
                .attach_printable(format!("undefined property `{}`", v.name.lexeme))
                .into());
        };

        if let object::Object::Callable(object::CallableObject::Function(func)) = &object {
            if func.is_getter() {
                return func.call(self, Vec::new(), &v.name);
            }
        }

        Ok(object)
    }

    fn visit_set(self, v: &ast::expr::Set<'s>) -> Self::Output {
        let object::Object::Instance(instance) = self.evaluate(&v.field.object)? else {
            return Err(
                error_stack::report!(RuntimeError::new(v.field.dot.clone().into_owned()))
                    .attach_printable("only instances have fields")
                    .into(),
            );
        };

        let value = self.evaluate(&v.value)?;
        instance.set(v.field.name.lexeme.clone().into_owned(), value.clone());

        Ok(value)
    }

    fn visit_grouping(self, v: &ast::expr::Grouping<'s>) -> Self::Output {
        self.evaluate(&v.expression)
    }

    fn visit_literal(self, v: &ast::expr::Literal<'s>) -> Self::Output {
        Ok(object::Object::from(v.literal.typ.clone()))
    }

    fn visit_logical(self, v: &ast::expr::Logical<'s>) -> Self::Output {
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

    fn visit_this(self, v: &ast::expr::This<'s>) -> Self::Output {
        self.lookup_variable(&v.keyword).ok_or_else(|| {
            error_stack::report!(RuntimeError::new(v.keyword.clone().into_owned()))
                .attach_printable("`this` is only defined for inside classes")
                .into()
        })
    }

    fn visit_unary(self, v: &ast::expr::Unary<'s>) -> Self::Output {
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

    fn visit_variable(self, v: &ast::expr::Variable<'s>) -> Self::Output {
        let value = self.lookup_variable(&v.info).ok_or_else(|| {
            error_stack::report!(RuntimeError::new(v.info.clone().into_owned()))
                .attach_printable(format!("undefined variable `{}`", v.info.lexeme))
        })?;

        Ok(value)
    }

    fn visit_function(self, v: &ast::expr::Function<'s>) -> Self::Output {
        Ok(object::Function::new(v, self.environment.context().cloned(), false).into())
    }
}

impl<'s> ast::stmt::Visitor<'s> for &mut Interpreter<'s> {
    type Output = Result<(), RuntimeErrorState<'s>>;

    fn visit_block(self, v: &ast::stmt::Block<'s>) -> Self::Output {
        self.execute_block_push_context(v, env::Context::new().info_ref())
    }

    fn visit_break(self, v: &ast::stmt::Break<'s>) -> Self::Output {
        Err(RuntimeErrorState::Break(v.info.clone()))
    }

    fn visit_class(self, v: &ast::stmt::Class<'s>) -> Self::Output {
        self.environment
            .define(v.name.lexeme.clone().into_owned(), object::Object::Nil);

        let mut methods = HashMap::new();
        for method in &v.methods {
            let is_initializer = method.name.lexeme == object::Class::INITIALIZER_NAME;
            let function = object::Function::new(
                &method.function,
                self.environment.context().cloned(),
                is_initializer,
            );
            methods.insert(method.name.lexeme.clone().into_owned(), function);
        }

        let class = object::Class::new(v.name.clone(), methods);
        self.assign_variable(&v.name, class.into())?;

        Ok(())
    }

    fn visit_expr(self, v: &ast::stmt::Expression<'s>) -> Self::Output {
        self.evaluate(&v.expression)?;
        Ok(())
    }

    fn visit_function(self, v: &ast::stmt::Function<'s>) -> Self::Output {
        let function =
            object::Function::new(&v.function, self.environment.context().cloned(), false)
                .attach_name(v.name.clone().into_owned())
                .into();

        self.environment
            .define(v.name.lexeme.clone().into_owned(), function);

        Ok(())
    }

    fn visit_if(self, v: &ast::stmt::If<'s>) -> Self::Output {
        if self.evaluate(&v.condition)?.is_truthy() {
            self.execute(&v.then_branch)
        } else if let Some(else_branch) = &v.else_branch {
            self.execute(else_branch)
        } else {
            Ok(())
        }
    }

    fn visit_print(self, v: &ast::stmt::Print<'s>) -> Self::Output {
        let value = self.evaluate(&v.expression)?;
        println!("{value}");
        Ok(())
    }

    fn visit_return(self, v: &ast::stmt::Return<'s>) -> Self::Output {
        let value = v
            .value
            .as_ref()
            .map_or(Ok(object::Object::Nil), |expr| self.evaluate(expr))?;

        Err(RuntimeErrorState::Return(value, v.keyword.clone()))
    }

    fn visit_var(self, v: &ast::stmt::Var<'s>) -> Self::Output {
        let value = v
            .initializer
            .as_ref()
            .map_or(Ok(object::Object::Nil), |init| self.evaluate(init))?;

        self.environment
            .define(v.info.lexeme.clone().into_owned(), value);

        Ok(())
    }

    fn visit_while(self, v: &ast::stmt::While<'s>) -> Self::Output {
        while self.evaluate(&v.condition)?.is_truthy() {
            if let Err(state) = self.execute(&v.body) {
                if let RuntimeErrorState::Break(_) = state {
                    break;
                }

                return Err(state);
            }
        }

        Ok(())
    }
}

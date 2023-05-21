use std::{borrow::Cow, collections::HashMap, fmt};

use crate::{ast, interpreter, into_owned::IntoOwned as _, token};

#[derive(Debug)]
pub struct ResolutionError {
    info: token::Info<'static>,
}

impl fmt::Display for ResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "[line {} at {:?}] resolution error",
            self.info.line, self.info.lexeme,
        )
    }
}

impl error_stack::Context for ResolutionError {}

pub type ResolveMap<'s> = HashMap<token::Info<'s>, usize>;

#[derive(Debug)]
pub struct Resolver<'s, 'a> {
    interpreter: &'a mut interpreter::Interpreter<'s>,
    scopes: Vec<HashMap<Cow<'s, str>, bool>>,
    current_function: Option<FunctionType>,
}

#[derive(Debug)]
enum FunctionType {
    Function,
}

impl<'s, 'a> Resolver<'s, 'a> {
    pub fn new(interpreter: &'a mut interpreter::Interpreter<'s>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: None,
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn with_scope(
        &mut self,
        f: impl FnOnce(&mut Self) -> error_stack::Result<(), ResolutionError>,
    ) -> error_stack::Result<(), ResolutionError> {
        self.begin_scope();
        f(self)?;
        self.end_scope();

        Ok(())
    }

    fn with_function_scope(
        &mut self,
        typ: FunctionType,
        f: impl FnOnce(&mut Self) -> error_stack::Result<(), ResolutionError>,
    ) -> error_stack::Result<(), ResolutionError> {
        let enclosing = self.current_function.replace(typ);
        self.with_scope(f)?;
        self.current_function = enclosing;

        Ok(())
    }

    fn resolve_expr(
        &mut self,
        expression: &ast::Expr<'s>,
    ) -> error_stack::Result<(), ResolutionError> {
        expression.accept(self)
    }

    fn resolve_stmt(
        &mut self,
        statement: &ast::Stmt<'s>,
    ) -> error_stack::Result<(), ResolutionError> {
        statement.accept(self)
    }

    pub fn resolve_many(
        &mut self,
        statements: &[ast::Stmt<'s>],
    ) -> error_stack::Result<(), ResolutionError> {
        let mut error: Option<error_stack::Report<ResolutionError>> = None;

        for statement in statements {
            if let Err(report) = self.resolve_stmt(statement) {
                match &mut error {
                    Some(e) => e.extend_one(report),
                    None => error = Some(report),
                }
            }
        }

        if let Some(error) = error {
            Err(error)
        } else {
            Ok(())
        }
    }

    fn resolve_local(&mut self, name: token::Info<'s>) {
        if let Some((distance, _)) = self
            .scopes
            .iter()
            .rev()
            .enumerate()
            .find(|(_, scope)| scope.contains_key(&name.lexeme))
        {
            self.interpreter.resolve(name, distance)
        }
    }

    fn resolve_function(
        &mut self,
        function: &ast::expr::Function<'s>,
        typ: FunctionType,
    ) -> error_stack::Result<(), ResolutionError> {
        self.with_function_scope(typ, |s| {
            for param in &function.parameters {
                s.declare(param)?;
                s.define(param);
            }
            s.resolve_many(&function.body.statements)
        })
    }

    fn declare(&mut self, name: &token::Info<'s>) -> error_stack::Result<(), ResolutionError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(error_stack::report!(ResolutionError {
                    info: name.clone().into_owned()
                })
                .attach_printable("variable with the same name already declared in this scope"));
            }
            scope.insert(name.lexeme.clone(), false);
        }

        Ok(())
    }

    fn define(&mut self, name: &token::Info<'s>) {
        self.scopes
            .last_mut()
            .and_then(|scope| scope.insert(name.lexeme.clone(), true));
    }
}

impl<'s> ast::expr::Visitor<'s> for &mut Resolver<'s, '_> {
    type Output = error_stack::Result<(), ResolutionError>;

    fn visit_assign(self, v: &ast::expr::Assign<'s>) -> Self::Output {
        self.resolve_expr(&v.value)?;
        self.resolve_local(v.info.clone());
        Ok(())
    }

    fn visit_binary(self, v: &ast::expr::Binary<'s>) -> Self::Output {
        self.resolve_expr(&v.left)?;
        self.resolve_expr(&v.right)?;
        Ok(())
    }

    fn visit_call(self, v: &ast::expr::Call<'s>) -> Self::Output {
        self.resolve_expr(&v.callee)?;

        for argument in &v.arguments {
            self.resolve_expr(argument)?;
        }

        Ok(())
    }

    fn visit_grouping(self, v: &ast::expr::Grouping<'s>) -> Self::Output {
        self.resolve_expr(&v.expression)
    }

    fn visit_literal(self, _v: &ast::expr::Literal<'s>) -> Self::Output {
        Ok(())
    }

    fn visit_logical(self, v: &ast::expr::Logical<'s>) -> Self::Output {
        self.resolve_expr(&v.left)?;
        self.resolve_expr(&v.right)?;
        Ok(())
    }

    fn visit_unary(self, v: &ast::expr::Unary<'s>) -> Self::Output {
        self.resolve_expr(&v.right)
    }

    fn visit_variable(self, v: &ast::expr::Variable<'s>) -> Self::Output {
        if self
            .scopes
            .last()
            .and_then(|scope| scope.get(&v.info.lexeme))
            .map(|b| !b)
            .unwrap_or(false)
        {
            return Err(error_stack::report!(ResolutionError {
                info: v.info.clone().into_owned()
            })
            .attach_printable("cannot read local variable in its own initializer"));
        }

        self.resolve_local(v.info.clone());
        Ok(())
    }

    fn visit_function(self, v: &ast::expr::Function<'s>) -> Self::Output {
        self.resolve_function(v, FunctionType::Function)
    }
}

impl<'s> ast::stmt::Visitor<'s> for &mut Resolver<'s, '_> {
    type Output = error_stack::Result<(), ResolutionError>;

    fn visit_block(self, v: &ast::stmt::Block<'s>) -> Self::Output {
        self.with_scope(|s| s.resolve_many(&v.statements))
    }

    fn visit_break(self, _v: &ast::stmt::Break<'s>) -> Self::Output {
        Ok(())
    }

    fn visit_expr(self, v: &ast::stmt::Expression<'s>) -> Self::Output {
        self.resolve_expr(&v.expression)
    }

    fn visit_function(self, v: &ast::stmt::Function<'s>) -> Self::Output {
        self.declare(&v.name)?;
        self.define(&v.name);

        v.function.accept(self)
    }

    fn visit_if(self, v: &ast::stmt::If<'s>) -> Self::Output {
        self.resolve_expr(&v.condition)?;
        self.resolve_stmt(&v.then_branch)?;
        v.else_branch
            .as_ref()
            .map(|stmt| self.resolve_stmt(stmt))
            .transpose()?;
        Ok(())
    }

    fn visit_print(self, v: &ast::stmt::Print<'s>) -> Self::Output {
        self.resolve_expr(&v.expression)
    }

    fn visit_return(self, v: &ast::stmt::Return<'s>) -> Self::Output {
        if self.current_function.is_none() {
            return Err(error_stack::report!(ResolutionError {
                info: v.keyword.clone().into_owned()
            })
            .attach_printable("cannot return from top-level code"));
        }

        v.value
            .as_ref()
            .map(|expr| self.resolve_expr(expr))
            .transpose()?;
        Ok(())
    }

    fn visit_var(self, v: &ast::stmt::Var<'s>) -> Self::Output {
        self.declare(&v.info)?;

        if let Some(initializer) = &v.initializer {
            self.resolve_expr(initializer)?;
        }

        self.define(&v.info);

        Ok(())
    }

    fn visit_while(self, v: &ast::stmt::While<'s>) -> Self::Output {
        self.resolve_expr(&v.condition)?;
        self.resolve_stmt(&v.body)?;
        Ok(())
    }
}

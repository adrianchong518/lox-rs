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

pub type ResolveMap<'s> = HashMap<token::Info<'s>, VariableLocation>;

#[derive(Debug)]
struct Variable<'s> {
    name: Option<token::Info<'s>>,
    state: VariableState,
    slot: usize,
}

#[derive(Debug, PartialEq, Eq)]
enum VariableState {
    Declared,
    Defined,
    Read,
}

#[derive(Debug)]
pub struct VariableLocation {
    pub distance: usize,
    pub slot: usize,
}

type Scope<'s> = HashMap<Cow<'s, str>, Variable<'s>>;

#[derive(Debug)]
pub struct Resolver<'s, 'a> {
    interpreter: &'a mut interpreter::Interpreter<'s>,
    scopes: Vec<Scope<'s>>,
    current_function: Option<FunctionType>,
    current_class: Option<ClassType>,
}

#[derive(Debug, Clone, Copy)]
enum FunctionType {
    Function,
    Method,
}

#[derive(Debug, Clone, Copy)]
enum ClassType {
    Class,
    SubClass,
}

impl<'s, 'a> Resolver<'s, 'a> {
    pub fn new(interpreter: &'a mut interpreter::Interpreter<'s>) -> Self {
        Self {
            interpreter,
            scopes: Vec::new(),
            current_function: None,
            current_class: None,
        }
    }

    fn begin_scope(&mut self) -> &mut HashMap<Cow<'s, str>, Variable<'s>> {
        self.scopes.push(HashMap::new());
        self.scopes
            .last_mut()
            .expect("a new scope must have been created")
    }

    fn end_scope(&mut self) -> error_stack::Result<(), ResolutionError> {
        let Some(scope) = self.scopes.pop() else {
            return Ok(())
        };

        let mut error: Option<error_stack::Report<ResolutionError>> = None;

        for (state, name) in scope
            .into_values()
            .filter_map(|v| v.name.map(|n| (v.state, n)))
        {
            if state == VariableState::Defined {
                let report = error_stack::report!(ResolutionError {
                    info: name.into_owned()
                })
                .attach_printable("local variable is not used");

                match error.as_mut() {
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

    fn with_scope(
        &mut self,
        f: impl FnOnce(&mut Self) -> error_stack::Result<(), ResolutionError>,
    ) -> error_stack::Result<(), ResolutionError> {
        self.begin_scope();
        let result = f(self);
        self.end_scope()?;

        result
    }

    fn with_function_scope(
        &mut self,
        typ: FunctionType,
        f: impl FnOnce(&mut Self) -> error_stack::Result<(), ResolutionError>,
    ) -> error_stack::Result<(), ResolutionError> {
        let enclosing = self.current_function.replace(typ);
        let result = self.with_scope(f);
        self.current_function = enclosing;
        result
    }

    fn with_class_scope(
        &mut self,
        typ: ClassType,
        f: impl FnOnce(&mut Self) -> error_stack::Result<(), ResolutionError>,
    ) -> error_stack::Result<(), ResolutionError> {
        let enclosing = self.current_class.replace(typ);

        let result = match typ {
            ClassType::SubClass => self.with_scope(|s| {
                s.scopes
                    .last_mut()
                    .expect("a new scope is created by `with_scope`")
                    .insert(
                        "super".into(),
                        Variable {
                            name: None,
                            state: VariableState::Read,
                            slot: 0,
                        },
                    );
                s.with_scope(f)
            }),

            ClassType::Class => self.with_scope(f),
        };

        self.current_class = enclosing;

        result
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

    fn resolve_local(&mut self, name: token::Info<'s>, is_read: bool) {
        if let Some((distance, scope)) = self
            .scopes
            .iter_mut()
            .rev()
            .enumerate()
            .find(|(_, scope)| scope.contains_key(&name.lexeme))
        {
            let var = scope.get_mut(&name.lexeme).unwrap();
            if is_read {
                var.state = VariableState::Read;
            }

            self.interpreter.resolve(
                name,
                VariableLocation {
                    distance,
                    slot: var.slot,
                },
            );
        }
    }

    fn resolve_function(
        &mut self,
        function: &ast::expr::Function<'s>,
        typ: FunctionType,
    ) -> error_stack::Result<(), ResolutionError> {
        self.with_function_scope(typ, |s| {
            if let Some(parameters) = &function.parameters {
                for param in parameters {
                    s.declare(param, VariableState::Defined)?;
                }
            }

            s.resolve_many(&function.body.statements)
        })
    }

    fn declare(
        &mut self,
        name: &token::Info<'s>,
        state: VariableState,
    ) -> error_stack::Result<(), ResolutionError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(error_stack::report!(ResolutionError {
                    info: name.clone().into_owned()
                })
                .attach_printable("variable with the same name already declared in this scope"));
            }
            scope.insert(
                name.lexeme.clone(),
                Variable {
                    name: Some(name.clone()),
                    state,
                    slot: scope.len(),
                },
            );
        }

        Ok(())
    }

    fn define(&mut self, name: &token::Info<'s>) {
        let Some(scope) = self.scopes.last_mut() else { return };
        if let Some(v) = scope.get_mut(&name.lexeme) {
            v.state = VariableState::Defined;
        }
    }
}

impl<'s> ast::expr::Visitor<'s> for &mut Resolver<'s, '_> {
    type Output = error_stack::Result<(), ResolutionError>;

    fn visit_assign(self, v: &ast::expr::Assign<'s>) -> Self::Output {
        self.resolve_expr(&v.value)?;
        self.resolve_local(v.info.clone(), false);
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

    fn visit_get(self, v: &ast::expr::Get<'s>) -> Self::Output {
        self.resolve_expr(&v.object)
    }

    fn visit_set(self, v: &ast::expr::Set<'s>) -> Self::Output {
        v.field.accept(&mut *self)?;
        self.resolve_expr(&v.value)?;
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

    fn visit_this(self, v: &ast::expr::This<'s>) -> Self::Output {
        if self.current_class.is_none() {
            return Err(error_stack::report!(ResolutionError {
                info: v.keyword.clone().into_owned()
            })
            .attach_printable("cannot use `this` outside of a class"));
        }

        self.resolve_local(v.keyword.clone(), true);
        Ok(())
    }

    fn visit_super(self, v: &ast::expr::Super<'s>) -> Self::Output {
        let Some(current_class) = self.current_class else {
            return Err(error_stack::report!(ResolutionError {
                info: v.keyword.clone().into_owned()
            })
            .attach_printable("cannot use `super` outside of a class"));
        };

        match current_class {
            ClassType::Class => Err(error_stack::report!(ResolutionError {
                info: v.keyword.clone().into_owned()
            })
            .attach_printable("cannot use `super` in a class with no superclass")),
            ClassType::SubClass => {
                self.resolve_local(v.keyword.clone(), true);
                Ok(())
            }
        }
    }

    fn visit_unary(self, v: &ast::expr::Unary<'s>) -> Self::Output {
        self.resolve_expr(&v.right)
    }

    fn visit_variable(self, v: &ast::expr::Variable<'s>) -> Self::Output {
        if self
            .scopes
            .last()
            .and_then(|scope| scope.get(&v.info.lexeme))
            .map_or(false, |v| v.state == VariableState::Declared)
        {
            return Err(error_stack::report!(ResolutionError {
                info: v.info.clone().into_owned()
            })
            .attach_printable("cannot read local variable in its own initializer"));
        }

        self.resolve_local(v.info.clone(), true);
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

    fn visit_class(self, v: &ast::stmt::Class<'s>) -> Self::Output {
        let class_type = if let Some(superclass) = &v.superclass {
            if superclass.info.lexeme == v.name.lexeme {
                return Err(error_stack::report!(ResolutionError {
                    info: superclass.info.clone().into_owned(),
                })
                .attach_printable("a class cannot inherit from itself"));
            }
            superclass.accept(&mut *self)?;
            ClassType::SubClass
        } else {
            ClassType::Class
        };

        self.declare(&v.name, VariableState::Defined)?;

        self.with_class_scope(class_type, |s| {
            s.scopes
                .last_mut()
                .expect("a new scope is created by `with_scope`")
                .insert(
                    "this".into(),
                    Variable {
                        name: None,
                        state: VariableState::Read,
                        slot: 0,
                    },
                );

            for method in v.methods.iter().chain(&v.class_methods) {
                s.declare(&method.name, VariableState::Read)?;
                s.resolve_function(&method.function, FunctionType::Method)?;
            }

            Ok(())
        })
    }

    fn visit_expr(self, v: &ast::stmt::Expression<'s>) -> Self::Output {
        self.resolve_expr(&v.expression)
    }

    fn visit_function(self, v: &ast::stmt::Function<'s>) -> Self::Output {
        self.declare(&v.name, VariableState::Defined)?;
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
        self.declare(&v.info, VariableState::Declared)?;

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

use std::fmt;

use itertools::Itertools as _;

use crate::{into_owned::IntoOwned, token};

macro_rules! define_ast {
    (
        mod $base_mod:ident => $base:ident { $(
            $typ:ident<$lt:lifetime> [$visit_fn:ident] {
                $( $field:ident : $field_type:ty ),* $(,)?
            }
        ),* $(,)? }
    ) => {
        pub use $base_mod::$base;

        pub mod $base_mod {
            use super::*;

            pub trait Visitor<'s> {
                type Output;
                $( fn $visit_fn(self, v: &$typ<'s>) -> Self::Output; )*
            }

            #[derive(Debug, Clone, PartialEq)]
            pub enum $base<'s> {
                $( $typ(Box<$typ<'s>>), )*
            }

            impl<'s> $base<'s> {
                pub fn accept<V: Visitor<'s>>(&self, visitor: V) -> V::Output {
                    match self {
                        $( Self::$typ(v) => v.accept(visitor), )*
                    }
                }
            }

            impl IntoOwned for $base<'_> {
                    type Owned = $base<'static>;

                fn into_owned(self) -> Self::Owned {
                    match self {
                        $(Self::$typ(v) => v.into_owned().into(),)*
                    }
                }
            }

            $(
                #[derive(Debug, Clone, PartialEq)]
                pub struct $typ<$lt> {
                    $( pub $field: $field_type, )*
                }

                impl<'s> $typ<'s> {
                    pub fn accept<V: Visitor<'s>>(&self, visitor: V) -> V::Output {
                        visitor.$visit_fn(self)
                    }

                    pub fn into_owned(self) -> $typ<'static> {
                        $typ {
                            $( $field: self.$field.into_owned(), )*
                        }
                    }
                }

                impl IntoOwned for $typ<'_> {
                    type Owned = $typ<'static>;

                    fn into_owned(self) -> Self::Owned {
                        $typ {
                            $( $field: self.$field.into_owned(), )*
                        }
                    }
                }

                impl<'s> From<$typ<'s>> for $base<'s> {
                    fn from(value: $typ<'s>) -> Self {
                        Self::$typ(Box::new(value))
                    }
                }
            )*
        }
    };
}

define_ast! {
    mod expr => Expr {
        Assign<'s> [visit_assign] {
            info: token::Info<'s>,
            value: Expr<'s>,
        },

        Binary<'s> [visit_binary] {
            left: Expr<'s>,
            operator: token::Token<'s>,
            right: Expr<'s>,
        },

        Call<'s> [visit_call] {
            callee: Expr<'s>,
            right_paren: token::Token<'s>,
            arguments: Vec<Expr<'s>>,
        },

        Grouping<'s> [visit_grouping] {
            expression: Expr<'s>,
        },

        Literal<'s> [visit_literal] {
            literal: token::Literal<'s>,
        },

        Logical<'s> [visit_logical] {
            left: Expr<'s>,
            operator: token::Token<'s>,
            right: Expr<'s>,
        },

        Unary<'s> [visit_unary] {
            operator: token::Token<'s>,
            right: Expr<'s>,
        },

        Variable<'s> [visit_variable] {
            info: token::Info<'s>
        },

        Function<'s> [visit_function] {
            keyword: token::Info<'s>,
            parameters: Vec<token::Info<'s>>,
            body: stmt::Block<'s>,
        },
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.accept(&mut Printer::default()))
    }
}

define_ast! {
    mod stmt => Stmt {
        Block<'s> [visit_block] {
            statements: Vec<Stmt<'s>>,
        },

        Break<'s> [visit_break] {
            info: token::Info<'s>,
        },

        Expression<'s> [visit_expr] {
            expression: Expr<'s>,
        },

        Function<'s> [visit_function] {
            name: token::Info<'s>,
            function: expr::Function<'s>,
        },

        If<'s> [visit_if] {
            condition: Expr<'s>,
            then_branch: Stmt<'s>,
            else_branch: Option<Stmt<'s>>,
        },

        Print<'s> [visit_print] {
            expression: expr::Expr<'s>,
        },

        Return<'s> [visit_return] {
            keyword: token::Info<'s>,
            value: Option<Expr<'s>>,
        },

        Var<'s> [visit_var] {
            info: token::Info<'s>,
            initializer: Option<expr::Expr<'s>>,
        },

        While<'s> [visit_while] {
            condition: Expr<'s>,
            body: Stmt<'s>,
        },
    }
}

impl fmt::Display for Stmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.accept(&mut Printer::default()))
    }
}

impl<'s> From<Expr<'s>> for Stmt<'s> {
    fn from(value: Expr<'s>) -> Self {
        stmt::Expression { expression: value }.into()
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Printer {
    indentation: usize,
}

impl Printer {
    fn indent(&self) -> String {
        " ".repeat(self.indentation)
    }
}

impl expr::Visitor<'_> for &mut Printer {
    type Output = String;

    fn visit_assign(self, v: &expr::Assign<'_>) -> Self::Output {
        format!("(set! {} {})", v.info.lexeme, v.value.accept(self))
    }

    fn visit_binary(self, v: &expr::Binary<'_>) -> Self::Output {
        format!(
            "({} {} {})",
            v.operator.info.lexeme,
            v.left.accept(&mut *self),
            v.right.accept(&mut *self),
        )
    }

    fn visit_call(self, v: &expr::Call<'_>) -> Self::Output {
        // TODO: change to std once stable
        #[allow(unstable_name_collisions)]
        let arguments = v
            .arguments
            .iter()
            .map(|expr| format!(" {}", expr.accept(&mut *self)))
            .fold(String::new(), |acc, x| format!("{acc}{x}"));

        format!("({}{arguments})", v.callee.accept(self))
    }

    fn visit_grouping(self, v: &expr::Grouping<'_>) -> Self::Output {
        format!("({})", v.expression.accept(self))
    }

    fn visit_literal(self, v: &expr::Literal<'_>) -> Self::Output {
        format!("{}", v.literal.typ)
    }

    fn visit_logical(self, v: &expr::Logical<'_>) -> Self::Output {
        format!(
            "({} {} {})",
            v.operator.info.lexeme,
            v.left.accept(&mut *self),
            v.right.accept(&mut *self),
        )
    }

    fn visit_unary(self, v: &expr::Unary<'_>) -> Self::Output {
        format!("({} {})", v.operator.info.lexeme, v.right.accept(self))
    }

    fn visit_variable(self, v: &expr::Variable<'_>) -> Self::Output {
        format!("{}", v.info.lexeme)
    }

    fn visit_function(self, v: &expr::Function<'_>) -> Self::Output {
        // TODO: change to std once stable
        #[allow(unstable_name_collisions)]
        let parameters = v
            .parameters
            .iter()
            .map(|info| &*info.lexeme)
            .intersperse(" ")
            .fold(String::new(), |acc, x| format!("{acc}{x}"));

        self.indentation += 2;
        let body = v.body.accept(&mut *self);
        self.indentation -= 2;

        format!("(fn ({parameters})\n{body})")
    }
}

impl stmt::Visitor<'_> for &mut Printer {
    type Output = String;

    fn visit_block(self, v: &stmt::Block<'_>) -> Self::Output {
        self.indentation += 2;

        // TODO: change to std once stable
        #[allow(unstable_name_collisions)]
        let statements = v
            .statements
            .iter()
            .map(|stmt| stmt.accept(&mut *self))
            .intersperse("\n".to_string())
            .fold(String::new(), |acc, x| format!("{acc}{x}"));

        self.indentation -= 2;

        format!("{}(begin\n{statements})", self.indent())
    }

    fn visit_break(self, _: &stmt::Break<'_>) -> Self::Output {
        format!("{}(break)", self.indent())
    }

    fn visit_expr(self, v: &stmt::Expression<'_>) -> Self::Output {
        format!("{}{}", self.indent(), v.expression.accept(self))
    }

    fn visit_function(self, v: &stmt::Function<'_>) -> Self::Output {
        self.indentation += 2;
        let function = v.function.accept(&mut *self);
        self.indentation -= 2;

        format!(
            "{}(define {}\n{0}  {function})",
            self.indent(),
            v.name.lexeme
        )
    }

    fn visit_if(self, v: &stmt::If<'_>) -> Self::Output {
        self.indentation += 2;
        let then_branch = v.then_branch.accept(&mut *self);
        let else_branch = v.else_branch.as_ref().map(|s| s.accept(&mut *self));
        self.indentation -= 2;

        if let Some(else_branch) = else_branch {
            format!(
                "{}(if {}\n{then_branch}\n{else_branch})",
                self.indent(),
                v.condition.accept(self),
            )
        } else {
            format!(
                "{}(if {}\n{then_branch})",
                self.indent(),
                v.condition.accept(self)
            )
        }
    }

    fn visit_print(self, v: &stmt::Print<'_>) -> Self::Output {
        format!("{}(print {})", self.indent(), v.expression.accept(self))
    }

    fn visit_return(self, v: &stmt::Return<'_>) -> Self::Output {
        if let Some(value) = &v.value {
            format!("{}(return {value})", self.indent())
        } else {
            format!("{}(return)", self.indent())
        }
    }

    fn visit_var(self, v: &stmt::Var<'_>) -> Self::Output {
        if let Some(initializer) = &v.initializer {
            format!(
                "{}(define {} {})",
                self.indent(),
                v.info.lexeme,
                initializer.accept(self)
            )
        } else {
            format!("{}(define {} nil)", self.indent(), v.info.lexeme)
        }
    }

    fn visit_while(self, v: &stmt::While<'_>) -> Self::Output {
        self.indentation += 7;
        let body = v.body.accept(&mut *self);
        self.indentation -= 7;

        format!(
            "{}(while {}\n{body})",
            self.indent(),
            v.condition.accept(self)
        )
    }
}

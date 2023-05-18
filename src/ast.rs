use std::fmt;

use crate::token;

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
                $( fn $visit_fn(&mut self, v: &$typ<'s>) -> Self::Output; )*
            }

            #[derive(Debug)]
            pub enum $base<'s> {
                $( $typ(Box<$typ<'s>>), )*
            }

            impl<'s> $base<'s> {
                pub fn accept<V: Visitor<'s>>(&self, visitor: &mut V) -> V::Output {
                    match self {
                        $( Self::$typ(v) => v.accept(visitor), )*
                    }
                }
            }

            $(
                #[derive(Debug)]
                pub struct $typ<$lt> {
                    $( pub $field: $field_type, )*
                }

                impl<'s> $typ<'s> {
                    fn accept<V: Visitor<'s>>(&self, visitor: &mut V) -> V::Output {
                        visitor.$visit_fn(self)
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

        Grouping<'s> [visit_grouping] {
            expression: Expr<'s>,
        },

        Literal<'s> [visit_literal] {
            literal: token::Literal<'s>,
        },

        Unary<'s> [visit_unary] {
            operator: token::Token<'s>,
            right: Expr<'s>,
        },

        Variable<'s> [visit_variable] {
            info: token::Info<'s>
        }
    }
}

impl fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.accept(&mut Printer))
    }
}

define_ast! {
    mod stmt => Stmt {
        Expression<'s> [visit_expr] {
            expression: expr::Expr<'s>,
        },

        Print<'s> [visit_print] {
            expression: expr::Expr<'s>,
        },

        Var<'s> [visit_var] {
            info: token::Info<'s>,
            initializer: Option<expr::Expr<'s>>,
        }
    }
}

impl fmt::Display for Stmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.accept(&mut Printer))
    }
}

#[derive(Debug, Clone, Copy)]
struct Printer;

impl expr::Visitor<'_> for Printer {
    type Output = String;

    fn visit_binary(&mut self, v: &expr::Binary) -> Self::Output {
        format!("({} {} {})", v.operator.info.lexeme, v.left, v.right,)
    }

    fn visit_grouping(&mut self, v: &expr::Grouping) -> Self::Output {
        format!("(group {})", v.expression)
    }

    fn visit_literal(&mut self, v: &expr::Literal) -> Self::Output {
        format!("{}", v.literal.typ)
    }

    fn visit_unary(&mut self, v: &expr::Unary) -> Self::Output {
        format!("({} {})", v.operator.info.lexeme, v.right)
    }

    fn visit_variable(&mut self, v: &expr::Variable) -> Self::Output {
        format!("{}", v.info.lexeme)
    }

    fn visit_assign(&mut self, v: &expr::Assign) -> Self::Output {
        format!("(assign {} {})", v.info.lexeme, v.value)
    }
}

impl stmt::Visitor<'_> for Printer {
    type Output = String;

    fn visit_expr(&mut self, v: &stmt::Expression) -> Self::Output {
        format!("(expr {})", v.expression)
    }

    fn visit_print(&mut self, v: &stmt::Print) -> Self::Output {
        format!("(print {})", v.expression)
    }

    fn visit_var(&mut self, v: &stmt::Var) -> Self::Output {
        if let Some(initializer) = &v.initializer {
            format!("(define {} {})", v.info.lexeme, initializer)
        } else {
            format!("(define {} nil)", v.info.lexeme)
        }
    }
}

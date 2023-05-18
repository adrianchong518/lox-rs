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
                $( fn $visit_fn(self, v: &$typ<'s>) -> Self::Output; )*
            }

            #[derive(Debug)]
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

            $(
                #[derive(Debug)]
                pub struct $typ<$lt> {
                    $( pub $field: $field_type, )*
                }

                impl<'s> $typ<'s> {
                    fn accept<V: Visitor<'s>>(&self, visitor: V) -> V::Output {
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
        write!(f, "{}", self.accept(Printer))
    }
}

define_ast! {
    mod stmt => Stmt {
        Block<'s> [visit_block] {
            statements: Vec<Stmt<'s>>,
        },

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
        write!(f, "{}", self.accept(Printer))
    }
}

#[derive(Debug, Clone, Copy)]
struct Printer;

impl expr::Visitor<'_> for Printer {
    type Output = String;

    fn visit_binary(self, v: &expr::Binary) -> Self::Output {
        format!("({} {} {})", v.operator.info.lexeme, v.left, v.right,)
    }

    fn visit_grouping(self, v: &expr::Grouping) -> Self::Output {
        format!("(group {})", v.expression)
    }

    fn visit_literal(self, v: &expr::Literal) -> Self::Output {
        format!("{}", v.literal.typ)
    }

    fn visit_unary(self, v: &expr::Unary) -> Self::Output {
        format!("({} {})", v.operator.info.lexeme, v.right)
    }

    fn visit_variable(self, v: &expr::Variable) -> Self::Output {
        format!("{}", v.info.lexeme)
    }

    fn visit_assign(self, v: &expr::Assign) -> Self::Output {
        format!("(assign {} {})", v.info.lexeme, v.value)
    }
}

impl stmt::Visitor<'_> for Printer {
    type Output = String;

    fn visit_expr(self, v: &stmt::Expression) -> Self::Output {
        format!("(expr {})", v.expression)
    }

    fn visit_print(self, v: &stmt::Print) -> Self::Output {
        format!("(print {})", v.expression)
    }

    fn visit_var(self, v: &stmt::Var) -> Self::Output {
        if let Some(initializer) = &v.initializer {
            format!("(define {} {})", v.info.lexeme, initializer)
        } else {
            format!("(define {} nil)", v.info.lexeme)
        }
    }

    fn visit_block(self, v: &stmt::Block) -> Self::Output {
        let statements = v
            .statements
            .iter()
            .fold(String::new(), |s, stmt| format!("    {s}{stmt}\n"));

        format!("(block\n{statements})")
    }
}

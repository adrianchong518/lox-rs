use crate::token;

macro_rules! define_ast {
    (
        $mod_v:vis mod $base_mod:ident => $base:ident { $(
            $typ:ident<$lt:lifetime> [$visit_fn:ident] {
                $( $v:vis $field:ident : $field_type:ty ),* $(,)?
            }
        ),* $(,)? }
    ) => {
        #[allow(dead_code)]
        $mod_v use $base_mod::$base;

        $mod_v mod $base_mod {
            use super::*;

            pub trait Visitor {
                type Output;
                $( fn $visit_fn(&mut self, v: &$typ<'_>) -> Self::Output; )*
            }

            pub enum $base<'s> {
                $( $typ(Box<$typ<'s>>), )*
            }

            impl<'s> $base<'s> {
                pub fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Output {
                    match self {
                        $( Self::$typ(v) => v.accept(visitor), )*
                    }
                }
            }

            $(
                pub struct $typ<$lt> {
                    $( $v $field: $field_type, )*
                }

                impl<'s> $typ<'s> {
                    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Output {
                        visitor.$visit_fn(self)
                    }
                }
            )*
        }
    };
}

define_ast! {
    pub mod expr => Expr {
        Binary<'s> [visit_binary] {
            pub left: Expr<'s>,
            pub operator: token::Token<'s>,
            pub right: Expr<'s>,
        },

        Grouping<'s> [visit_grouping] {
            pub expression: Expr<'s>,
        },

        Literal<'s> [visit_literal] {
            pub literal: token::Literal<'s>,
        },

        Unary<'s> [visit_unary] {
            pub operator: token::Token<'s>,
            pub right: Expr<'s>,
        },
    }
}

pub fn print(expression: &Expr) -> String {
    expression.accept(&mut Printer)
}

#[derive(Debug, Clone, Copy)]
pub struct Printer;

impl expr::Visitor for Printer {
    type Output = String;

    fn visit_binary(&mut self, v: &expr::Binary<'_>) -> Self::Output {
        format!(
            "({} {} {})",
            v.operator.lexeme,
            v.left.accept(self),
            v.right.accept(self),
        )
    }

    fn visit_grouping(&mut self, v: &expr::Grouping<'_>) -> Self::Output {
        format!("(group {})", v.expression.accept(self))
    }

    fn visit_literal(&mut self, v: &expr::Literal<'_>) -> Self::Output {
        format!("{}", v.literal.typ)
    }

    fn visit_unary(&mut self, v: &expr::Unary<'_>) -> Self::Output {
        format!("({} {})", v.operator.lexeme, v.right.accept(self))
    }
}

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
        $mod_v mod $base_mod {
            use super::*;

            pub trait Visitor<'s> {
                type Output;
                $( fn $visit_fn(self, value: &$typ<'s>) -> Self::Output; )*
            }

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
                pub struct $typ<$lt> {
                    $( $v $field: $field_type, )*
                }

                impl<'s> $typ<'s> {
                    fn accept<V: Visitor<'s>>(&self, visitor: V) -> V::Output {
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

#[derive(Debug, Clone, Copy)]
pub struct Printer;

impl Printer {
    pub fn print(expression: &expr::Expr) -> String {
        expression.accept(Printer)
    }
}

impl<'s> expr::Visitor<'s> for Printer {
    type Output = String;

    fn visit_binary(self, value: &expr::Binary<'s>) -> Self::Output {
        format!(
            "({} {} {})",
            value.operator.lexeme,
            value.left.accept(self),
            value.right.accept(self),
        )
    }

    fn visit_grouping(self, value: &expr::Grouping<'s>) -> Self::Output {
        format!("(group {})", value.expression.accept(self))
    }

    fn visit_literal(self, value: &expr::Literal<'s>) -> Self::Output {
        format!("{}", value.literal.typ)
    }

    fn visit_unary(self, value: &expr::Unary<'s>) -> Self::Output {
        format!("({} {})", value.operator.lexeme, value.right.accept(self))
    }
}

use std::fmt;

use itertools::Itertools as _;

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
        }
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

        Expression<'s> [visit_expr] {
            expression: expr::Expr<'s>,
        },

        If<'s> [visit_if] {
            condition: expr::Expr<'s>,
            then_branch: Stmt<'s>,
            else_branch: Option<Stmt<'s>>,
        },

        Print<'s> [visit_print] {
            expression: expr::Expr<'s>,
        },

        Var<'s> [visit_var] {
            info: token::Info<'s>,
            initializer: Option<expr::Expr<'s>>,
        },

        While<'s> [visit_while] {
            condition: expr::Expr<'s>,
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

    fn visit_assign(self, v: &expr::Assign) -> Self::Output {
        format!("(assign {} {})", v.info.lexeme, v.value.accept(self))
    }

    fn visit_binary(self, v: &expr::Binary) -> Self::Output {
        format!(
            "({} {} {})",
            v.operator.info.lexeme,
            v.left.accept(&mut *self),
            v.right.accept(&mut *self),
        )
    }

    fn visit_grouping(self, v: &expr::Grouping) -> Self::Output {
        format!("(group {})", v.expression.accept(self))
    }

    fn visit_literal(self, v: &expr::Literal) -> Self::Output {
        format!("{}", v.literal.typ)
    }

    fn visit_logical(self, v: &expr::Logical) -> Self::Output {
        format!(
            "({} {} {})",
            v.operator.info.lexeme,
            v.left.accept(&mut *self),
            v.right.accept(&mut *self),
        )
    }

    fn visit_unary(self, v: &expr::Unary) -> Self::Output {
        format!("({} {})", v.operator.info.lexeme, v.right.accept(self))
    }

    fn visit_variable(self, v: &expr::Variable) -> Self::Output {
        format!("{}", v.info.lexeme)
    }
}

impl stmt::Visitor<'_> for &mut Printer {
    type Output = String;

    fn visit_block(self, v: &stmt::Block) -> Self::Output {
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

        format!("{}(block\n{statements})", self.indent())
    }

    fn visit_expr(self, v: &stmt::Expression) -> Self::Output {
        format!("{}(expr {})", self.indent(), v.expression.accept(self))
    }

    fn visit_if(self, v: &stmt::If) -> Self::Output {
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
            format!("(if {}\n{then_branch})", v.condition.accept(self))
        }
    }

    fn visit_print(self, v: &stmt::Print) -> Self::Output {
        format!("{}(print {})", self.indent(), v.expression.accept(self))
    }

    fn visit_var(self, v: &stmt::Var) -> Self::Output {
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

    fn visit_while(self, v: &stmt::While) -> Self::Output {
        self.indentation += 2;
        let body = v.body.accept(&mut *self);
        self.indentation -= 2;

        format!(
            "{}(while {}\n{body})",
            self.indent(),
            v.condition.accept(self)
        )
    }
}

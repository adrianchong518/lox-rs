use std::fmt;

use error_stack::ResultExt as _;
use itertools::Itertools as _;

use crate::{ast, token};

#[derive(Debug)]
pub struct ParseError {
    token: Option<token::Token<'static>>,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(token) = &self.token {
            write!(f, "[line {} at {:?}] parse error", token.line, token.lexeme)
        } else {
            write!(f, "[at file end] parse error")
        }
    }
}

impl error_stack::Context for ParseError {}

pub fn parse(tokens: Vec<token::Token<'_>>) -> error_stack::Result<ast::Expr<'_>, ParseError> {
    Parser::new(tokens.into_iter()).expression()
}

struct Parser<'s, Tokens>
where
    Tokens: Iterator<Item = token::Token<'s>>,
{
    tokens: std::iter::Peekable<Tokens>,
}

macro_rules! matches_token {
    ($token:expr, $pattern:pat) => {{
        matches!($token, token::Token { typ: $pattern, .. })
    }};
}

macro_rules! grammar_rule {
    (binary: $rule:ident => $next_rule:ident, $token_pat:pat) => {
        fn $rule(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
            let mut expr = self.$next_rule()?;

            while let Some(operator) = self.tokens.next_if(|t| matches_token!(t, $token_pat)) {
                let left = expr;
                let right = self.$next_rule()?;

                expr = ast::Expr::Binary(Box::new(ast::expr::Binary {
                    left,
                    operator,
                    right,
                }))
            }

            Ok(expr)
        }
    };
}

impl<'s, Tokens> Parser<'s, Tokens>
where
    Tokens: Iterator<Item = token::Token<'s>>,
{
    pub fn new(tokens: Tokens) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn expression(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        self.equality()
    }

    grammar_rule!(binary: equality => comparision, token::Type::BangEqual | token::Type::EqualEqual);
    grammar_rule!(binary: comparision => term,
        token::Type::Greater | token::Type::GreaterEqual | token::Type::Less | token::Type::LessEqual);
    grammar_rule!(binary: term => factor, token::Type::Minus | token::Type::Plus);
    grammar_rule!(binary: factor => unary, token::Type::Slash | token::Type::Star);

    fn unary(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        if let Some(operator) = self
            .tokens
            .next_if(|t| matches_token!(t, token::Type::Bang | token::Type::Minus))
        {
            Ok(ast::Expr::Unary(Box::new(ast::expr::Unary {
                operator,
                right: self.unary()?,
            })))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        if let Some(token::Token {
            typ: token::Type::Literal(typ),
            lexeme,
            line,
        }) = self
            .tokens
            .next_if(|t| matches_token!(t, token::Type::Literal(_)))
        {
            return Ok(ast::Expr::Literal(Box::new(ast::expr::Literal {
                literal: token::Literal { typ, lexeme, line },
            })));
        }

        if self
            .tokens
            .next_if(|t| matches_token!(t, token::Type::LeftParen))
            .is_some()
        {
            let expression = self.expression()?;

            if self
                .tokens
                .next_if(|t| matches_token!(t, token::Type::RightParen))
                .is_none()
            {
                return Err(self.error()).attach_printable("expect ')' after expression");
            }

            return Ok(ast::Expr::Grouping(Box::new(ast::expr::Grouping {
                expression,
            })));
        }

        Err(self.error()).attach_printable("expect expression")
    }

    fn error(&mut self) -> error_stack::Report<ParseError> {
        error_stack::report!(ParseError {
            token: self.tokens.next().map(token::Token::into_owned)
        })
    }

    fn synchronize(&mut self) {
        _ = self.tokens.next();

        self.tokens
            .peeking_take_while(|t| {
                !matches_token!(
                    t,
                    token::Type::Semicolon
                        | token::Type::Class
                        | token::Type::Fun
                        | token::Type::Var
                        | token::Type::For
                        | token::Type::If
                        | token::Type::While
                        | token::Type::Print
                        | token::Type::Return
                )
            })
            .for_each(drop);

        _ = self
            .tokens
            .next_if(|t| matches_token!(t, token::Type::Semicolon));
    }
}

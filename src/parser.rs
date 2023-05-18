use std::fmt;

use error_stack::ResultExt as _;
use itertools::Itertools as _;
use result_inspect::ResultInspectErr as _;

use crate::{ast, token};

#[derive(Debug)]
pub struct ParseError {
    info: Option<token::Info<'static>>,
    synchronize: bool,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(token) = &self.info {
            write!(f, "[line {} at {:?}] parse error", token.line, token.lexeme)
        } else {
            write!(f, "[at file end] parse error")
        }
    }
}

impl error_stack::Context for ParseError {}

pub fn parse(
    tokens: Vec<token::Token<'_>>,
) -> (Vec<ast::Stmt>, Option<error_stack::Report<ParseError>>) {
    let mut statements = Vec::new();
    let mut error: Option<error_stack::Report<ParseError>> = None;

    Parser::new(tokens.into_iter()).for_each(|result| match result {
        Ok(stmt) => statements.push(stmt),
        Err(report) => match &mut error {
            Some(error) => error.extend_one(report),
            None => error = Some(report),
        },
    });

    (statements, error)
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

macro_rules! rule {
    (next_matches($self:ident): $token_pat:pat) => {
        $self.tokens.next_if(|t| matches_token!(t, $token_pat))
    };

    (binary: $rule:ident => $next_rule:ident, $token_pat:pat) => {
        fn $rule(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
            let mut expr = self.$next_rule()?;

            while let Some(operator) = rule!(next_matches(self): $token_pat) {
                let left = expr;
                let right = self.$next_rule()?;

                expr = ast::expr::Binary {
                    left,
                    operator,
                    right,
                }
                .into()
            }

            Ok(expr)
        }
    };

    (consume($self:ident): $token_pat:pat, $error_msg:expr) => {
        if let Some(token) = rule!(next_matches($self): $token_pat) {
            Ok(token)
        } else {
            Err($self.error(true).attach_printable($error_msg))
        }
    };

    (statement_end($self:ident)) => {
        rule!(
            consume($self): token::Type::Semicolon,
            "expect `;` after expression"
        )
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

    fn delcaration(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        if rule!(next_matches(self): token::Type::Var).is_some() {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        let info = rule!(
            consume(self): token::Type::Identifier,
            "expect variable name"
        )?
        .info;

        let initializer = if rule!(next_matches(self): token::Type::Equal).is_some() {
            Some(self.expression()?)
        } else {
            None
        };

        rule!(statement_end(self))?;

        Ok(ast::stmt::Var { info, initializer }.into())
    }

    fn statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        if rule!(next_matches(self): token::Type::Print).is_some() {
            self.print_statement()
        } else if rule!(next_matches(self): token::Type::LeftBrace).is_some() {
            let (statements, error) = self.block();

            if let Some(error) = error {
                Err(error)
            } else {
                Ok(ast::stmt::Block { statements }.into())
            }
        } else {
            self.expression_statement()
        }
    }

    fn expression_statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        let expression = self.expression()?;
        rule!(statement_end(self))?;
        Ok(ast::stmt::Expression { expression }.into())
    }

    fn block(&mut self) -> (Vec<ast::Stmt<'s>>, Option<error_stack::Report<ParseError>>) {
        let mut statements = Vec::new();
        let mut error: Option<error_stack::Report<ParseError>> = None;

        while self
            .tokens
            .peek()
            .map_or(false, |t| !matches_token!(t, token::Type::RightBrace))
        {
            let result = self.delcaration();
            match result {
                Ok(stmt) => statements.push(stmt),
                Err(report) => {
                    if report.current_context().synchronize {
                        self.synchronize()
                    }

                    match &mut error {
                        Some(error) => error.extend_one(report),
                        None => error = Some(report),
                    }
                }
            }
        }

        rule!(
            consume(self): token::Type::RightBrace,
            "expect `}` after block"
        )
        .map_err(|report| match &mut error {
            Some(error) => error.extend_one(report),
            None => error = Some(report),
        })
        .unwrap();

        (statements, error)
    }

    fn print_statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        let expression = self.expression()?;
        rule!(statement_end(self))?;
        Ok(ast::stmt::Print { expression }.into())
    }

    fn expression(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        let l_value = self.equality()?;

        if let Some(token::Token {
            info: equals_info, ..
        }) = rule!(next_matches(self): token::Type::Equal)
        {
            let value = self.assignment()?;

            if let ast::Expr::Variable(var) = l_value {
                return Ok(ast::expr::Assign {
                    info: var.info,
                    value,
                }
                .into());
            }

            // NB This deviates from the book as this will **not** return the `l_value` and
            // continue parsing the expression
            return Err(error_stack::report!(ParseError {
                info: Some(equals_info.into_owned()),
                synchronize: false
            })
            .attach_printable("invalid assignment target"));
        }

        Ok(l_value)
    }

    rule!(binary: equality => comparision, token::Type::BangEqual | token::Type::EqualEqual);
    rule!(binary: comparision => term,
        token::Type::Greater | token::Type::GreaterEqual | token::Type::Less | token::Type::LessEqual);
    rule!(binary: term => factor, token::Type::Minus | token::Type::Plus);
    rule!(binary: factor => unary, token::Type::Slash | token::Type::Star);

    fn unary(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        if let Some(operator) = rule!(next_matches(self): token::Type::Bang | token::Type::Minus) {
            Ok(ast::expr::Unary {
                operator,
                right: self.unary()?,
            }
            .into())
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        if let Some(token::Token {
            typ: token::Type::Literal(typ),
            info,
        }) = rule!(next_matches(self): token::Type::Literal(_))
        {
            return Ok(ast::expr::Literal {
                literal: token::Literal { typ, info },
            }
            .into());
        }

        if let Some(token::Token { info, .. }) = rule!(next_matches(self): token::Type::Identifier)
        {
            return Ok(ast::expr::Variable { info }.into());
        }

        if rule!(next_matches(self): token::Type::LeftParen).is_some() {
            let expression = self.expression()?;

            rule!(
                consume(self): token::Type::RightParen,
                "expect `)` after expression"
            )?;

            return Ok(ast::expr::Grouping { expression }.into());
        }

        Err(self.error(true)).attach_printable("expect expression")
    }

    fn error(&mut self, synchronize: bool) -> error_stack::Report<ParseError> {
        error_stack::report!(ParseError {
            info: self.tokens.next().map(|t| t.info.into_owned()),
            synchronize
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

        _ = rule!(next_matches(self): token::Type::Semicolon);
    }
}

impl<'s, Tokens> Iterator for Parser<'s, Tokens>
where
    Tokens: Iterator<Item = token::Token<'s>>,
{
    type Item = error_stack::Result<ast::Stmt<'s>, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.tokens.peek().is_some() {
            // TODO: use std once stable
            #[allow(unstable_name_collisions)]
            Some(self.delcaration().inspect_err(|r| {
                if r.current_context().synchronize {
                    self.synchronize();
                }
            }))
        } else {
            None
        }
    }
}

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
    allow_expr: bool,
) -> (
    bool,
    Vec<ast::Stmt>,
    Option<error_stack::Report<ParseError>>,
) {
    let mut statements = Vec::new();
    let mut error: Option<error_stack::Report<ParseError>> = None;

    let mut parser = Parser::new(tokens.into_iter(), allow_expr);
    parser.by_ref().for_each(|result| match result {
        Ok(stmt) => statements.push(stmt),
        Err(report) => match &mut error {
            Some(error) => error.extend_one(report),
            None => error = Some(report),
        },
    });

    (parser.found_expr, statements, error)
}

struct Parser<'s, Tokens>
where
    Tokens: Iterator<Item = token::Token<'s>>,
{
    tokens: std::iter::Peekable<Tokens>,
    allow_expr: bool,
    found_expr: bool,
}

macro_rules! matches_token {
    ($token:expr, $pattern:pat) => {{
        matches!($token, token::Token { typ: $pattern, .. })
    }};
}

macro_rules! rule {
    (next_if_matches($self:ident): $token_pat:pat) => {
        $self.tokens.next_if(|t| matches_token!(t, $token_pat))
    };

    (peek_matches($self:ident): $token_pat:pat) => {
        $self.tokens
            .peek()
            .map(|t| matches_token!(t, $token_pat))
    };

    (!peek_matches($self:ident): $token_pat:pat) => {
        $self.tokens
            .peek()
            .map(|t| !matches_token!(t, $token_pat))
    };

    (binary($($expr_type:tt)*): $rule:ident => $next_rule:ident, $token_pat:pat) => {
        fn $rule(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
            let mut expr = self.$next_rule()?;

            while let Some(operator) = rule!(next_if_matches(self): $token_pat) {
                let left = expr;
                let right = self.$next_rule()?;

                expr = $($expr_type)* {
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
        if let Some(token) = rule!(next_if_matches($self): $token_pat) {
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
    pub fn new(tokens: Tokens, allow_expr: bool) -> Self {
        Self {
            tokens: tokens.peekable(),
            allow_expr,
            found_expr: false,
        }
    }

    fn delcaration(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        if rule!(next_if_matches(self): token::Type::Var).is_some() {
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

        let initializer = if rule!(next_if_matches(self): token::Type::Equal).is_some() {
            Some(self.expression()?)
        } else {
            None
        };

        rule!(statement_end(self))?;

        Ok(ast::stmt::Var { info, initializer }.into())
    }

    fn statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        if rule!(next_if_matches(self): token::Type::If).is_some() {
            self.if_statement()
        } else if rule!(next_if_matches(self): token::Type::While).is_some() {
            self.while_statement()
        } else if rule!(next_if_matches(self): token::Type::For).is_some() {
            self.for_statement()
        } else if rule!(next_if_matches(self): token::Type::Print).is_some() {
            self.print_statement()
        } else if rule!(next_if_matches(self): token::Type::LeftBrace).is_some() {
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

    fn if_statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        rule!(
            consume(self): token::Type::LeftParen,
            "expect `(` after `if`"
        )?;

        let condition = self.expression()?;

        rule!(
            consume(self): token::Type::RightParen,
            "expect `)` after if condition"
        )?;

        let then_branch = self.statement()?;

        let else_branch = rule!(next_if_matches(self): token::Type::Else)
            .map(|_| self.statement())
            .transpose()?;

        Ok(ast::stmt::If {
            condition,
            then_branch,
            else_branch,
        }
        .into())
    }

    fn while_statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        rule!(
            consume(self): token::Type::LeftParen,
            "expect `(` after `while`"
        )?;

        let condition = self.expression()?;

        rule!(
            consume(self): token::Type::RightParen,
            "expect `)` after while condition"
        )?;

        let body = self.statement()?;

        Ok(ast::stmt::While { condition, body }.into())
    }

    fn for_statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        rule!(
            consume(self): token::Type::LeftParen,
            "expect `(` after `for`"
        )?;

        let initializer = if rule!(next_if_matches(self): token::Type::Semicolon).is_some() {
            None
        } else if rule!(next_if_matches(self): token::Type::Var).is_some() {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if rule!(!peek_matches(self): token::Type::Semicolon).unwrap_or(true) {
            self.expression()?
        } else {
            ast::expr::Literal {
                literal: token::Literal {
                    typ: token::LiteralType::True,
                    info: self.tokens.peek().unwrap().info.clone(),
                },
            }
            .into()
        };

        rule!(
            consume(self): token::Type::Semicolon,
            "expect `;` after loop condition"
        )?;

        let increment = if rule!(!peek_matches(self): token::Type::RightParen).unwrap_or(true) {
            Some(self.expression()?)
        } else {
            None
        };

        rule!(
            consume(self): token::Type::RightParen,
            "expect `)` after for clauses"
        )?;

        let while_body: ast::Stmt = {
            let body = self.statement()?;

            if let Some(inc) = increment {
                ast::stmt::Block {
                    statements: vec![body, inc.into()],
                }
                .into()
            } else {
                body
            }
        };

        let while_stmt = ast::stmt::While {
            condition,
            body: while_body,
        }
        .into();

        Ok(if let Some(init) = initializer {
            ast::stmt::Block {
                statements: vec![init, while_stmt],
            }
            .into()
        } else {
            while_stmt
        })
    }

    fn expression_statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        let expression = self.expression()?;

        if self.allow_expr && self.tokens.peek().is_none() {
            self.found_expr = true;
        } else {
            rule!(statement_end(self))?;
        }

        Ok(ast::stmt::Expression { expression }.into())
    }

    fn block(&mut self) -> (Vec<ast::Stmt<'s>>, Option<error_stack::Report<ParseError>>) {
        let mut statements = Vec::new();
        let mut error: Option<error_stack::Report<ParseError>> = None;

        while rule!(!peek_matches(self): token::Type::RightBrace).unwrap_or(false) {
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
        let expr = self.or()?;

        if let Some(token::Token {
            info: equals_info, ..
        }) = rule!(next_if_matches(self): token::Type::Equal)
        {
            let value = self.assignment()?;

            if let ast::Expr::Variable(var) = expr {
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

        Ok(expr)
    }

    rule!(binary(ast::expr::Logical): or => and, token::Type::Or);
    rule!(binary(ast::expr::Logical): and => equality, token::Type::And);
    rule!(binary(ast::expr::Binary): equality => comparision, token::Type::BangEqual | token::Type::EqualEqual);
    rule!(binary(ast::expr::Binary): comparision => term,
        token::Type::Greater | token::Type::GreaterEqual | token::Type::Less | token::Type::LessEqual);
    rule!(binary(ast::expr::Binary): term => factor, token::Type::Minus | token::Type::Plus);
    rule!(binary(ast::expr::Binary): factor => unary, token::Type::Slash | token::Type::Star);

    fn unary(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        if let Some(operator) = rule!(next_if_matches(self): token::Type::Bang | token::Type::Minus)
        {
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
        }) = rule!(next_if_matches(self): token::Type::Literal(_))
        {
            return Ok(ast::expr::Literal {
                literal: token::Literal { typ, info },
            }
            .into());
        }

        if let Some(token::Token { info, .. }) =
            rule!(next_if_matches(self): token::Type::Identifier)
        {
            return Ok(ast::expr::Variable { info }.into());
        }

        if rule!(next_if_matches(self): token::Type::LeftParen).is_some() {
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

        _ = rule!(next_if_matches(self): token::Type::Semicolon);
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

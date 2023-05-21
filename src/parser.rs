use std::fmt;

use itertools::{Itertools as _, PeekingNext as _};

use crate::{ast, into_owned::IntoOwned as _, token};

#[derive(Debug)]
pub struct ParseError {
    info: Option<token::Info<'static>>,
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

    if let Some(error) = error {
        parser.report(error);
    }

    (parser.found_expr, statements, parser.error)
}

struct Parser<'s, Tokens>
where
    Tokens: Iterator<Item = token::Token<'s>>,
{
    tokens: itertools::PeekNth<Tokens>,
    error: Option<error_stack::Report<ParseError>>,
    allow_expr: bool,
    found_expr: bool,
    break_depth: usize,
}

macro_rules! matches_token {
    ($token:expr, $pattern:pat) => {{
        matches!($token, token::Token { typ: $pattern, .. })
    }};
}

macro_rules! rule {
    (next_if_matches($self:ident): $token_pat:pat) => {
        $self.tokens.peeking_next(|t| matches_token!(t, $token_pat))
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

    (peek_nth_matches($self:ident, $n:expr): $token_pat:pat) => {
        $self.tokens
            .peek_nth($n)
            .map(|t| matches_token!(t, $token_pat))
    };

    (infix($($expr_type:tt)*): $rule:ident => $next_rule:ident, $token_pat:pat) => {
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
            Err($self.error().attach_printable($error_msg))
        }
    };

    (statement_end($self:ident): $stmt:literal) => {
        rule!(
            consume($self): token::Type::Semicolon,
            concat!("expect `;` after ", $stmt)
        )
    };
}

impl<'s, Tokens> Parser<'s, Tokens>
where
    Tokens: Iterator<Item = token::Token<'s>>,
{
    pub fn new(tokens: Tokens, allow_expr: bool) -> Self {
        Self {
            tokens: itertools::peek_nth(tokens),
            error: None,
            allow_expr,
            found_expr: false,
            break_depth: 0,
        }
    }

    fn declaration(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        if rule!(next_if_matches(self): token::Type::Var).is_some() {
            return self.var_declaration();
        }

        if rule!(peek_matches(self): token::Type::Fun).unwrap_or(false)
            && rule!(peek_nth_matches(self, 1): token::Type::Identifier).unwrap_or(false)
        {
            let keyword = self.tokens.next().unwrap();
            return self.function(keyword, "function");
        }

        self.statement()
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

        rule!(statement_end(self): "variable declaration")?;

        Ok(ast::stmt::Var { info, initializer }.into())
    }

    fn statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        if rule!(next_if_matches(self): token::Type::If).is_some() {
            self.if_statement()
        } else if rule!(next_if_matches(self): token::Type::While).is_some() {
            self.while_statement()
        } else if rule!(next_if_matches(self): token::Type::For).is_some() {
            self.for_statement()
        } else if let Some(keyword) = rule!(next_if_matches(self): token::Type::Break) {
            self.break_statement(keyword)
        } else if let Some(keyword) = rule!(next_if_matches(self): token::Type::Return) {
            self.return_statement(keyword)
        } else if rule!(next_if_matches(self): token::Type::Print).is_some() {
            self.print_statement()
        } else if rule!(next_if_matches(self): token::Type::LeftBrace).is_some() {
            let (block, error) = self.block();

            if let Some(error) = error {
                Err(error)
            } else {
                Ok(block.into())
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

        let body = {
            self.break_depth += 1;
            let result = self.statement();
            self.break_depth -= 1;
            result?
        };

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

        rule!(statement_end(self): "loop condition")?;

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
            let body = {
                self.break_depth += 1;
                let result = self.statement();
                self.break_depth -= 1;
                result?
            };

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

    fn break_statement(
        &mut self,
        keyword: token::Token<'s>,
    ) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        rule!(statement_end(self): "`break`")?;

        if self.break_depth == 0 {
            self.report(
                error_stack::report!(ParseError {
                    info: Some(keyword.info.clone().into_owned()),
                })
                .attach_printable("`break` can only be used in a loop"),
            );
        }

        Ok(ast::stmt::Break { info: keyword.info }.into())
    }

    fn return_statement(
        &mut self,
        keyword: token::Token<'s>,
    ) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        let value = if rule!(!peek_matches(self): token::Type::Semicolon).unwrap_or(false) {
            Some(self.expression()?)
        } else {
            None
        };

        rule!(statement_end(self): "return value")?;

        Ok(ast::stmt::Return {
            keyword: keyword.info,
            value,
        }
        .into())
    }

    fn expression_statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        let expression = self.expression()?;

        if self.allow_expr && self.tokens.peek().is_none() {
            self.found_expr = true;
        } else {
            rule!(statement_end(self): "expression")?;
        }

        Ok(ast::stmt::Expression { expression }.into())
    }

    fn function(
        &mut self,
        keyword: token::Token<'s>,
        kind: &str,
    ) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        let name = rule!(
            consume(self): token::Type::Identifier,
            format!("expect {kind} name")
        )?
        .info;

        Ok(ast::stmt::Function {
            name,
            function: self.function_body(keyword, kind)?,
        }
        .into())
    }

    fn function_body(
        &mut self,
        keyword: token::Token<'s>,
        kind: &str,
    ) -> error_stack::Result<ast::expr::Function<'s>, ParseError> {
        rule!(
            consume(self): token::Type::LeftParen,
            format!("expect `(` after {kind} name")
        )?;

        let mut parameters = Vec::new();
        if rule!(!peek_matches(self): token::Type::RightParen).unwrap_or(false) {
            loop {
                if parameters.len() >= 255 {
                    let report = self
                        .error()
                        .attach_printable("cannot have more than 255 arguments");
                    self.report(report)
                }

                parameters.push(
                    rule!(
                        consume(self): token::Type::Identifier,
                        "expect parameter name"
                    )?
                    .info,
                );

                if rule!(next_if_matches(self): token::Type::Comma).is_none() {
                    break;
                }
            }
        }

        rule!(
            consume(self): token::Type::RightParen,
            "expect `)` after parameters"
        )?;

        rule!(
            consume(self): token::Type::LeftBrace,
            format!("expect `{{` before {kind} body")
        )?;

        let (body, error) = self.block();
        if let Some(error) = error {
            return Err(error);
        }

        Ok(ast::expr::Function {
            keyword: keyword.info,
            parameters,
            body,
        })
    }

    fn block(
        &mut self,
    ) -> (
        ast::stmt::Block<'s>,
        Option<error_stack::Report<ParseError>>,
    ) {
        let mut statements = Vec::new();
        let mut error: Option<error_stack::Report<ParseError>> = None;

        while rule!(!peek_matches(self): token::Type::RightBrace).unwrap_or(false) {
            let result = self.declaration();
            match result {
                Ok(stmt) => statements.push(stmt),
                Err(report) => match &mut error {
                    Some(error) => error.extend_one(report),
                    None => error = Some(report),
                },
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

        (ast::stmt::Block { statements }, error)
    }

    fn print_statement(&mut self) -> error_stack::Result<ast::Stmt<'s>, ParseError> {
        let expression = self.expression()?;
        rule!(statement_end(self): "value")?;
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

            self.report(
                error_stack::report!(ParseError {
                    info: Some(equals_info.into_owned()),
                })
                .attach_printable("invalid assignment target"),
            );
        }

        Ok(expr)
    }

    rule!(infix(ast::expr::Logical): or => and, token::Type::Or);
    rule!(infix(ast::expr::Logical): and => equality, token::Type::And);
    rule!(infix(ast::expr::Binary): equality => comparision, token::Type::BangEqual | token::Type::EqualEqual);
    rule!(infix(ast::expr::Binary): comparision => term,
        token::Type::Greater | token::Type::GreaterEqual | token::Type::Less | token::Type::LessEqual);
    rule!(infix(ast::expr::Binary): term => factor, token::Type::Minus | token::Type::Plus);
    rule!(infix(ast::expr::Binary): factor => unary, token::Type::Slash | token::Type::Star);

    fn unary(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        if let Some(operator) = rule!(next_if_matches(self): token::Type::Bang | token::Type::Minus)
        {
            Ok(ast::expr::Unary {
                operator,
                right: self.unary()?,
            }
            .into())
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> error_stack::Result<ast::Expr<'s>, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if rule!(next_if_matches(self): token::Type::LeftParen).is_some() {
                let callee = expr;
                let mut arguments = Vec::new();

                if rule!(!peek_matches(self): token::Type::RightParen).unwrap_or(false) {
                    loop {
                        if arguments.len() >= 255 {
                            let report = self
                                .error()
                                .attach_printable("cannot have more than 255 arguments");
                            self.report(report)
                        }

                        arguments.push(self.expression()?);

                        if rule!(next_if_matches(self): token::Type::Comma).is_none() {
                            break;
                        }
                    }
                }

                let right_paren = rule!(
                    consume(self): token::Type::RightParen,
                    "expect `)` after arguments"
                )?;

                expr = ast::expr::Call {
                    callee,
                    right_paren,
                    arguments,
                }
                .into();
            } else {
                break;
            }
        }

        Ok(expr)
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

        if let Some(keyword) = rule!(next_if_matches(self): token::Type::Fun) {
            return Ok(self.function_body(keyword, "function")?.into());
        }

        Err(self.error().attach_printable("expect expression"))
    }

    fn error(&mut self) -> error_stack::Report<ParseError> {
        error_stack::report!(ParseError {
            info: self.tokens.next().map(|t| t.info.into_owned()),
        })
    }

    fn report(&mut self, report: error_stack::Report<ParseError>) {
        match self.error.as_mut() {
            Some(r) => r.extend_one(report),
            None => self.error = Some(report),
        }
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
            Some(self.declaration().map_err(|e| {
                self.synchronize();
                e
            }))
        } else {
            None
        }
    }
}

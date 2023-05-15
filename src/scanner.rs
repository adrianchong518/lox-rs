use std::{fmt, str};

use error_stack::{IntoReport, ResultExt};
use itertools::{Itertools as _, PeekingNext as _};

/// Scanner used for lexing the provided `source` into an [`Iterator`] of [`Token`]s
#[derive(Debug)]
pub struct Scanner<'s> {
    /// Original source code
    source: &'s str,

    /// A [peekable](iter::Peekable) iterator into characters of `source`
    char_indices: itertools::PeekNth<str::CharIndices<'s>>,

    /// Current line number
    current_line: usize,
}

impl<'s> Scanner<'s> {
    /// Create a new `Scanner` from `source`
    pub fn new(source: &'s str) -> Self {
        Self {
            source,
            char_indices: itertools::peek_nth(source.char_indices()),
            current_line: 1,
        }
    }

    /// Helper to create a new [`Token`] from its `typ` and location in `source`
    fn new_token(&self, typ: TokenType<'s>, token_start: usize, token_end: usize) -> Token<'s> {
        Token {
            typ,
            lexeme: &self.source[token_start..=token_end],
            line: self.current_line,
        }
    }
}

impl<'s> Iterator for Scanner<'s> {
    type Item = error_stack::Result<Token<'s>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        let next_token = 'tok: loop {
            let (i, c) = self.char_indices.next()?;
            let token_start = i;

            match c {
                // These tokens can be easily matched against
                '(' => break self.new_token(TokenType::LeftParen, token_start, i),
                ')' => break self.new_token(TokenType::RightParen, token_start, i),
                '{' => break self.new_token(TokenType::LeftBrace, token_start, i),
                '}' => break self.new_token(TokenType::RightBrace, token_start, i),
                ',' => break self.new_token(TokenType::Comma, token_start, i),
                '.' => break self.new_token(TokenType::Dot, token_start, i),
                '-' => break self.new_token(TokenType::Minus, token_start, i),
                '+' => break self.new_token(TokenType::Plus, token_start, i),
                ';' => break self.new_token(TokenType::Semicolon, token_start, i),
                '*' => break self.new_token(TokenType::Star, token_start, i),

                // These tokens requires peeking at the next character to determine the token at hand
                '!' => {
                    if let Some((i, _)) = self.char_indices.peeking_next(|&(_, c)| c == '=') {
                        break self.new_token(TokenType::BangEqual, token_start, i);
                    } else {
                        break self.new_token(TokenType::Bang, token_start, i);
                    }
                }

                '=' => {
                    if let Some((i, _)) = self.char_indices.peeking_next(|&(_, c)| c == '=') {
                        break self.new_token(TokenType::EqualEqual, token_start, i);
                    } else {
                        break self.new_token(TokenType::Equal, token_start, i);
                    }
                }

                '<' => {
                    if let Some((i, _)) = self.char_indices.peeking_next(|&(_, c)| c == '=') {
                        break self.new_token(TokenType::LessEqual, token_start, i);
                    } else {
                        break self.new_token(TokenType::Less, token_start, i);
                    }
                }

                '>' => {
                    if let Some((i, _)) = self.char_indices.peeking_next(|&(_, c)| c == '=') {
                        break self.new_token(TokenType::GreaterEqual, token_start, i);
                    } else {
                        break self.new_token(TokenType::Greater, token_start, i);
                    }
                }

                // The `/` token is special as comments also start with `//`
                '/' => {
                    if self.char_indices.peeking_next(|&(_, c)| c == '/').is_some() {
                        // Consumes all characters until a newline
                        self.char_indices
                            .peeking_take_while(|&(_, c)| c != '\n')
                            .for_each(drop);
                    } else {
                        break self.new_token(TokenType::Slash, token_start, i);
                    }
                }

                // String literals (supports multiline strings)
                '"' => {
                    let mut literal = String::new();
                    for (i, c) in self.char_indices.by_ref() {
                        match c {
                            // Special care is required for `\n` as the `current_line` needs to be
                            // updated
                            '\n' => {
                                self.current_line += 1;
                                literal.push('\n');
                            }

                            // Literal ends when a `"` is hit
                            '"' => {
                                break 'tok self.new_token(
                                    TokenType::String(literal),
                                    token_start,
                                    i,
                                );
                            }
                            c => literal.push(c),
                        }
                    }

                    return Some(Err(error_stack::report!(SyntaxError {
                        line: self.current_line
                    })
                    .attach_printable("unterminated string literal")));
                }

                // Numeric literals
                d if d.is_ascii_digit() => {
                    let literal_start = token_start;
                    let mut literal_end = self
                        .char_indices
                        .take_while_ref(|&(_, c)| c.is_ascii_digit())
                        .last()
                        .map_or(token_start, |(i, _)| i);

                    let has_dot = self
                        .char_indices
                        .peek_nth(0)
                        .map_or(false, |&(_, c)| c == '.');
                    if has_dot {
                        let has_fraction = self
                            .char_indices
                            .peek_nth(1)
                            .map_or(false, |&(_, c)| c.is_ascii_digit());
                        if has_fraction {
                            _ = self.char_indices.next();

                            literal_end = self
                                .char_indices
                                .take_while_ref(|&(_, c)| c.is_ascii_digit())
                                .last()
                                .map_or(token_start, |(i, _)| i);
                        }
                    }

                    let literal = &self.source[literal_start..=literal_end];
                    match literal.parse::<f64>() {
                        Ok(literal) => {
                            break self.new_token(
                                TokenType::Number(literal),
                                literal_start,
                                literal_end,
                            )
                        }
                        Err(e) => {
                            return Some(
                                Err(e)
                                    .into_report()
                                    .change_context(SyntaxError::new(self.current_line))
                                    .attach_printable_lazy(|| {
                                        format!("unable to parse {literal:?} as `f64`")
                                    }),
                            )
                        }
                    }
                }

                c if is_identifier_start(c) => {
                    let start = token_start;
                    let end = self
                        .char_indices
                        .take_while_ref(|&(_, c)| is_identifier(c))
                        .last()
                        .map_or(token_start, |(i, _)| i);

                    let text = &self.source[start..=end];

                    break if let Some(typ) = TokenType::try_from_keyword(text) {
                        self.new_token(typ, start, end)
                    } else {
                        self.new_token(TokenType::Identifier(text), start, end)
                    };
                }

                // Increment line number for a newline
                '\n' => self.current_line += 1,
                // Ignore all other whitespace
                _ if c.is_whitespace() => {}

                _ => {
                    return Some(Err(error_stack::report!(SyntaxError::new(
                        self.current_line
                    ))
                    .attach_printable(format!("unexpected character {c:?}"))));
                }
            }
        };

        Some(Ok(next_token))
    }
}

fn is_identifier_start(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_identifier(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

#[derive(Debug)]
pub struct SyntaxError {
    line: usize,
}

impl SyntaxError {
    fn new(line: usize) -> Self {
        SyntaxError { line }
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] syntax error", self.line)
    }
}

impl error_stack::Context for SyntaxError {}

#[derive(Debug)]
pub struct Token<'s> {
    typ: TokenType<'s>,
    lexeme: &'s str,
    line: usize,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum TokenType<'s> {
    // single character tokens (with no ambiguity)
    /// `(`
    LeftParen,
    /// `)`
    RightParen,
    /// `{`
    LeftBrace,
    /// `}`
    RightBrace,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `-`
    Minus,
    /// `+`
    Plus,
    /// `;`
    Semicolon,
    /// `/`
    Slash,
    /// `*`
    Star,

    // 1/2 character tokens (with ambiguity)
    /// `!`
    Bang,
    /// `!=`
    BangEqual,
    /// `=`
    Equal,
    /// `==`
    EqualEqual,
    /// `>`
    Greater,
    /// `>=`
    GreaterEqual,
    /// `<`
    Less,
    /// `<=`
    LessEqual,

    Identifier(&'s str),
    String(String),
    Number(f64),

    /// `and`
    And,
    /// `class`
    Class,
    /// `else`
    Else,
    /// `false`
    False,
    /// `fun`
    Fun,
    /// `for`
    For,
    /// `if`
    If,
    /// `nil`
    Nil,
    /// `or`
    Or,
    /// `print`
    Print,
    /// `return`
    Return,
    /// `super`
    Super,
    /// `this`
    This,
    /// `true`
    True,
    /// `var`
    Var,
    /// `while`
    While,
}

impl<'s> TokenType<'s> {
    fn try_from_keyword(text: &str) -> Option<Self> {
        Some(match text {
            "and" => Self::And,
            "class" => Self::Class,
            "else" => Self::Else,
            "false" => Self::False,
            "for" => Self::For,
            "fun" => Self::Fun,
            "if" => Self::If,
            "nil" => Self::Nil,
            "or" => Self::Or,
            "print" => Self::Print,
            "return" => Self::Return,
            "super" => Self::Super,
            "this" => Self::This,
            "true" => Self::True,
            "var" => Self::Var,
            "while" => Self::While,
            _ => return None,
        })
    }
}

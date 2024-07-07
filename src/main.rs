use std::fmt::{self, Display, Formatter};

use colored::Colorize;
use human_panic::{setup_panic, Metadata};
use itertools::Itertools;
use rustyline::{error::ReadlineError, DefaultEditor};
use thiserror::Error;

fn main() {
    setup_panic!(Metadata::new(
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION")
    ));
    eprintln!(
        "{} {} {}{}",
        "starting".dimmed(),
        env!("CARGO_PKG_NAME").truecolor(255, 136, 0),
        env!("CARGO_PKG_VERSION").dimmed(),
        "...".dimmed()
    );
    let mut readline = DefaultEditor::new().unwrap();
    loop {
        let line = {
            match readline.readline(&concat!(env!("CARGO_PKG_NAME"), " > ").green().to_string()) {
                Ok(line) => {
                    readline.add_history_entry(line.as_str()).unwrap();
                    line
                }
                Err(ReadlineError::Interrupted) => {
                    eprintln!("interrupted (probably by ctrl-c). bye!");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    eprintln!("read EOF (probably by ctrl-d). bye!");
                    break;
                }
                Err(err) => {
                    eprintln!("unexpected error while reading\n\n{err:?}\n\nbye!");
                    break;
                }
            }
        };
        let tokens = match Token::tokenise(&line) {
            Ok(tokens) => tokens,
            Err(error) => {
                eprintln!("{}", format!("{error}").red());
                continue;
            }
        };
        match rep(&tokens) {
            Ok(output) => {
                println!("{output}");
            }
            Err(error) => {
                eprintln!("{}", format!("{error}").red());
            }
        }
    }
}

#[allow(clippy::missing_const_for_fn)]
fn eval(input: Type) -> Type {
    input
}

fn print(input: &Type) -> String {
    input.to_string()
}

fn rep(input: &[Token]) -> Result<String, Error> {
    Ok(print(&eval(Type::from_tokens(input)?.0)))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Balancing {
    Balanced,
    Unbalanced,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Token {
    TildeAt,
    LeftSquare,
    RightSquare,
    LeftBrace,
    RightBrace,
    LeftRound,
    RightRound,
    SingleQuote,
    Backtick,
    Tilde,
    Caret,
    At,
    String(String, Balancing),
    Normal(String),
}

impl Token {
    const TILDE_AT: &'static str = "~@";
    const WHITE_SPACE: [char; 6] = ['\r', '\n', '\t', '\x0c', '\x0b', ' '];
    const IGNORE: [char; 7] = ['\r', '\n', '\t', '\x0c', '\x0b', ' ', ','];

    const fn from_special(char: char) -> Option<Self> {
        match char {
            '[' => Some(Self::LeftSquare),
            ']' => Some(Self::RightSquare),
            '{' => Some(Self::LeftBrace),
            '}' => Some(Self::RightBrace),
            '(' => Some(Self::LeftRound),
            ')' => Some(Self::RightRound),
            '\'' => Some(Self::SingleQuote),
            '`' => Some(Self::Backtick),
            '~' => Some(Self::Tilde),
            '^' => Some(Self::Caret),
            '@' => Some(Self::At),
            _ => None,
        }
    }

    fn tokenise(mut input: &str) -> Result<Vec<Self>, Error> {
        let mut tokens = Vec::new();
        'outer: loop {
            input = input.strip_prefix(Self::IGNORE).unwrap_or(input);
            if let Some(rest) = input.strip_prefix(Self::TILDE_AT) {
                input = rest;
                tokens.push(Self::TildeAt);
                continue;
            }
            let next = {
                let Some((char, rest)) = input.split_at_checked(1) else {
                    break;
                };
                input = rest;
                char.chars().exactly_one().unwrap()
            };
            if let Some(token) = Self::from_special(next) {
                tokens.push(token);
                continue;
            }
            if next == '"' {
                let mut token = String::new();
                let mut chars = input.chars();
                loop {
                    let next = chars.clone().next().ok_or(Error::UnbalancedString)?;
                    match next {
                        '\\' => {
                            let after = chars.clone().nth(1).ok_or(Error::UnbalancedString)?;
                            token.push(match after {
                                'n' => '\n',
                                '"' => '\"',
                                '\\' => '\\',
                                after => return Err(Error::BadEscape(format!("\\{after}"))),
                            });
                            chars.next().ok_or(Error::UnbalancedString)?;
                            chars.next().ok_or(Error::UnbalancedString)?;
                        }
                        next if next != '"' => {
                            token.push(next);
                            chars.next().ok_or(Error::UnbalancedString)?;
                        }
                        _ => break,
                    }
                }
                let next = chars.next();
                let balancing = match next {
                    Some('"') => Balancing::Balanced,
                    Some(_) | None => Balancing::Unbalanced,
                };
                let token = Self::String(token, balancing);
                tokens.push(token);
                input = &input[input.len() - chars.count()..];
                continue;
            }
            if next == ';' {
                input = input.split_once('\n').map_or("", |(_, rest)| rest);
                continue;
            }
            if Self::WHITE_SPACE.contains(&next)
                || matches!(
                    next,
                    '[' | ']' | '{' | '}' | '(' | '\'' | '"' | '`' | ',' | ';' | ')'
                )
            {
                continue;
            }
            let mut token = String::new();
            token.push(next);
            loop {
                let next = {
                    let Some((char, rest)) = input.split_at_checked(1) else {
                        tokens.push(Self::Normal(token));
                        break 'outer;
                    };
                    input = rest;
                    char.chars().exactly_one().unwrap()
                };
                if Self::WHITE_SPACE.contains(&next)
                    || matches!(
                        next,
                        '[' | ']' | '{' | '}' | '(' | '\'' | '"' | '`' | ',' | ';' | ')'
                    )
                {
                    tokens.push(Self::Normal(token));
                    tokens.extend(Self::tokenise(&next.to_string())?);
                    continue 'outer;
                }
                token.push(next);
            }
        }
        Ok(tokens)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::String(string, balancing) => format!(
                    "\"{string}{}",
                    match balancing {
                        Balancing::Balanced => "\"",
                        Balancing::Unbalanced => "",
                    }
                ),
                token => {
                    match token {
                        Self::TildeAt => "~@",
                        Self::LeftSquare => "[",
                        Self::RightSquare => "]",
                        Self::LeftBrace => "{",
                        Self::RightBrace => "}",
                        Self::LeftRound => "(",
                        Self::RightRound => ")",
                        Self::SingleQuote => "'",
                        Self::Backtick => "`",
                        Self::Tilde => "~",
                        Self::Caret => "^",
                        Self::At => "@",
                        Self::Normal(string) => string,
                        Self::String(_, _) => unreachable!(),
                    }
                    .to_string()
                }
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
struct List(Vec<Type>);

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Compound(List),
    Symbol(Token),
    String(String),
    Number(f64),
    Nil,
    True,
    False,
}

#[derive(Debug, Error)]
enum Error {
    #[error("unexpected end-of-file while parsing")]
    UnexpectedEof,
    #[error("string unbalanced (missing closing `\"`)")]
    UnbalancedString,
    #[error(r#"bad escape (expected one of '\n', '\\', '\"', got {0})"#)]
    BadEscape(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", {
            match self {
                Self::Number(number) => number.to_string(),
                Self::String(string) => format!("\"{string}\""),
                Self::Symbol(symbol) => symbol.to_string(),
                Self::Nil => "nil".to_string(),
                Self::True => "true".to_string(),
                Self::False => "false".to_string(),
                Self::Compound(List(items)) => format!(
                    "({})",
                    items
                        .iter()
                        .map(ToString::to_string)
                        .collect_vec()
                        .join(" ")
                ),
            }
        })
    }
}

impl Type {
    fn from_tokens(input: &[Token]) -> Result<(Self, usize), Error> {
        let first = input.first().ok_or(Error::UnexpectedEof)?;
        Ok(match first {
            Token::LeftRound => {
                let (list, len) = List::from_tokens(input)?;
                (Self::Compound(list), len)
            }
            Token::SingleQuote | Token::Backtick | Token::Tilde | Token::TildeAt => {
                let (parameter, len) = Self::from_tokens(&input[1..])?;
                (
                    Self::Compound(List(vec![
                        Self::Symbol(Token::Normal(
                            match first {
                                Token::SingleQuote => "quote",
                                Token::Backtick => "quasiquote",
                                Token::Tilde => "unquote",
                                Token::TildeAt => "splice-unquote",
                                _ => unreachable!(),
                            }
                            .to_string(),
                        )),
                        parameter,
                    ])),
                    len + 1,
                )
            }
            _ => (
                match first {
                    Token::String(ref string, balancing) => match balancing {
                        Balancing::Balanced => Ok(Self::String(string.clone())),
                        Balancing::Unbalanced => Err(Error::UnbalancedString),
                    },
                    Token::Normal(string) => Ok(string.parse::<f64>().map_or(
                        match string.as_str() {
                            "nil" => Self::Nil,
                            "true" => Self::True,
                            "false" => Self::False,
                            _ => Self::Symbol(Token::Normal(string.clone())),
                        },
                        Self::Number,
                    )),
                    token => Ok(Self::Symbol(token.clone())),
                }?,
                1,
            ),
        })
    }
}

impl List {
    fn from_tokens(input: &[Token]) -> Result<(Self, usize), Error> {
        let original_len = input.len();
        let mut input = &input[1..];
        let mut items = Vec::new();
        loop {
            if *input.first().ok_or(Error::UnexpectedEof)? == Token::RightRound {
                break;
            }
            let (item, len) = Type::from_tokens(input)?;
            input = &input[len..];
            items.push(item);
        }
        Ok((Self(items), original_len - input.len() + 1))
    }
}

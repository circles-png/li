use colored::Colorize;
use human_panic::{setup_panic, Metadata};
use indexmap::IndexMap;
use itertools::Itertools;
use rustyline::{error::ReadlineError, history::FileHistory, DefaultEditor, Editor};
use std::{
    collections::HashMap, env, fmt::{self, Debug, Display, Formatter}, rc::Rc, string::String
};
use thiserror::Error;

#[derive(Clone)]
struct Environment {
    outer: Option<Box<Self>>,
    data: HashMap<Token, Type>,
}

impl Environment {
    fn new() -> Self {
        Self {
            outer: None,
            data: HashMap::new(),
        }
    }

    fn set(&mut self, symbol: Token, value: Type) -> Option<Type> {
        self.data.insert(symbol, value)
    }

    fn find(&self, symbol: &Token) -> Option<Self> {
        if self.data.contains_key(symbol) {
            return Some(self.clone());
        } else if let Some(outer) = &self.outer {
            return outer.find(symbol);
        }
        None
    }

    fn get(&self, symbol: &Token) -> Option<Type> {
        self.find(symbol)?.data.get(symbol).cloned()
    }
}

struct Repl {
    readline: Editor<(), FileHistory>,
    environment: Environment,
}

impl Repl {
    fn new() -> Self {
        setup_panic!(Metadata::new(
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION")
        ));
        eprintln!(
            "{} {} {}{}",
            "starting".dimmed(),
            env!("CARGO_PKG_NAME").green(),
            env!("CARGO_PKG_VERSION").dimmed(),
            "...".dimmed()
        );
        let readline = DefaultEditor::new().unwrap();
        let mut environment = Environment::new();
        for (symbol, op) in [
            ("+", &|a, b| a + b),
            ("-", &|a, b| a - b),
            ("*", &|a, b| a * b),
            ("/", &|a, b| a / b),
        ] as [(&str, &dyn Fn(f64, f64) -> f64); 4]
        {
            environment.set(
                Token::Other(symbol.to_string()),
                Type::Function(Rc::new(move |values| {
                    Type::Number(
                        values
                            .iter()
                            .map(|value| *value.as_number().unwrap())
                            .reduce(op)
                            .unwrap(),
                    )
                })),
            );
        }
        Self {
            readline,
            environment,
        }
    }

    fn run(&mut self) {
        loop {
            let line = {
                match self
                    .readline
                    .readline(&concat!(env!("CARGO_PKG_NAME"), " > ").green().to_string())
                {
                    Ok(line) => {
                        self.readline.add_history_entry(line.as_str()).unwrap();
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

            match rep(&tokens, &mut self.environment) {
                Ok(output) => {
                    println!("{output}");
                }
                Err(error) => {
                    eprintln!("{}", format!("{error}").red());
                }
            }
        }
    }
}

fn main() {
    Repl::new().run();
}

#[derive(Clone)]
enum EvalResult {
    ResultList(Vec<EvalResult>),
    Type(Type),
}

impl Debug for EvalResult {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ResultList(list) => Debug::fmt(list, f),
            Self::Type(value) => Debug::fmt(value, f),
        }
    }
}

impl EvalResult {
    const fn as_type(&self) -> Option<&Type> {
        if let Self::Type(v) = self {
            Some(v)
        } else {
            None
        }
    }

    const fn as_result_list(&self) -> Option<&Vec<Self>> {
        if let Self::ResultList(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[allow(clippy::too_many_lines)]
fn eval(ast: Type, environment: &mut Environment) -> Result<EvalResult, Error> {
    fn eval(ast: Type, environment: &mut Environment) -> Result<EvalResult, Error> {
        Ok(match ast {
            Type::List(ref list) if list.is_empty() => EvalResult::Type(ast),
            Type::List(_) => {
                let unevaluated = ast.as_list().unwrap();
                match unevaluated
                    .first()
                    .unwrap()
                    .as_symbol()
                    .and_then(Token::as_other)
                    .map(String::as_str)
                {
                    Some("def!") => {
                        let result = eval(unevaluated.get(2).unwrap().clone(), environment)?;
                        environment.set(
                            unevaluated.get(1).unwrap().as_symbol().unwrap().clone(),
                            result.as_type().unwrap().clone(),
                        );
                        result
                    }
                    Some("let*") => {
                        let mut new = Environment::new();
                        new.outer = Some(Box::new(environment.clone()));
                        let pairs = match unevaluated.get(1).unwrap() {
                            Type::List(list) => list,
                            Type::Vector(vector) => vector,
                            _ => todo!("error"),
                        }
                        .chunks_exact(2);
                        if !pairs.remainder().is_empty() {
                            todo!("this should be an error!!")
                        }
                        for pair in pairs {
                            match pair {
                                [name, value] => {
                                    let value =
                                        eval(value.clone(), &mut new)?.as_type().unwrap().clone();
                                    new.set(name.as_symbol().unwrap().clone(), value);
                                }
                                _ => todo!("error"),
                            }
                        }

                        eval(unevaluated.get(2).unwrap().clone(), &mut new)?
                    }
                    Some("do") => eval_ast(Type::List(unevaluated[1..].to_vec()), environment)?,
                    Some("if") => {
                        if match eval(unevaluated.get(1).unwrap().clone(), environment)? {
                            EvalResult::Type(Type::Nil | Type::False) => false,
                            EvalResult::Type(_) => true,
                            EvalResult::ResultList(_) => todo!(),
                        } {
                            eval(unevaluated.get(2).unwrap().clone(), environment)?
                        } else {
                            let get = unevaluated.get(3);
                            match get {
                                Some(get) => eval(get.clone(), environment)?,
                                None => EvalResult::Type(Type::Nil),
                            }
                        }
                    }
                    Some("fn*") => {
                        let environment = environment.clone();
                        let unevaluated = unevaluated.clone();
                        EvalResult::Type(Type::Function(Rc::new(move |values| {
                            let mut new = Environment::new();
                            new.outer = Some(Box::new(environment.clone()));
                            for (bind, expr) in unevaluated
                                .get(1)
                                .unwrap()
                                .as_list()
                                .unwrap()
                                .iter()
                                .map(|value| value.as_symbol().unwrap().clone())
                                .zip_eq(values)
                            {
                                new.set(bind, expr.clone());
                            }
                            eval(unevaluated.get(2).unwrap().clone(), &mut new)
                                .unwrap()
                                .as_type()
                                .unwrap()
                                .clone()
                        })))
                    }
                    _ => {
                        let result = eval_ast(ast.clone(), environment)?;
                        let evaluated = result.as_result_list().unwrap();
                        EvalResult::Type(Rc::clone(
                            evaluated
                                .first()
                                .unwrap()
                                .as_type()
                                .unwrap()
                                .as_function()
                                .unwrap(),
                        )(
                            &evaluated
                                .iter()
                                .skip(1)
                                .map(|item| item.as_type().unwrap().clone())
                                .collect_vec(),
                        ))
                    }
                }
            }
            _ => eval_ast(ast, environment)?,
        })
    }
    fn eval_ast(ast: Type, environment: &mut Environment) -> Result<EvalResult, Error> {
        Ok(match ast {
            Type::Symbol(symbol) => EvalResult::Type(
                environment
                    .get(&symbol)
                    .ok_or(Error::NotFound(symbol.to_string()))?,
            ),
            Type::List(list) => EvalResult::ResultList(
                list.iter()
                    .map(|item| eval(item.clone(), environment))
                    .try_collect()?,
            ),
            Type::Vector(vector) => EvalResult::Type(Type::Vector(
                vector
                    .into_iter()
                    .map(|item| {
                        eval(item, environment).map(|result| result.as_type().unwrap().clone())
                    })
                    .try_collect()?,
            )),
            Type::HashMap(hash_map) => EvalResult::Type(Type::HashMap(
                hash_map
                    .into_iter()
                    .map(|(key, value)| {
                        eval(value, environment)
                            .map(|value| (key, value.as_type().unwrap().clone()))
                    })
                    .try_collect()?,
            )),
            _ => EvalResult::Type(ast.clone()),
        })
    }
    eval(ast, environment)
}

fn print(input: &Type) -> String {
    input.to_string()
}

fn rep(input: &[Token], environment: &mut Environment) -> Result<String, Error> {
    let result = eval(Type::from_tokens(input)?.0, environment)?;
    Ok(print(result.as_type().unwrap()))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Balancing {
    Balanced,
    Unbalanced,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Other(String),
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
                        tokens.push(Self::Other(token));
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
                    tokens.push(Self::Other(token));
                    tokens.extend(Self::tokenise(&next.to_string())?);
                    continue 'outer;
                }
                token.push(next);
            }
        }
        Ok(tokens)
    }

    const fn as_other(&self) -> Option<&String> {
        if let Self::Other(v) = self {
            Some(v)
        } else {
            None
        }
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
                        Self::Other(string) => string,
                        Self::String(_, _) => unreachable!(),
                    }
                    .to_string()
                }
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum HashMapKey {
    Keyword(String),
    String(String),
}

impl Display for HashMapKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Keyword(keyword) => format!(":{keyword}"),
                Self::String(string) => format!("\"{string}\""),
            }
        )
    }
}

impl TryFrom<Type> for HashMapKey {
    type Error = Error;
    fn try_from(value: Type) -> Result<Self, Self::Error> {
        Ok(match value {
            Type::Keyword(keyword) => Self::Keyword(keyword),
            Type::String(string) => Self::String(string),
            _ => return Err(Error::BadHashMapKey(value)),
        })
    }
}

#[derive(Clone)]
enum Type {
    List(Vec<Type>),
    Vector(Vec<Type>),
    HashMap(IndexMap<HashMapKey, Type>),
    Symbol(Token),
    String(String),
    Number(f64),
    Nil,
    True,
    False,
    Keyword(String),
    Function(Function),
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Function(_) => writeln!(f, "#<function>"),
            other => Debug::fmt(other, f),
        }
    }
}

#[derive(Debug, Error)]
enum Error {
    #[error("unexpected end-of-file while parsing")]
    UnexpectedEof,
    #[error("string unbalanced (missing closing `\"`)")]
    UnbalancedString,
    #[error(r#"bad escape (found {0}, expected one of '\n', '\\', '\"')"#)]
    BadEscape(String),
    #[error("missing value in hash-map (found key {0}, expected a value after it)")]
    MissingHashMapValue(HashMapKey),
    #[error("bad key in hash-map (found {0}, expected a keyword or string)")]
    BadHashMapKey(Type),
    #[error("symbol at start of list not found (found {0})")]
    NotFound(String),
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
                Self::List(items) => format!(
                    "({})",
                    items
                        .iter()
                        .map(ToString::to_string)
                        .collect_vec()
                        .join(" ")
                ),
                Self::Vector(items) => format!(
                    "[{}]",
                    items
                        .iter()
                        .map(ToString::to_string)
                        .collect_vec()
                        .join(" ")
                ),
                Self::HashMap(items) => format!(
                    "{{{}}}",
                    items
                        .iter()
                        .flat_map(|(key, value)| [key.to_string(), value.to_string()])
                        .collect_vec()
                        .join(" ")
                ),
                Self::Keyword(string) => format!(":{string}"),
                Self::Function(_) => "#<function>".to_string(),
            }
        })
    }
}

impl Type {
    fn from_tokens(input: &[Token]) -> Result<(Self, usize), Error> {
        let first = input.first().ok_or(Error::UnexpectedEof)?;
        Ok(match first {
            Token::LeftRound => {
                let original_len = input.len();
                let mut input = &input[1..];
                let mut items = Vec::new();
                loop {
                    if *input.first().ok_or(Error::UnexpectedEof)? == Token::RightRound {
                        break;
                    }
                    let (item, len) = Self::from_tokens(input)?;
                    input = &input[len..];
                    items.push(item);
                }
                (Self::List(items), original_len - input.len() + 1)
            }
            Token::LeftSquare => {
                let original_len = input.len();
                let mut input = &input[1..];
                let mut items = Vec::new();
                loop {
                    if *input.first().ok_or(Error::UnexpectedEof)? == Token::RightSquare {
                        break;
                    }
                    let (item, len) = Self::from_tokens(input)?;
                    input = &input[len..];
                    items.push(item);
                }
                (Self::Vector(items), original_len - input.len() + 1)
            }
            Token::LeftBrace => {
                let original_len = input.len();
                let mut input = &input[1..];
                let mut items = IndexMap::new();
                let mut key = None;
                loop {
                    if *input.first().ok_or(Error::UnexpectedEof)? == Token::RightBrace {
                        break;
                    }
                    let (item, len) = Self::from_tokens(input)?;
                    input = &input[len..];
                    match key {
                        None => {
                            key = Some(HashMapKey::try_from(item)?);
                        }
                        Some(k) => {
                            key = None;
                            items.insert(k, item);
                        }
                    }
                }
                if let Some(key) = key {
                    return Err(Error::MissingHashMapValue(key));
                }
                (Self::HashMap(items), original_len - input.len() + 1)
            }
            Token::SingleQuote | Token::Backtick | Token::Tilde | Token::TildeAt => {
                let (parameter, len) = Self::from_tokens(&input[1..])?;
                (
                    Self::List(vec![
                        Self::Symbol(Token::Other(
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
                    ]),
                    len + 1,
                )
            }
            _ => (
                match first {
                    Token::String(ref string, balancing) => match balancing {
                        Balancing::Balanced => Ok(Self::String(string.clone())),
                        Balancing::Unbalanced => Err(Error::UnbalancedString),
                    },
                    Token::Other(string) => Ok(string.parse::<f64>().map_or(
                        match string.as_str() {
                            "nil" => Self::Nil,
                            "true" => Self::True,
                            "false" => Self::False,
                            string if string.starts_with(':') => {
                                Self::Keyword(string[1..].to_string())
                            }
                            _ => Self::Symbol(Token::Other(string.clone())),
                        },
                        Self::Number,
                    )),
                    token => Ok(Self::Symbol(token.clone())),
                }?,
                1,
            ),
        })
    }

    const fn as_number(&self) -> Option<&f64> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }

    const fn as_symbol(&self) -> Option<&Token> {
        if let Self::Symbol(v) = self {
            Some(v)
        } else {
            None
        }
    }

    const fn as_list(&self) -> Option<&Vec<Self>> {
        if let Self::List(v) = self {
            Some(v)
        } else {
            None
        }
    }

    fn as_function(&self) -> Option<&Function> {
        if let Self::Function(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

type Function = Rc<dyn Fn(&[Type]) -> Type>;

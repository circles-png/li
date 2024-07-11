use colored::Colorize;
use core::mem::discriminant;
use human_panic::{setup_panic, Metadata};
use indexmap::IndexMap;
use itertools::{EitherOrBoth, Itertools};
use rustyline::{error::ReadlineError, history::FileHistory, DefaultEditor, Editor};
use std::{
    cell::RefCell,
    collections::HashMap,
    env,
    fmt::{self, Debug, Display, Formatter},
    iter::once,
    ops::Deref,
    rc::Rc,
    vec,
};
use thiserror::Error;

struct Environment {
    outer: Option<Rc<RefCell<Self>>>,
    data: HashMap<Token, Type>,
}

impl Environment {
    fn new() -> Self {
        Self {
            outer: None,
            data: HashMap::new(),
        }
    }

    fn set(&mut self, symbol: Token, value: Type) {
        self.data.insert(symbol, value);
    }

    fn find(&self, symbol: &Token) -> Option<Self> {
        if self.data.contains_key(symbol) {
            return Some(Self {
                outer: self.outer.as_ref().map(Rc::clone),
                data: self.data.clone(),
            });
        } else if let Some(outer) = &self.outer {
            return outer.borrow().find(symbol);
        }
        None
    }

    fn get(&self, symbol: &Token) -> Option<Type> {
        self.find(symbol)?.data.get(symbol).cloned()
    }
}

struct Repl {
    readline: Editor<(), FileHistory>,
    environment: Rc<RefCell<Environment>>,
}

impl Repl {
    #[allow(clippy::too_many_lines)]
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
        for (symbol, function) in [
            ("prn", &|values| {
                println!("{}", values.iter().map(Type::display).join(" "));
                Type::Nil
            }),
            ("list", &|values| Type::List(values.to_vec())),
            ("list?", &|values| {
                Type::Bool(values.first().unwrap().as_list().is_some())
            }),
            ("empty?", &|values| {
                Type::Bool(match values.first().unwrap() {
                    Type::List(list) => list.is_empty(),
                    Type::Vector(vector) => vector.is_empty(),
                    _ => unimplemented!(),
                })
            }),
            ("count", &|values| {
                #[allow(clippy::cast_precision_loss)]
                Type::Number(
                    (match values.first().unwrap() {
                        Type::List(list) => list.len(),
                        Type::Vector(vector) => vector.len(),
                        Type::Nil => 0,
                        _ => unimplemented!(),
                    }) as f64,
                )
            }),
            ("=", &|values| {
                Type::Bool(values.first().unwrap() == values.get(1).unwrap())
            }),
            ("pr-str", &|values| {
                Type::String(values.iter().map(Type::display).join(" "))
            }),
            ("str", &|values| {
                Type::String(values.iter().map(Type::debug).collect())
            }),
            ("println", &|values| {
                println!("{}", values.iter().map(Type::display).join(" "));
                Type::Nil
            }),
        ] as [(&str, &<Function as Deref>::Target); 9]
        {
            environment.set(
                Token::Other(symbol.to_string()),
                Type::Function(Rc::new(function)),
            );
        }
        for (symbol, op) in [
            ("<", &|a, b| a < b),
            ("<=", &|a, b| a <= b),
            (">", &|a, b| a > b),
            (">=", &|a, b| a >= b),
        ] as [(&str, &dyn Fn(f64, f64) -> bool); 4]
        {
            environment.set(
                Token::Other(symbol.to_string()),
                Type::Function(Rc::new(|values| {
                    Type::Bool(op(
                        *values.first().unwrap().as_number().unwrap(),
                        *values.get(1).unwrap().as_number().unwrap(),
                    ))
                })),
            );
        }
        let environment = Rc::new(RefCell::new(environment));
        Self {
            readline,
            environment,
        }
    }

    fn run(mut self) {
        const RC: &[&str] = &["(def! not (fn* (a) (if a false true)))"];
        let run = |line: String| match Token::tokenise(&line) {
            Ok(tokens) => match rep(&tokens, Rc::clone(&self.environment)) {
                Ok(output) => Ok(output),
                Err(error) => Err(format!("{error}").red()),
            },
            Err(error) => Err(format!("{error}").red()),
        };
        for line in RC {
            run((*line).to_string()).unwrap();
        }
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
            match run(line) {
                Ok(output) => {
                    println!("{output}");
                }
                Err(error) => {
                    eprintln!("{error}");
                    continue;
                }
            };
        }
    }
}

fn main() {
    Repl::new().run();
}

#[derive(Clone, Debug)]
enum EvalResult {
    ResultList(Vec<EvalResult>),
    Type(Type),
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
fn eval(ast: Type, environment: Rc<RefCell<Environment>>) -> Result<EvalResult, Error> {
    fn eval(ast: Type, environment: Rc<RefCell<Environment>>) -> Result<EvalResult, Error> {
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
                        let result =
                            eval(unevaluated.get(2).unwrap().clone(), Rc::clone(&environment))?;
                        environment.borrow_mut().set(
                            unevaluated.get(1).unwrap().as_symbol().unwrap().clone(),
                            result.as_type().unwrap().clone(),
                        );
                        result
                    }
                    Some("let*") => {
                        let mut new = Environment::new();
                        new.outer = Some(Rc::clone(&environment));
                        let new = Rc::new(RefCell::new(new));
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
                                    let value = eval(value.clone(), Rc::clone(&new))?
                                        .as_type()
                                        .unwrap()
                                        .clone();
                                    new.borrow_mut()
                                        .set(name.as_symbol().unwrap().clone(), value);
                                }
                                _ => todo!("error"),
                            }
                        }

                        eval(unevaluated.get(2).unwrap().clone(), new)?
                    }
                    Some("do") => eval_ast(Type::List(unevaluated[1..].to_vec()), &environment)?
                        .as_result_list()
                        .unwrap()
                        .last()
                        .unwrap()
                        .clone(),
                    Some("if") => {
                        if match eval(unevaluated.get(1).unwrap().clone(), Rc::clone(&environment))?
                        {
                            EvalResult::Type(Type::Nil | Type::Bool(false)) => false,
                            EvalResult::Type(_) => true,
                            EvalResult::ResultList(_) => unimplemented!(),
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
                        let unevaluated = unevaluated.clone();
                        EvalResult::Type(Type::Function(Rc::new(move |values| {
                            enum BindRest<'a> {
                                None,
                                BindEmpty,
                                BindFirstValue(&'a Type),
                            }
                            let mut new = Environment::new();
                            new.outer = Some(Rc::clone(&environment));
                            let new = Rc::new(RefCell::new(new));
                            let mut bindings = match unevaluated.get(1).unwrap() {
                                Type::List(list) => list,
                                Type::Vector(vector) => vector,
                                _ => unimplemented!(),
                            }
                            .iter()
                            .map(|value| value.as_symbol().unwrap().clone());
                            let mut binding_rest = BindRest::None;
                            let mut values = values.iter();
                            for pair in bindings.by_ref().zip_longest(values.by_ref()) {
                                match pair {
                                    EitherOrBoth::Both(binding, value) => {
                                        if binding == Token::Other("&".to_string()) {
                                            binding_rest = BindRest::BindFirstValue(value);
                                            break;
                                        }
                                        new.borrow_mut().set(binding, value.clone());
                                    }
                                    EitherOrBoth::Left(binding) => {
                                        if binding == Token::Other("&".to_string()) {
                                            binding_rest = BindRest::BindEmpty;
                                            break;
                                        }
                                        unimplemented!("error")
                                    }
                                    EitherOrBoth::Right(_) => {
                                        unimplemented!("error")
                                    }
                                }
                            }
                            match binding_rest {
                                BindRest::None => {}
                                BindRest::BindEmpty => {
                                    let next = bindings.next().unwrap();
                                    new.borrow_mut().set(next, Type::List(Vec::new()));
                                }
                                BindRest::BindFirstValue(first_value) => {
                                    let next = bindings.next().unwrap();
                                    let rest =
                                        values.chain(once(first_value)).cloned().collect_vec();
                                    new.borrow_mut().set(next, Type::List(rest));
                                }
                            }
                            eval(unevaluated.get(2).unwrap().clone(), new)
                                .unwrap()
                                .as_type()
                                .unwrap()
                                .clone()
                        })))
                    }
                    _ => {
                        let result = eval_ast(ast.clone(), &environment)?;
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
            _ => eval_ast(ast, &environment)?,
        })
    }
    fn eval_ast(ast: Type, environment: &Rc<RefCell<Environment>>) -> Result<EvalResult, Error> {
        Ok(match ast {
            Type::Symbol(symbol) => EvalResult::Type(
                environment
                    .borrow()
                    .get(&symbol)
                    .ok_or(Error::NotFound(symbol.to_string()))?,
            ),
            Type::List(list) => EvalResult::ResultList(
                list.iter()
                    .map(|item| eval(item.clone(), Rc::clone(environment)))
                    .try_collect()?,
            ),
            Type::Vector(vector) => EvalResult::Type(Type::Vector(
                vector
                    .into_iter()
                    .map(|item| {
                        eval(item, Rc::clone(environment))
                            .map(|result| result.as_type().unwrap().clone())
                    })
                    .try_collect()?,
            )),
            Type::HashMap(hash_map) => EvalResult::Type(Type::HashMap(
                hash_map
                    .into_iter()
                    .map(|(key, value)| {
                        eval(value, Rc::clone(environment))
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
    input.display()
}

fn rep(input: &[Token], environment: Rc<RefCell<Environment>>) -> Result<String, Error> {
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
    Bool(bool),
    Keyword(String),
    Function(Function),
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::List(arg0) => f.debug_tuple("List").field(arg0).finish(),
            Self::Vector(arg0) => f.debug_tuple("Vector").field(arg0).finish(),
            Self::HashMap(arg0) => f.debug_tuple("HashMap").field(arg0).finish(),
            Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::Nil => write!(f, "Nil"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Keyword(arg0) => f.debug_tuple("Keyword").field(arg0).finish(),
            Self::Function(_) => write!(f, "Function"),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Vector(a) | Self::List(a), Self::Vector(b) | Self::List(b)) => a == b,
            (Self::HashMap(a), Self::HashMap(b)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::String(a), Self::String(b)) | (Self::Keyword(a), Self::Keyword(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Function(_), Self::Function(_)) => false,
            _ => discriminant(self) == discriminant(other),
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
    #[error("bad key in hash-map (found {}, expected a keyword or string)", .0.debug())]
    BadHashMapKey(Type),
    #[error("symbol at start of list not found (found {0})")]
    NotFound(String),
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
                            "true" => Self::Bool(true),
                            "false" => Self::Bool(false),
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

    fn print(&self, display: bool) -> Result<String, String> {
        Ok(match self {
            Self::Number(number) => number.to_string(),
            Self::Symbol(symbol) => symbol.to_string(),
            Self::Nil => "nil".to_string(),
            Self::Bool(bool) => bool.to_string(),
            Self::List(items) => format!(
                "({})",
                items
                    .iter()
                    .map(if display { Self::display } else { Self::debug })
                    .collect_vec()
                    .join(" ")
            ),
            Self::Vector(items) => format!(
                "[{}]",
                items
                    .iter()
                    .map(if display { Self::display } else { Self::debug })
                    .collect_vec()
                    .join(" ")
            ),
            Self::HashMap(items) => format!(
                "{{{}}}",
                items
                    .iter()
                    .flat_map(|(key, value)| [
                        key.to_string(),
                        if display { Self::display } else { Self::debug }(value)
                    ])
                    .collect_vec()
                    .join(" ")
            ),
            Self::Keyword(string) => format!(":{string}"),
            Self::Function(_) => "#<function>".to_string(),
            Self::String(string) => return Err(string.clone()),
        })
    }

    fn debug(&self) -> String {
        match self.print(false) {
            Ok(string) => string,
            Err(string) => {
                let string = format!("{string:?}");
                string[1..string.len() - 1].to_string()
            }
        }
    }

    fn display(&self) -> String {
        match self.print(true) {
            Ok(string) | Err(string) => string,
        }
    }
}

type Function = Rc<dyn Fn(&[Type]) -> Type>;

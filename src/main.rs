use clap::ArgAction;
use clap::Parser;
use clap::ValueHint;
use colored::Colorize;
use core::mem::discriminant;
use enum_as_inner::EnumAsInner;
use human_panic::{setup_panic, Metadata};
use indexmap::IndexMap;
use itertools::{EitherOrBoth, Itertools};
use rustyline::{error::ReadlineError, history::FileHistory, DefaultEditor, Editor};
use std::{
    cell::RefCell,
    collections::HashMap,
    env::args,
    fmt::{self, Debug, Display, Formatter},
    fs::read_to_string,
    iter::{once, Extend},
    ops::Deref,
    path::PathBuf,
    rc::Rc,
    string::ToString,
    vec::Vec,
};
use thiserror::Error;

#[derive(Debug)]
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
            return outer.deref().borrow().find(symbol);
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
    cli: Cli,
}

impl Repl {
    #[allow(clippy::too_many_lines)]
    fn new(cli: Cli) -> Self {
        setup_panic!(Metadata::new(
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION")
        ));
        if !cli.testing {
            eprintln!(
                "{} {} {}{}",
                "starting".dimmed(),
                env!("CARGO_PKG_NAME").green(),
                env!("CARGO_PKG_VERSION").dimmed(),
                "...".dimmed()
            );
        }
        let mut readline = DefaultEditor::new().unwrap();
        let _ = readline.load_history("history");
        let environment = Rc::new(RefCell::new(Environment::new()));
        for (symbol, op) in [
            ("+", &|a, b| a + b),
            ("-", &|a, b| a - b),
            ("*", &|a, b| a * b),
            ("/", &|a, b| a / b),
        ] as [(&str, &dyn Fn(f64, f64) -> f64); 4]
        {
            environment.deref().borrow_mut().set(
                Token::Other(symbol.to_string()),
                Type::Function(FunctionType::Regular(Rc::new(move |values| {
                    Type::Number(
                        values
                            .iter()
                            .map(|value| *value.as_number().unwrap())
                            .reduce(op)
                            .unwrap(),
                    )
                }))),
            );
        }
        for (symbol, function) in [
            ("prn", &|values| {
                println!(
                    "{}",
                    values
                        .iter()
                        .map(|value| value.print(false, true))
                        .join(" ")
                );
                Type::Nil
            }),
            ("list", &|values| {
                Type::List(values.iter().copied().cloned().collect())
            }),
            ("list?", &|values| {
                Type::Bool(values.first().unwrap().is_list())
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
                Type::String(
                    values
                        .iter()
                        .map(|value| value.print(false, true))
                        .join(" "),
                )
            }),
            ("str", &|values| {
                Type::String(
                    values
                        .iter()
                        .map(|value| value.print(true, false))
                        .collect(),
                )
            }),
            ("println", &|values| {
                println!(
                    "{}",
                    values
                        .iter()
                        .map(|value| value.print(true, false))
                        .join(" ")
                );
                Type::Nil
            }),
            ("read-string", &|values| {
                Type::from_tokens(
                    &Token::tokenise(values.first().unwrap().as_string().unwrap()).unwrap(),
                )
                .unwrap()
                .0
            }),
            ("slurp", &|values| {
                Type::String(read_to_string(values.first().unwrap().as_string().unwrap()).unwrap())
            }),
            ("atom", &|values| {
                Type::Atom(Rc::new(RefCell::new((*values.first().unwrap()).clone())))
            }),
            ("atom?", &|values| {
                Type::Bool(values.first().unwrap().is_atom())
            }),
            ("deref", &|values| {
                values
                    .first()
                    .unwrap()
                    .as_atom()
                    .unwrap()
                    .borrow()
                    .deref()
                    .clone()
            }),
            ("reset!", &|values| {
                let [atom, value] = values else {
                    unimplemented!()
                };
                *atom.as_atom().unwrap().borrow_mut() = (*value).clone();
                (*value).clone()
            }),
            ("swap!", &|values| {
                let [atom, function, args @ ..] = values else {
                    unimplemented!()
                };
                let mut arguments: Vec<Type> = Vec::new();
                {
                    let first_argument = atom.as_atom().unwrap().borrow();
                    arguments.push(first_argument.clone());
                }
                arguments.extend(args.iter().copied().cloned());
                let arguments = arguments.iter().collect_vec();
                let result = match function.as_function().unwrap() {
                    FunctionType::Regular(function) => Rc::clone(function)(&arguments),
                    FunctionType::UserDefined(function) => {
                        enum BindRest<'a> {
                            None,
                            BindEmpty,
                            BindFirstValue(&'a Type),
                        }
                        let UserDefined { body, params, env } = &**function;
                        let mut new = Environment::new();
                        new.outer = Some(Rc::clone(env));
                        let new = Rc::new(RefCell::new(new));
                        let mut bindings = match params {
                            Type::List(list) => list,
                            Type::Vector(vector) => vector,
                            _ => unimplemented!(),
                        }
                        .iter()
                        .map(|value| value.as_symbol().unwrap().clone());
                        let mut binding_rest = BindRest::None;
                        let mut values = arguments.iter().copied();
                        for pair in bindings.by_ref().zip_longest(values.by_ref()) {
                            match pair {
                                EitherOrBoth::Both(binding, value) => {
                                    if binding == Token::Other("&".to_string()) {
                                        binding_rest = BindRest::BindFirstValue(value);
                                        break;
                                    }
                                    new.deref().borrow_mut().set(binding, value.clone().clone());
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
                                new.deref().borrow_mut().set(next, Type::List(Vec::new()));
                            }
                            BindRest::BindFirstValue(first_value) => {
                                let next = bindings.next().unwrap();
                                let rest = values.chain(once(first_value)).cloned().collect_vec();
                                new.deref().borrow_mut().set(next, Type::List(rest));
                            }
                        }
                        eval(body.clone(), new).unwrap().as_type().unwrap().clone()
                    }
                };
                *atom.as_atom().unwrap().clone().borrow_mut() = result.clone();
                result
            }),
        ] as [(&str, &<Function as Deref>::Target); 16]
        {
            environment.deref().borrow_mut().set(
                Token::Other(symbol.to_string()),
                Type::Function(FunctionType::Regular(Rc::new(function))),
            );
        }
        for (symbol, op) in [
            ("<", &|a, b| a < b),
            ("<=", &|a, b| a <= b),
            (">", &|a, b| a > b),
            (">=", &|a, b| a >= b),
        ] as [(&str, &dyn Fn(f64, f64) -> bool); 4]
        {
            environment.deref().borrow_mut().set(
                Token::Other(symbol.to_string()),
                Type::Function(FunctionType::Regular(Rc::new(|values| {
                    Type::Bool(op(
                        *values.first().unwrap().as_number().unwrap(),
                        *values.get(1).unwrap().as_number().unwrap(),
                    ))
                }))),
            );
        }
        environment.deref().borrow_mut().set(
            Token::Other("eval".to_string()),
            Type::Function(FunctionType::Regular(Rc::new({
                let environment = Rc::clone(&environment);
                move |ast| {
                    eval((*ast.first().unwrap()).clone(), Rc::clone(&environment))
                        .unwrap()
                        .as_type()
                        .unwrap()
                        .clone()
                }
            }))),
        );
        Self {
            readline,
            environment,
            cli,
        }
    }

    fn run(mut self) {
        const RC: &[&str] = &[
            "(def! not (fn* (a) (if a false true)))",
            r#"(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))"#,
        ];
        let run = |line: String| match Token::tokenise(&line) {
            Ok(tokens) => match rep(&tokens, &self.environment) {
                Ok(output) => Ok(output),
                Err(error) => Err(format!("{error}").red()),
            },
            Err(error) => Err(format!("{error}").red()),
        };
        for line in RC {
            run((*line).to_string()).unwrap();
        }
        self.environment
            .borrow_mut()
            .data
            .insert(Token::Other("*ARGV*".to_string()), Type::List(Vec::new()));
        if let Some(file) = self.cli.file {
            self.environment.borrow_mut().data.insert(
                Token::Other("*ARGV*".to_string()),
                Type::List(
                    self.cli
                        .rest
                        .iter()
                        .map(|value| Type::String(value.clone()))
                        .collect(),
                ),
            );
            match run(format!("(load-file {})", file.display())) {
                Ok(output) => {
                    println!("{output}");
                }
                Err(error) => {
                    eprintln!("{error}");
                }
            }
            return;
        }
        loop {
            let line = {
                let green_prompt = concat!(env!("CARGO_PKG_NAME"), " > ").green().to_string();
                match self.readline.readline(if self.cli.testing {
                    concat!(env!("CARGO_PKG_NAME"), "> ")
                } else {
                    &green_prompt
                }) {
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
            }
        }
        self.readline.save_history("history").unwrap();
    }
}

#[derive(Parser)]
struct Cli {
    #[arg(short, long, action = ArgAction::SetTrue)]
    testing: bool,
    #[arg(value_hint = ValueHint::FilePath)]
    file: Option<PathBuf>,
    #[arg(trailing_var_arg = true)]
    rest: Vec<String>,
}

fn main() {
    Repl::new(Cli::parse()).run();
}

#[derive(Clone, Debug, EnumAsInner)]
enum EvalResult {
    ResultList(Vec<EvalResult>),
    Type(Type),
}

#[allow(clippy::too_many_lines)]
fn eval(ast: Type, environment: Rc<RefCell<Environment>>) -> Result<EvalResult, Error> {
    fn eval(mut ast: Type, mut environment: Rc<RefCell<Environment>>) -> Result<EvalResult, Error> {
        loop {
            return Ok(match ast {
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
                            environment.deref().borrow_mut().set(
                                unevaluated.get(1).unwrap().as_symbol().unwrap().clone(),
                                match result {
                                    EvalResult::Type(ref v) => v.clone(),
                                    EvalResult::ResultList(_) => unimplemented!(),
                                },
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
                                _ => unimplemented!("error"),
                            }
                            .chunks_exact(2);
                            if !pairs.remainder().is_empty() {
                                unimplemented!("this should be an error!!")
                            }
                            for pair in pairs {
                                match pair {
                                    [name, value] => {
                                        let value = match eval(value.clone(), Rc::clone(&new))? {
                                            EvalResult::ResultList(_) => unimplemented!(),
                                            EvalResult::Type(value) => value,
                                        }
                                        .clone();
                                        new.deref()
                                            .borrow_mut()
                                            .set(name.as_symbol().unwrap().clone(), value);
                                    }
                                    _ => unimplemented!("error"),
                                }
                            }
                            environment = new;
                            ast = unevaluated.get(2).unwrap().clone();
                            continue;
                        }
                        Some("do") => {
                            eval_ast(
                                Type::List(unevaluated[1..unevaluated.len() - 1].to_vec()),
                                &environment,
                            )?
                            .as_result_list()
                            .unwrap();
                            ast = ast.as_list().unwrap().last().unwrap().clone();
                            continue;
                        }
                        Some("if") => {
                            if match eval(
                                unevaluated.get(1).unwrap().clone(),
                                Rc::clone(&environment),
                            )? {
                                EvalResult::Type(Type::Nil | Type::Bool(false)) => false,
                                EvalResult::Type(_) => true,
                                EvalResult::ResultList(_) => unimplemented!(),
                            } {
                                ast = unevaluated.get(2).unwrap().clone();
                                continue;
                            }
                            let get = unevaluated.get(3);
                            match get {
                                Some(get) => {
                                    ast = get.clone();
                                    continue;
                                }
                                None => EvalResult::Type(Type::Nil),
                            }
                        }
                        Some("fn*") => EvalResult::Type(Type::Function(FunctionType::UserDefined(
                            Box::new(UserDefined {
                                body: ast.as_list().unwrap().get(2).unwrap().clone(),
                                params: ast.as_list().unwrap().get(1).unwrap().clone(),
                                env: Rc::clone(&environment),
                            }),
                        ))),
                        _ => {
                            let result = eval_ast(ast.clone(), &environment)?;
                            let evaluated = result.as_result_list().unwrap();
                            let [function, args @ ..] = evaluated.as_slice() else {
                                unimplemented!()
                            };
                            match function {
                                EvalResult::Type(Type::Function(FunctionType::Regular(
                                    function,
                                ))) => EvalResult::Type(Rc::clone(function)(
                                    &evaluated
                                        .iter()
                                        .skip(1)
                                        .map(|item| item.as_type().unwrap())
                                        .collect_vec(),
                                )),
                                EvalResult::Type(Type::Function(FunctionType::UserDefined(
                                    function,
                                ))) => {
                                    enum BindRest<'a> {
                                        None,
                                        BindEmpty,
                                        BindFirstValue(&'a Type),
                                    }
                                    let UserDefined { body, params, env } = &**function;
                                    ast = body.clone();
                                    let mut new = Environment::new();
                                    new.outer = Some(Rc::clone(env));
                                    let new = Rc::new(RefCell::new(new));
                                    let mut bindings = match params {
                                        Type::List(list) => list,
                                        Type::Vector(vector) => vector,
                                        _ => unimplemented!(),
                                    }
                                    .iter()
                                    .map(|value| value.as_symbol().unwrap().clone());
                                    let mut binding_rest = BindRest::None;
                                    let mut values =
                                        args.iter().map(EvalResult::as_type).map(Option::unwrap);
                                    for pair in bindings.by_ref().zip_longest(values.by_ref()) {
                                        match pair {
                                            EitherOrBoth::Both(binding, value) => {
                                                if binding == Token::Other("&".to_string()) {
                                                    binding_rest = BindRest::BindFirstValue(value);
                                                    break;
                                                }
                                                new.deref()
                                                    .borrow_mut()
                                                    .set(binding, value.clone());
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
                                            new.deref()
                                                .borrow_mut()
                                                .set(next, Type::List(Vec::new()));
                                        }
                                        BindRest::BindFirstValue(first_value) => {
                                            let next = bindings.next().unwrap();
                                            let rest = values
                                                .chain(once(first_value))
                                                .cloned()
                                                .collect_vec();
                                            new.deref().borrow_mut().set(next, Type::List(rest));
                                        }
                                    }
                                    environment = new;
                                    continue;
                                }
                                EvalResult::Type(_) => unimplemented!(),
                                EvalResult::ResultList(_) => unimplemented!(),
                            }
                        }
                    }
                }
                _ => eval_ast(ast, &environment)?,
            });
        }
    }
    fn eval_ast(ast: Type, environment: &Rc<RefCell<Environment>>) -> Result<EvalResult, Error> {
        Ok(match ast {
            Type::Symbol(symbol) => EvalResult::Type(
                environment
                    .deref()
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
    input.print(false, true)
}

fn rep(input: &[Token], environment: &Rc<RefCell<Environment>>) -> Result<String, Error> {
    let result = eval(Type::from_tokens(input)?.0, Rc::clone(environment))?;
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
                    if next == ';' {
                        input = input.split_once('\n').map_or("", |(_, rest)| rest);
                        continue 'outer;
                    }
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

#[derive(Clone, EnumAsInner)]
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
    Function(FunctionType),
    Atom(Rc<RefCell<Type>>),
}

#[derive(Clone)]
enum FunctionType {
    Regular(Function),
    UserDefined(Box<UserDefined>),
}

#[derive(Clone)]
struct UserDefined {
    body: Type,
    params: Type,
    env: Rc<RefCell<Environment>>,
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
            Self::Atom(atom) => f.debug_tuple("Atom").field(&&**atom).finish(),
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
    #[error("bad key in hash-map (found {}, expected a keyword or string)", .0.print(true, false))]
    BadHashMapKey(Type),
    #[error("symbol at start of list not found (found {0})")]
    NotFound(String),
}

impl Type {
    #[allow(clippy::too_many_lines)]
    fn from_tokens(input: &[Token]) -> Result<(Self, usize), Error> {
        let Some(first) = input.first() else {
            return Ok((Self::Nil, 0));
        };
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
            Token::SingleQuote | Token::Backtick | Token::Tilde | Token::TildeAt | Token::At => {
                let (parameter, len) = Self::from_tokens(&input[1..])?;
                (
                    Self::List(vec![
                        Self::Symbol(Token::Other(
                            match first {
                                Token::SingleQuote => "quote",
                                Token::Backtick => "quasiquote",
                                Token::Tilde => "unquote",
                                Token::TildeAt => "splice-unquote",
                                Token::At => "deref",
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

    fn print(&self, display: bool, quote_strings: bool) -> String {
        match self {
            Self::Number(number) => number.to_string(),
            Self::Symbol(symbol) => symbol.to_string(),
            Self::Nil => "nil".to_string(),
            Self::Bool(bool) => bool.to_string(),
            Self::List(items) => format!(
                "({})",
                items
                    .iter()
                    .map(|value| dbg!(value.print(display, quote_strings)))
                    .collect_vec()
                    .join(" ")
            ),
            Self::Vector(items) => format!(
                "[{}]",
                items
                    .iter()
                    .map(|value| value.print(display, quote_strings))
                    .collect_vec()
                    .join(" ")
            ),
            Self::HashMap(items) => format!(
                "{{{}}}",
                items
                    .iter()
                    .flat_map(|(key, value)| [key.to_string(), value.print(display, quote_strings)])
                    .collect_vec()
                    .join(" ")
            ),
            Self::Keyword(string) => format!(":{string}"),
            Self::Function(_) => "#<function>".to_string(),
            Self::String(string) => {
                let string = if display {
                    string.clone()
                } else {
                    string
                        .replace('\\', "\\\\")
                        .replace('\n', "\\n")
                        .replace('\"', "\\\"")
                };
                if quote_strings {
                    format!("\"{string}\"")
                } else {
                    string
                }
            }
            Self::Atom(atom) => format!("(atom {})", atom.borrow().print(display, quote_strings)),
        }
    }
}

type Function = Rc<dyn Fn(&[&Type]) -> Type>;

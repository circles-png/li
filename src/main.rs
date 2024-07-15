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
use std::convert::Into;
use std::io::stdin;
use std::io::stdout;
use std::io::Write;
use std::time::SystemTime;
use std::{
    cell::RefCell,
    collections::HashMap,
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
                Type::Function(
                    FunctionType::Regular(Rc::new(move |values| {
                        Ok(Type::Number(
                            values
                                .iter()
                                .map(|value| *value.as_number().unwrap())
                                .reduce(op)
                                .unwrap(),
                        ))
                    })),
                    Box::new(Type::Nil),
                ),
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
                Ok(Type::Nil)
            }),
            ("list", &|values| {
                Ok(Type::List(
                    values.iter().copied().cloned().collect(),
                    Box::new(Type::Nil),
                ))
            }),
            ("list?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_list()))
            }),
            ("empty?", &|values| {
                Ok(Type::Bool(match values.first().unwrap() {
                    Type::List(list, _) => list.is_empty(),
                    Type::Vector(vector, _) => vector.is_empty(),
                    _ => unimplemented!(),
                }))
            }),
            ("count", &|values| {
                Ok(
                    #[allow(clippy::cast_precision_loss)]
                    Type::Number(
                        (match values.first().unwrap() {
                            Type::List(list, _) => list.len(),
                            Type::Vector(vector, _) => vector.len(),
                            Type::Nil => 0,
                            _ => unimplemented!(),
                        }) as f64,
                    ),
                )
            }),
            ("=", &|values| {
                Ok(Type::Bool(
                    values.first().unwrap() == values.get(1).unwrap(),
                ))
            }),
            ("pr-str", &|values| {
                Ok(Type::String(
                    values
                        .iter()
                        .map(|value| value.print(false, true))
                        .join(" "),
                ))
            }),
            ("str", &|values| {
                Ok(Type::String(
                    values
                        .iter()
                        .map(|value| value.print(true, false))
                        .collect(),
                ))
            }),
            ("println", &|values| {
                println!(
                    "{}",
                    values
                        .iter()
                        .map(|value| value.print(true, false))
                        .join(" ")
                );
                Ok(Type::Nil)
            }),
            ("read-string", &|values| {
                Ok(Type::from_tokens(
                    &Token::tokenise(values.first().unwrap().as_string().unwrap()).unwrap(),
                )
                .unwrap()
                .0)
            }),
            ("slurp", &|values| {
                Ok(Type::String(
                    read_to_string(values.first().unwrap().as_string().unwrap()).unwrap(),
                ))
            }),
            ("atom", &|values| {
                Ok(Type::Atom(Rc::new(RefCell::new(
                    (*values.first().unwrap()).clone(),
                ))))
            }),
            ("atom?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_atom()))
            }),
            ("deref", &|values| {
                Ok(values
                    .first()
                    .unwrap()
                    .as_atom()
                    .unwrap()
                    .borrow()
                    .deref()
                    .clone())
            }),
            ("reset!", &|values| {
                let [atom, value] = values else {
                    unimplemented!()
                };
                *atom.as_atom().unwrap().borrow_mut() = (*value).clone();
                Ok((*value).clone())
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
                let result = match function.as_function().unwrap().0 {
                    FunctionType::Regular(function) => Rc::clone(function)(&arguments),
                    FunctionType::UserDefined(function) => (function.function)(&arguments),
                };
                *atom.as_atom().unwrap().clone().borrow_mut() = match result {
                    Ok(ref result) => result.clone(),
                    Err(error) => return Err(error),
                };
                result
            }),
            ("cons", &|values| {
                Ok(Type::List(
                    once((*values.first().unwrap()).clone())
                        .chain(
                            match values.get(1).unwrap() {
                                Type::List(list, _) => list,
                                Type::Vector(vector, _) => vector,
                                _ => unimplemented!(),
                            }
                            .clone(),
                        )
                        .collect(),
                    Box::new(Type::Nil),
                ))
            }),
            ("concat", &|values| {
                Ok(Type::List(
                    values
                        .iter()
                        .flat_map(|value| {
                            match value {
                                Type::List(list, _) => list,
                                Type::Vector(vector, _) => vector,
                                _ => unimplemented!(),
                            }
                            .clone()
                        })
                        .collect(),
                    Box::new(Type::Nil),
                ))
            }),
            ("vec", &|values| match values.first().unwrap() {
                Type::List(list, _) => Ok(Type::Vector(list.clone(), Box::new(Type::Nil))),
                vector @ Type::Vector(_, _) => Ok((*vector).clone()),
                _ => unimplemented!(),
            }),
            ("nth", &|values| {
                let items = match values.first().unwrap() {
                    Type::List(list, _) => list,
                    Type::Vector(vector, _) => vector,
                    _ => unimplemented!(),
                };
                let number = values.get(1).unwrap().as_number().unwrap();
                assert!(number.fract() == 0.);
                #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                let number = *number as usize;
                Ok(items
                    .get(number)
                    .ok_or(Error::OutOfRange(number, "nth".to_string()))?
                    .clone())
            }),
            ("first", &|values| {
                let items = match values.first().unwrap() {
                    Type::List(list, _) => list,
                    Type::Vector(vector, _) => vector,
                    Type::Nil => return Ok(Type::Nil),
                    _ => unimplemented!(),
                };
                Ok(items.first().unwrap_or(&Type::Nil).clone())
            }),
            ("rest", &|values| {
                let items = match values.first().unwrap() {
                    Type::List(list, _) if list.is_empty() => {
                        return Ok(Type::List(Vec::new(), Box::new(Type::Nil)))
                    }
                    Type::Nil => return Ok(Type::List(Vec::new(), Box::new(Type::Nil))),
                    Type::List(list, _) => list,
                    Type::Vector(vector, _) => vector,
                    _ => unimplemented!(),
                };
                Ok(Type::List(
                    items.get(1..).map_or(Vec::new(), <[Type]>::to_vec),
                    Box::new(Type::Nil),
                ))
            }),
            ("throw", &|values| {
                Err(Error::Custom((*values.first().unwrap()).clone()))
            }),
            ("apply", &|values| {
                let function = values.first().unwrap().as_function().unwrap().0;
                let values = values[1..values.len() - 1]
                    .iter()
                    .copied()
                    .chain(match values.last().unwrap() {
                        Type::List(list, _) => list,
                        Type::Vector(vector, _) => vector,
                        _ => unimplemented!(),
                    })
                    .collect_vec();
                match function {
                    FunctionType::Regular(function) => (**function)(&values),
                    FunctionType::UserDefined(function) => (*function.deref().function)(&values),
                }
            }),
            ("map", &|values| {
                let function = values.first().unwrap().as_function().unwrap().0;
                let items = match values.get(1).unwrap() {
                    Type::List(list, _) => list,
                    Type::Vector(vector, _) => vector,
                    _ => unimplemented!(),
                };
                Ok(Type::List(
                    items
                        .iter()
                        .map(|item| match function {
                            FunctionType::Regular(function) => (**function)(&[item]),
                            FunctionType::UserDefined(function) => {
                                (*function.deref().function)(&[item])
                            }
                        })
                        .try_collect()?,
                    Box::new(Type::Nil),
                ))
            }),
            ("nil?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_nil()))
            }),
            ("true?", &|values| {
                Ok(Type::Bool(
                    values.first().unwrap().as_bool().is_some_and(|bool| *bool),
                ))
            }),
            ("false?", &|values| {
                Ok(Type::Bool(
                    values.first().unwrap().as_bool().is_some_and(|bool| !*bool),
                ))
            }),
            ("symbol?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_symbol()))
            }),
            ("symbol", &|values| {
                Ok(Type::Symbol(Token::Other(
                    values.first().unwrap().as_string().unwrap().clone(),
                )))
            }),
            ("keyword", &|values| {
                let first = values.first().unwrap();
                if first.is_keyword() {
                    return Ok((*first).clone());
                }
                Ok(Type::Keyword(first.as_string().unwrap().clone()))
            }),
            ("keyword?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_keyword()))
            }),
            ("vector", &|values| {
                Ok(Type::Vector(
                    values.iter().copied().cloned().collect(),
                    Box::new(Type::Nil),
                ))
            }),
            ("vector?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_vector()))
            }),
            ("sequential?", &|values| {
                let value = values.first().unwrap();
                Ok(Type::Bool(value.is_vector() || value.is_list()))
            }),
            ("hash-map", &|values| {
                let pairs = values.chunks_exact(2);
                if let Some(key) = pairs.remainder().first() {
                    return Err(Error::MissingHashMapValue((*key).clone().try_into()?));
                }
                Ok(Type::HashMap(
                    pairs
                        .map(|pair| pair[0].clone().try_into().map(|key| (key, pair[1].clone())))
                        .try_collect::<_, IndexMap<_, _>, _>()?,
                    Box::new(Type::Nil),
                ))
            }),
            ("map?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_hash_map()))
            }),
            ("assoc", &|values| {
                let hash_map = values.first().unwrap().as_hash_map().unwrap().0;
                let pairs = values[1..].chunks_exact(2);
                if let Some(key) = pairs.remainder().first() {
                    return Err(Error::MissingHashMapValue((*key).clone().try_into()?));
                }
                Ok(Type::HashMap(
                    hash_map
                        .iter()
                        .map(|(key, value)| Ok((key.clone(), value.clone())))
                        .chain(pairs.map(|pair| {
                            pair[0].clone().try_into().map(|key| (key, pair[1].clone()))
                        }))
                        .try_collect::<_, IndexMap<_, _>, _>()?,
                    Box::new(Type::Nil),
                ))
            }),
            ("dissoc", &|values| {
                let mut hash_map = values.first().unwrap().as_hash_map().unwrap().0.clone();
                let keys = &values[1..];
                hash_map.retain(|key, _| !keys.contains(&&Type::from(key.clone())));
                Ok(Type::HashMap(hash_map.clone(), Box::new(Type::Nil)))
            }),
            ("get", &|values| {
                let Some(hash_map) = values
                    .first()
                    .unwrap()
                    .as_hash_map()
                    .map(|(hash_map, _)| hash_map)
                    .cloned()
                else {
                    return Ok(Type::Nil);
                };
                let key = HashMapKey::try_from((*values.get(1).unwrap()).clone())?;
                Ok(hash_map.get(&key).unwrap_or(&Type::Nil).clone())
            }),
            ("contains?", &|values| {
                let hash_map = values.first().unwrap().as_hash_map().unwrap().0;
                let key = HashMapKey::try_from((*values.get(1).unwrap()).clone())?;
                Ok(Type::Bool(hash_map.contains_key(&key)))
            }),
            ("keys", &|values| {
                let hash_map = values.first().unwrap().as_hash_map().unwrap().0.clone();
                Ok(Type::List(
                    hash_map.into_keys().map(Into::into).collect(),
                    Box::new(Type::Nil),
                ))
            }),
            ("vals", &|values| {
                let hash_map = values.first().unwrap().as_hash_map().unwrap().0.clone();
                Ok(Type::List(
                    hash_map.into_values().collect(),
                    Box::new(Type::Nil),
                ))
            }),
            ("readline", &|values| {
                let mut buffer = String::new();
                print!("{}", values.first().unwrap().as_string().unwrap());
                stdout().flush().unwrap();
                if stdin().read_line(&mut buffer).unwrap() == 0 {
                    return Ok(Type::Nil);
                }
                Ok(Type::String(buffer[..buffer.len() - 1].to_string()))
            }),
            ("time-ms", &|_| {
                const NANOS_PER_MILLI: f64 = 1_000_000.;
                const MILLIS_PER_SEC: f64 = 1_000.;
                let duration = &SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap();
                Ok(Type::Number(
                    #[allow(clippy::cast_precision_loss)]
                    (duration.as_secs() as f64).mul_add(
                        MILLIS_PER_SEC,
                        f64::from(duration.subsec_nanos()) / NANOS_PER_MILLI,
                    ),
                ))
            }),
            ("meta", &|values| match values.first().unwrap() {
                Type::Function(_, metadata)
                | Type::List(_, metadata)
                | Type::Vector(_, metadata)
                | Type::HashMap(_, metadata) => Ok(metadata.deref().clone()),
                _ => unimplemented!(),
            }),
            ("with-meta", &|values| {
                let mut value = (*values.first().unwrap()).clone();
                let set = (*values.get(1).unwrap()).clone();
                match &mut value {
                    Type::Function(_, metadata)
                    | Type::List(_, metadata)
                    | Type::Vector(_, metadata)
                    | Type::HashMap(_, metadata) => **metadata = set,
                    _ => unimplemented!(),
                }
                Ok(value)
            }),
            ("fn?", &|values| {
                Ok(Type::Bool(
                    values.first().unwrap().as_function().is_some_and(
                        |(function, _)| match function {
                            FunctionType::Regular(_) => true,
                            FunctionType::UserDefined(function) => !function.is_macro,
                        },
                    ),
                ))
            }),
            ("string?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_string()))
            }),
            ("number?", &|values| {
                Ok(Type::Bool(values.first().unwrap().is_number()))
            }),
            ("macro?", &|values| {
                Ok(Type::Bool(
                    values
                        .first()
                        .unwrap()
                        .as_function()
                        .is_some_and(|(function, _)| {
                            function
                                .as_user_defined()
                                .is_some_and(|function| function.is_macro)
                        }),
                ))
            }),
            ("seq", &|values| {
                Ok(match values.first().unwrap() {
                    value @ Type::List(list, _) => {
                        if list.is_empty() {
                            Type::Nil
                        } else {
                            (*value).clone()
                        }
                    }
                    Type::Vector(vector, _) => {
                        if vector.is_empty() {
                            Type::Nil
                        } else {
                            Type::List(vector.clone(), Box::new(Type::Nil))
                        }
                    }
                    Type::String(string) => {
                        if string.is_empty() {
                            Type::Nil
                        } else {
                            Type::List(
                                string
                                    .chars()
                                    .map(|char| Type::String(char.to_string()))
                                    .collect(),
                                Box::new(Type::Nil),
                            )
                        }
                    }
                    Type::Nil => Type::Nil,
                    _ => unimplemented!(),
                })
            }),
            ("conj", &|values| {
                let collection = values.first().unwrap();
                let new = values[1..].iter().copied().cloned();
                Ok(match collection {
                    Type::List(list, _) => {
                        let list = new.rev().chain(list.iter().cloned()).collect();
                        Type::List(list, Box::new(Type::Nil))
                    }
                    Type::Vector(vector, _) => {
                        let mut vector = vector.clone();
                        vector.extend(new);
                        Type::Vector(vector.clone(), Box::new(Type::Nil))
                    }
                    _ => unimplemented!(),
                })
            }),
        ] as [(&str, &<Function as Deref>::Target); 53]
        {
            environment.deref().borrow_mut().set(
                Token::Other(symbol.to_string()),
                Type::Function(
                    FunctionType::Regular(Rc::new(function)),
                    Box::new(Type::Nil),
                ),
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
                Type::Function(
                    FunctionType::Regular(Rc::new(|values| {
                        Ok(Type::Bool(op(
                            *values.first().unwrap().as_number().unwrap(),
                            *values.get(1).unwrap().as_number().unwrap(),
                        )))
                    })),
                    Box::new(Type::Nil),
                ),
            );
        }
        environment.deref().borrow_mut().set(
            Token::Other("eval".to_string()),
            Type::Function(
                FunctionType::Regular(Rc::new({
                    let environment = Rc::clone(&environment);
                    move |ast| {
                        Ok(
                            eval((*ast.first().unwrap()).clone(), Rc::clone(&environment))
                                .unwrap()
                                .as_type()
                                .unwrap()
                                .clone(),
                        )
                    }
                })),
                Box::new(Type::Nil),
            ),
        );
        environment.borrow_mut().set(
            Token::Other("*ARGV*".to_string()),
            Type::List(Vec::new(), Box::new(Type::Nil)),
        );
        environment.deref().borrow_mut().set(
            Token::Other("*host-language*".to_string()),
            Type::String("Rust".to_string()),
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
            "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))",
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
        if let Some(file) = self.cli.file {
            self.environment.borrow_mut().set(
                Token::Other("*ARGV*".to_string()),
                Type::List(
                    self.cli
                        .rest
                        .iter()
                        .map(|value| Type::String(value.clone()))
                        .collect(),
                    Box::new(Type::Nil),
                ),
            );
            match run(format!("(load-file \"{}\")", file.display())) {
                Ok(output) => {
                    println!("{output}");
                }
                Err(error) => {
                    eprintln!("{error}");
                }
            }
            return;
        }
        run(r#"(println (str "Mal [" *host-language* "]"))"#.to_string()).unwrap();
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
                Type::List(ref list, _) if list.is_empty() => EvalResult::Type(ast),
                Type::List(_, _) => {
                    fn quasiquote(ast: &Type) -> Type {
                        let f = |items: &Vec<Type>| {
                            let mut result = Vec::new();
                            for item in items.iter().rev() {
                                if let Some((list, _)) = item.as_list() {
                                    if list.first().map_or(false, |first| {
                                        *first
                                            == Type::Symbol(Token::Other(
                                                "splice-unquote".to_string(),
                                            ))
                                    }) {
                                        result = vec![
                                            Type::Symbol(Token::Other("concat".to_string())),
                                            list.get(1).unwrap().clone(),
                                            Type::List(result, Box::new(Type::Nil)),
                                        ];
                                        continue;
                                    }
                                }
                                result = vec![
                                    Type::Symbol(Token::Other("cons".to_string())),
                                    quasiquote(item),
                                    Type::List(result, Box::new(Type::Nil)),
                                ];
                            }
                            Type::List(result, Box::new(Type::Nil))
                        };
                        match ast {
                            Type::Vector(vector, _) => Type::List(
                                vec![Type::Symbol(Token::Other("vec".to_string())), f(vector)],
                                Box::new(Type::Nil),
                            ),
                            Type::List(list, _) => {
                                if list.first().map_or(false, |first| {
                                    *first == Type::Symbol(Token::Other("unquote".to_string()))
                                }) {
                                    return list.get(1).unwrap().clone();
                                }
                                f(list)
                            }
                            Type::HashMap(_, _) | Type::Symbol(_) => Type::List(
                                vec![Type::Symbol(Token::Other("quote".to_string())), ast.clone()],
                                Box::new(Type::Nil),
                            ),
                            _ => ast.clone(),
                        }
                    }
                    fn is_macro_call(ast: &Type, environment: &Rc<RefCell<Environment>>) -> bool {
                        ast.as_list().is_some_and(|(list, _)| {
                            list.first().is_some_and(|item| {
                                item.as_symbol().is_some_and(|symbol| {
                                    environment.borrow().get(symbol).is_some_and(|value| {
                                        value.as_function().is_some_and(|(function, _)| {
                                            function
                                                .as_user_defined()
                                                .is_some_and(|function| function.is_macro)
                                        })
                                    })
                                })
                            })
                        })
                    }
                    fn macro_expand(ast: &Type, environment: &Rc<RefCell<Environment>>) -> Type {
                        let mut ast = ast.clone();
                        while is_macro_call(&ast, environment) {
                            ast = (environment
                                .borrow()
                                .get(
                                    ast.as_list()
                                        .unwrap()
                                        .0
                                        .first()
                                        .unwrap()
                                        .as_symbol()
                                        .unwrap(),
                                )
                                .unwrap()
                                .as_function()
                                .unwrap()
                                .0
                                .as_user_defined()
                                .unwrap()
                                .deref()
                                .function)(
                                &ast.as_list().unwrap().0[1..].iter().collect_vec()
                            )
                            .unwrap();
                        }
                        ast
                    }
                    ast = macro_expand(&ast, &environment);
                    if !ast.is_list() {
                        return eval_ast(ast, &environment);
                    }
                    let unevaluated = ast.as_list().unwrap().0.clone();
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
                                Type::List(list, _) => list,
                                Type::Vector(vector, _) => vector,
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
                                Type::List(
                                    unevaluated[1..unevaluated.len() - 1].to_vec(),
                                    Box::new(Type::Nil),
                                ),
                                &environment,
                            )?
                            .as_result_list()
                            .unwrap();
                            ast = ast.as_list().unwrap().0.last().unwrap().clone();
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
                        Some("fn*") => EvalResult::Type(Type::Function(
                            FunctionType::UserDefined(Box::new(UserDefined {
                                body: unevaluated.get(2).unwrap().clone(),
                                params: unevaluated.get(1).unwrap().clone(),
                                env: Rc::clone(&environment),
                                function: Rc::new(move |values| {
                                    enum BindRest<'a> {
                                        None,
                                        BindEmpty,
                                        BindFirstValue(&'a Type),
                                    }
                                    let body = unevaluated.get(2).unwrap();
                                    let params = unevaluated.get(1).unwrap();
                                    let mut new = Environment::new();
                                    new.outer = Some(Rc::clone(&environment));
                                    let new = Rc::new(RefCell::new(new));
                                    let mut bindings = match params {
                                        Type::List(list, _) => list,
                                        Type::Vector(vector, _) => vector,
                                        _ => unimplemented!(),
                                    }
                                    .iter()
                                    .map(|value| value.as_symbol().unwrap().clone());
                                    let mut binding_rest = BindRest::None;
                                    let mut values = values.iter().copied();
                                    for pair in bindings.by_ref().zip_longest(values.by_ref()) {
                                        match pair {
                                            EitherOrBoth::Both(binding, value) => {
                                                if binding == Token::Other("&".to_string()) {
                                                    binding_rest = BindRest::BindFirstValue(value);
                                                    break;
                                                }
                                                new.deref()
                                                    .borrow_mut()
                                                    .set(binding, value.clone().clone());
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
                                            new.deref().borrow_mut().set(
                                                next,
                                                Type::List(Vec::new(), Box::new(Type::Nil)),
                                            );
                                        }
                                        BindRest::BindFirstValue(first_value) => {
                                            let next = bindings.next().unwrap();
                                            let rest = once(first_value)
                                                .chain(values)
                                                .cloned()
                                                .collect_vec();
                                            new.deref()
                                                .borrow_mut()
                                                .set(next, Type::List(rest, Box::new(Type::Nil)));
                                        }
                                    }
                                    Ok(eval(body.clone(), new).unwrap().as_type().unwrap().clone())
                                }),
                                is_macro: false,
                            })),
                            Box::new(Type::Nil),
                        )),
                        Some("quote") => EvalResult::Type(unevaluated.get(1).unwrap().clone()),
                        Some("quasiquote") => {
                            ast = quasiquote(unevaluated.get(1).unwrap());
                            continue;
                        }
                        Some("quasiquoteexpand") => {
                            EvalResult::Type(quasiquote(unevaluated.get(1).unwrap()))
                        }
                        Some("defmacro!") => {
                            let result =
                                eval(unevaluated.get(2).unwrap().clone(), Rc::clone(&environment))?;
                            environment.deref().borrow_mut().set(
                                unevaluated.get(1).unwrap().as_symbol().unwrap().clone(),
                                match result {
                                    EvalResult::Type(Type::Function(
                                        FunctionType::UserDefined(ref function),
                                        _,
                                    )) => Type::Function(
                                        FunctionType::UserDefined(Box::new(UserDefined {
                                            is_macro: true,
                                            ..*(*function).clone()
                                        })),
                                        Box::new(Type::Nil),
                                    ),
                                    EvalResult::Type(ref v) => v.clone(),
                                    EvalResult::ResultList(_) => unimplemented!(),
                                },
                            );
                            result
                        }
                        Some("macroexpand") => EvalResult::Type(macro_expand(
                            unevaluated.get(1).unwrap(),
                            &environment,
                        )),
                        Some("try*") => {
                            match eval(unevaluated.get(1).unwrap().clone(), Rc::clone(&environment))
                            {
                                Ok(result) => result,
                                Err(error) => {
                                    let mut new = Environment::new();
                                    new.outer = Some(Rc::clone(&environment));
                                    let new = Rc::new(RefCell::new(new));
                                    let catch = unevaluated.get(2);
                                    if let Some(catch) = catch {
                                        let catch = catch.as_list().unwrap().0;
                                        new.borrow_mut().set(
                                            catch.get(1).unwrap().as_symbol().unwrap().clone(),
                                            error
                                                .as_custom()
                                                .unwrap_or(&Type::String(error.to_string()))
                                                .clone(),
                                        );
                                        eval(catch.get(2).unwrap().clone(), new)?
                                    } else {
                                        return Err(error);
                                    }
                                }
                            }
                        }
                        _ => {
                            let result = eval_ast(ast.clone(), &environment)?;
                            let evaluated = result.as_result_list().unwrap();
                            let [function, args @ ..] = evaluated.as_slice() else {
                                unimplemented!()
                            };
                            match function {
                                EvalResult::Type(Type::Function(
                                    FunctionType::Regular(function),
                                    _,
                                )) => EvalResult::Type(Rc::clone(function)(
                                    &evaluated
                                        .iter()
                                        .skip(1)
                                        .map(|item| item.as_type().unwrap())
                                        .collect_vec(),
                                )?),
                                EvalResult::Type(Type::Function(
                                    FunctionType::UserDefined(function),
                                    _,
                                )) => {
                                    enum BindRest<'a> {
                                        None,
                                        BindEmpty,
                                        BindFirstValue(&'a Type),
                                    }
                                    let UserDefined {
                                        body, params, env, ..
                                    } = &**function;
                                    ast = body.clone();
                                    let mut new = Environment::new();
                                    new.outer = Some(Rc::clone(env));
                                    let new = Rc::new(RefCell::new(new));
                                    let mut bindings = match params {
                                        Type::List(list, _) => list,
                                        Type::Vector(vector, _) => vector,
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
                                            new.deref().borrow_mut().set(
                                                next,
                                                Type::List(Vec::new(), Box::new(Type::Nil)),
                                            );
                                        }
                                        BindRest::BindFirstValue(first_value) => {
                                            let next = bindings.next().unwrap();
                                            let rest = values
                                                .chain(once(first_value))
                                                .cloned()
                                                .collect_vec();
                                            new.deref()
                                                .borrow_mut()
                                                .set(next, Type::List(rest, Box::new(Type::Nil)));
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
                    .ok_or(Error::OperatorNotFound(symbol.to_string()))?,
            ),
            Type::List(list, _) => EvalResult::ResultList(
                list.iter()
                    .map(|item| eval(item.clone(), Rc::clone(environment)))
                    .try_collect()?,
            ),
            Type::Vector(vector, _) => EvalResult::Type(Type::Vector(
                vector
                    .into_iter()
                    .map(|item| {
                        eval(item, Rc::clone(environment))
                            .map(|result| result.as_type().unwrap().clone())
                    })
                    .try_collect()?,
                Box::new(Type::Nil),
            )),
            Type::HashMap(hash_map, _) => EvalResult::Type(Type::HashMap(
                hash_map
                    .into_iter()
                    .map(|(key, value)| {
                        eval(value, Rc::clone(environment))
                            .map(|value| (key, value.as_type().unwrap().clone()))
                    })
                    .try_collect()?,
                Box::new(Type::Nil),
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

impl From<HashMapKey> for Type {
    fn from(key: HashMapKey) -> Self {
        match key {
            HashMapKey::Keyword(keyword) => Self::Keyword(keyword),
            HashMapKey::String(string) => Self::String(string),
        }
    }
}

#[derive(Clone, EnumAsInner)]
enum Type {
    List(Vec<Type>, Box<Type>),
    Vector(Vec<Type>, Box<Type>),
    HashMap(IndexMap<HashMapKey, Type>, Box<Type>),
    Symbol(Token),
    String(String),
    Number(f64),
    Nil,
    Bool(bool),
    Keyword(String),
    Function(FunctionType, Box<Type>),
    Atom(Rc<RefCell<Type>>),
}

#[derive(Clone, EnumAsInner)]
enum FunctionType {
    Regular(Function),
    UserDefined(Box<UserDefined>),
}

#[derive(Clone)]
struct UserDefined {
    body: Type,
    params: Type,
    env: Rc<RefCell<Environment>>,
    function: Function,
    is_macro: bool,
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::List(arg0, _) => f.debug_tuple("List").field(arg0).finish(),
            Self::Vector(arg0, _) => f.debug_tuple("Vector").field(arg0).finish(),
            Self::HashMap(arg0, _) => f.debug_tuple("HashMap").field(arg0).finish(),
            Self::Symbol(arg0) => f.debug_tuple("Symbol").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::Nil => write!(f, "Nil"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Keyword(arg0) => f.debug_tuple("Keyword").field(arg0).finish(),
            Self::Function(_, _) => write!(f, "Function"),
            Self::Atom(atom) => f.debug_tuple("Atom").field(&&**atom).finish(),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Vector(a, _) | Self::List(a, _), Self::Vector(b, _) | Self::List(b, _)) => {
                a == b
            }
            (Self::HashMap(a, _), Self::HashMap(b, _)) => a == b,
            (Self::Symbol(a), Self::Symbol(b)) => a == b,
            (Self::String(a), Self::String(b)) | (Self::Keyword(a), Self::Keyword(b)) => a == b,
            (Self::Number(a), Self::Number(b)) => a == b,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Function(_, _), Self::Function(_, _)) => false,
            _ => discriminant(self) == discriminant(other),
        }
    }
}

#[derive(Debug, Error, EnumAsInner)]
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
    #[error("operator at start of list not found (found {0})")]
    OperatorNotFound(String),
    #[error("index {0} out of range when evaluating {0}")]
    OutOfRange(usize, String),
    #[error("argument not found when evaluating {0}")]
    ArgumentNotFound(String),
    #[error("custom error thrown: {}", .0.print(false, false))]
    Custom(Type),
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
                (
                    Self::List(items, Box::new(Self::Nil)),
                    original_len - input.len() + 1,
                )
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
                (
                    Self::Vector(items, Box::new(Self::Nil)),
                    original_len - input.len() + 1,
                )
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
                (
                    Self::HashMap(items, Box::new(Self::Nil)),
                    original_len - input.len() + 1,
                )
            }
            Token::SingleQuote | Token::Backtick | Token::Tilde | Token::TildeAt | Token::At => {
                let (parameter, len) = Self::from_tokens(&input[1..])?;
                (
                    Self::List(
                        vec![
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
                        ],
                        Box::new(Self::Nil),
                    ),
                    len + 1,
                )
            }
            Token::Caret => {
                let (set, len1) = Self::from_tokens(&input[1..])?;
                let (value, len2) = Self::from_tokens(&input[(1 + len1)..])?;
                (
                    Self::List(
                        vec![
                            Self::Symbol(Token::Other("with-meta".to_string())),
                            value,
                            set,
                        ],
                        Box::new(Self::Nil),
                    ),
                    len1 + len2 + 1,
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
            Self::List(items, _) => format!(
                "({})",
                items
                    .iter()
                    .map(|value| value.print(display, quote_strings))
                    .collect_vec()
                    .join(" ")
            ),
            Self::Vector(items, _) => format!(
                "[{}]",
                items
                    .iter()
                    .map(|value| value.print(display, quote_strings))
                    .collect_vec()
                    .join(" ")
            ),
            Self::HashMap(items, _) => format!(
                "{{{}}}",
                items
                    .iter()
                    .flat_map(|(key, value)| [key.to_string(), value.print(display, quote_strings)])
                    .collect_vec()
                    .join(" ")
            ),
            Self::Keyword(string) => format!(":{string}"),
            Self::Function(_, _) => "#<function>".to_string(),
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

type Function = Rc<dyn Fn(&[&Type]) -> Result<Type, Error>>;

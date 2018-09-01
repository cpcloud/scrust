#![feature(box_syntax, fn_traits, unboxed_closures, box_patterns)]

use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Copy, Clone, PartialEq)]
enum SpecialForm {
    If,
    And,
    Or,
    Define,
    Lambda,
}

impl fmt::Display for SpecialForm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                SpecialForm::If => "if",
                SpecialForm::And => "and",
                SpecialForm::Or => "or",
                SpecialForm::Define => "define",
                SpecialForm::Lambda => "lambda",
            }
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
enum List {
    Cell(Box<Value>, Box<List>),
    Null,
}

impl List {
    fn car(&self) -> Option<Box<Value>> {
        match self.clone() {
            List::Cell(head, _) => Some(head),
            _ => None,
        }
    }

    fn cdr(&self) -> Box<List> {
        match self.clone() {
            List::Cell(_, tail) => tail,
            _ => box List::Null,
        }
    }

    fn nth(self, n: u64) -> Value {
        let mut value = self.car().unwrap();
        let mut rest = self.cdr();
        for i in 0..n {
            value = rest.car().unwrap();
            rest = rest.cdr();
        }
        *value
    }

    fn from_vec(values: Vec<Value>) -> List {
        if values.is_empty() {
            List::Null
        } else {
            let mut result = List::Cell(box values.last().unwrap().clone(), box List::Null);
            for value in values[..values.len() - 1].iter().rev() {
                result = List::Cell(box value.clone(), box result);
            }
            result
        }
    }

    fn as_value(&self) -> Value {
        Value::List(self.clone())
    }

    fn as_vec(&self) -> Vec<Value> {
        let mut res: Vec<_> = vec![];
        let mut cur = self.clone();

        while cur != List::Null {
            res.push(*cur.car().unwrap());
            cur = *cur.cdr();
        }
        res
    }

    fn map<F>(&self, f: F) -> List
    where
        F: FnMut(Value) -> Value,
    {
        List::from_vec(self.as_vec().into_iter().map(f).collect::<Vec<_>>())
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clone() {
            List::Cell(value, list) => write!(f, "{} -> {}", value, list),
            List::Null => write!(f, "*"),
        }
    }
}

#[cfg(test)]
mod test {
    fn test_create_list() -> () {}
}

#[derive(PartialEq, Clone, Debug)]
enum Procedure {
    Builtin(String),
    Function {
        argnames: Vec<String>,
        body: List,
        env: Environment,
    },
}

#[derive(PartialEq, Clone, Debug)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Symbol(String),
    List(List),
    Procedure(Procedure),
    SpecialForm(SpecialForm),
}

#[derive(PartialEq, Clone, Debug)]
pub struct Environment {
    parent: Option<Rc<Environment>>,
    values: HashMap<String, Value>,
}

fn reduce<F>(f: F, args: List) -> Value
where
    F: Fn(i64, i64) -> i64,
{
    Value::Integer(
        args.as_vec()
            .iter()
            .fold(0i64, |acc, element| match element {
                Value::Integer(value) => f(acc, *value),
                _ => panic!("Invalid operand to arithmetic operator: {:?}", element),
            }),
    )
}

impl Environment {
    fn new(parent: Option<Rc<Environment>>, values: HashMap<String, Value>) -> Environment {
        let mut env = Environment { parent, values };
        env.define("+", Value::Procedure(Procedure::Builtin("+".to_string())));
        env.define("-", Value::Procedure(Procedure::Builtin("-".to_string())));
        env.define("*", Value::Procedure(Procedure::Builtin("*".to_string())));
        env.define("/", Value::Procedure(Procedure::Builtin("/".to_string())));
        env
    }

    fn define(&mut self, name: &str, value: Value) -> () {
        self.values.insert(name.to_string(), value);
    }

    fn retrieve(self, name: String) -> Value {
        let msg = format!("name {:?} not in scope", &name);
        self.values.get(&name).expect(&msg).clone()
    }

    fn parent(self) -> Option<Rc<Environment>> {
        self.parent
    }

    fn new_child(self) -> Environment {
        Environment::new(Some(Rc::new(self)), HashMap::new())
    }

    fn empty() -> Environment {
        Environment::new(None, HashMap::new())
    }

    fn add(args: List) -> Value {
        reduce(|lhs, rhs| lhs + rhs, args)
    }

    fn sub(args: List) -> Value {
        reduce(|lhs, rhs| lhs - rhs, args)
    }

    fn mul(args: List) -> Value {
        reduce(|lhs, rhs| lhs * rhs, args)
    }

    fn div(args: List) -> Value {
        reduce(|lhs, rhs| lhs / rhs, args)
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.clone() {
            Value::Integer(value) => write!(f, "{}", value),
            Value::Boolean(value) => write!(f, "{}", if value { "#t" } else { "#f" }),
            Value::Symbol(value) => write!(f, "'{}", value),
            Value::List(list) => write!(f, "{}", list),
            Value::Procedure(_) => write!(f, "procedure"),
            Value::SpecialForm(form) => write!(f, "{}", form),
            _ => panic!("Foo!"),
        }
    }
}

macro_rules! int {
    [$value:expr] => {
        Value::Integer($value)
    }
}

macro_rules! sym {
    [$value:expr] => {
        Value::Symbol($value.to_string())
    }
}

macro_rules! list {
    [$($value:expr),*] => { List::from_vec(vec![$($value,)*]).as_value() }
}

macro_rules! spec {
    [$value:ident] => { Value::SpecialForm(SpecialForm::$value) }
}

macro_rules! bool_ {
    [$value:expr] => { Value::Boolean($value) }
}

fn apply(f: Procedure, args: List, env: &mut Environment) -> Value {
    let evaluated_arguments = args.map(|arg: Value| eval(arg, env));
    match f {
        Procedure::Builtin(name) => match &name[..] {
            "+" => Environment::add(evaluated_arguments),
            "-" => Environment::sub(evaluated_arguments),
            "*" => Environment::mul(evaluated_arguments),
            "/" => Environment::div(evaluated_arguments),
            _ => panic!("Function {:?}", name),
        },
        _ => panic!("User defined functions not yet supported"),
    }
}

fn evaluate_if(expr: Value, list: List, env: &mut Environment) -> Value {
    let cond = eval(list.clone().nth(1), env);
    let evaluated_condition = match cond {
        Value::Boolean(value) => value,
        _ => panic!("invalid condition {:?}", expr),
    };
    let conseq = list.clone().nth(2);
    let alt = list.clone().nth(3);
    eval(if evaluated_condition { conseq } else { alt }, env)
}

fn evaluate_define(expr: Value, args: List, env: &mut Environment) -> Value {
    let mut def_env = env.new_child();
    let func_name = match args.clone().nth(1) {
        Value::Symbol(name) => name,
        _ => panic!(),
    };
    let argnames = args.cdr().car().unwrap().as_list().as_vec().map(|x| match x { Value::Symbol(n) => n, _ => panic!() });
    let funcbody = args.cdr().cdr();
    env.define(&func_name, Value::Procedure(Procedure::Function { argnames: arg}));
    List::Null.as_value()
}

fn eval(expr: Value, env: &mut Environment) -> Value {
    match expr {
        Value::Integer(val) => Value::Integer(val),
        Value::Symbol(name) => env.clone().retrieve(name),
        Value::List(ref list) => {
            let first = eval(list.clone().nth(0), env);
            match first {
                Value::SpecialForm(form) => match form {
                    SpecialForm::If => evaluate_if(expr.clone(), list.clone(), env),
                    SpecialForm::Define => evaluate_define(expr.clone(), *list.cdr(), env),
                    _ => panic!("Invalid special form {:?}", first),
                },
                Value::Procedure(func) => apply(func, *list.clone().cdr(), env),
                _ => expr.clone(),
            }
        }
        _ => expr,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_arith() -> () {
        let mut env = Environment::empty();
        env.define("a", int![1]);
        env.define("b", int![42]);
        let cond = list![sym!["+"], sym!["a"], sym!["b"]];
        assert_eq!(eval(cond, &mut env), Value::Integer(43));
    }

    #[test]
    fn test_eval_define() -> () {
        let mut env = Environment::empty();
        let expr = list![
            spec![Define],
            list![sym!["x"]],
            list![sym!["+"], sym!["a"], sym!["a"]]
        ];
    }
}

fn main() {}

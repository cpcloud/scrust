#![feature(box_syntax, fn_traits, unboxed_closures)]

use std::collections::HashMap;
use std::fmt;
use std::ops::Fn;
use std::rc::Rc;
use std::slice::Iter;

type Expr = Rc<Evaluable>;
type Env = HashMap<String, Expr>;

fn make_expr<T: Evaluable>(val: T) -> Expr {
    Rc::new(val)
}

trait Evaluable: fmt::Display {
    fn eval(&self, env: Env) -> Expr;
}

impl Evaluable for i64 {
    fn eval(&self, _: Env) -> Expr {
        make_expr(*self)
    }
}

impl Evaluable for f64 {
    fn eval(&self, _: Env) -> Expr {
        make_expr(*self)
    }
}

#[derive(PartialEq, Eq, Debug)]
struct Symbol {
    name: String,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}", self.name)
    }
}

impl Evaluable for Symbol {
    fn eval(&self, env: Env) -> Expr {
        let res = env.get(&self.name);
        let unwrapped = res.unwrap();
        let cloned = unwrapped;
        unwrapped.clone()
    }
}

struct List {
    items: Vec<Expr>,
}

impl List {
    fn iter(&self) -> Iter<Expr> {
        self.items.iter()
    }
}

macro_rules! list {
    [$($value:expr),*] => {
        {
            let mut items: Vec<Expr> = vec![];
            $(
                items.push(make_expr($value));
            )*
            List { items }
        }
    }
}

struct Func<F>
where
    F: Fn(List) -> Expr,
{
    func: F,
}

impl<F: Fn(List) -> Expr> Evaluable for Func<F> {
    fn eval(&self, env: Env) -> Expr {
        make_expr(*self)
    }
}

impl<F: Fn(List) -> Expr> fmt::Display for Func<F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "procedure")
    }
}

impl Evaluable for List {
    fn eval(&self, env: Env) -> Expr {
        make_expr(*self)
    }
}

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted = self
            .iter()
            .map(|element| format!("{}", element))
            .collect::<Vec<String>>()
            .join(" ");
        write!(f, "({})", formatted)
    }
}

macro_rules! sym {
    [$value:expr] => {
        Symbol { name: $value }
    }
}

fn main() {
    let mut env: Env = HashMap::new();
    env.insert("a".to_string(), make_expr(1));
    env.insert(
        "+".to_string(),
        make_expr(Func {
            func: &|args: List| -> Expr { make_expr(args.iter().sum()) },
        }),
    );

    let expr = list![sym!["+".to_string()], sym!["a".to_string()], 2];
    let evald = expr.eval(env);
    println!("{:?}", 1)
}

pub type Location = String;
pub type Variable = String;

use std::fmt;
use crate::typing::types::Type;

#[derive(Clone, Debug)]
pub enum LitKind {
    Bool(bool),
    Unit,
}

#[derive(Clone, Debug)]
pub enum GateKind {
    X,
    Z,
    H,
    S
}

#[derive(Clone, Debug)]
pub enum MeasBase {
    X,
    Z
}

impl fmt::Display for GateKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GateKind::X => write!(f, "X"),
            GateKind::Z => write!(f, "Z"),
            GateKind::H => write!(f, "H"),
            GateKind::S => write!(f, "S")
        }
    }
}

impl fmt::Display for MeasBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MeasBase::X => write!(f, "X"),
            MeasBase::Z => write!(f, "Z"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    /// A variable
    Var(Variable),
    /// A literal (e.g., `true`, ...)
    Lit(LitKind),
    /// A qubit allocation
    Init(Variable, Location, Box<Expr>),
    /// A qubit deallocation (e.g., `free q`)
    Free(Variable),
    /// A gate call
    GateCall(GateKind, Variable),
    /// A quantum measurement
    Measure(Variable, Vec<MeasBase>, Vec<Variable>, Box<Expr>),
    /// A mkref expression
    MkRef(Variable, Box<Expr>, Box<Expr>),
    /// A dereference expression
    Deref(Box<Expr>),
    /// An update reference expression
    Update(Variable, Box<Expr>),
    /// A if expression
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    /// A while expression
    While(Box<Expr>, Box<Expr>),
    /// A function call
    Call(Variable, Vec<Location>, Vec<Variable>),

    Seq(Box<Expr>, Box<Expr>)
}

#[derive(Clone, Debug)]
pub struct Expr {
    kind: ExprKind,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Expr {
        Expr {
            kind
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn unit() -> Expr {
        Expr::new(ExprKind::Lit(LitKind::Unit))
    }

    pub fn var(x: Variable) -> Expr {
        Expr::new(ExprKind::Var(x))
    }

    pub fn init(x: Variable, l: Location, e: Expr) -> Expr {
        Expr::new(ExprKind::Init(x, l, Box::new(e)))
    }

    pub fn free(x: Variable) -> Expr {
        Expr::new(ExprKind::Free(x))
    }

    pub fn mkref(x: Variable, e1: Expr, e2: Expr) -> Expr {
        Expr::new(ExprKind::MkRef(x, Box::new(e1), Box::new(e2)))
    }

    pub fn deref(e: Expr) -> Expr {
        Expr::new(ExprKind::Deref(Box::new(e)))
    }

    pub fn update(x: Variable, e: Expr) -> Expr {
        Expr::new(ExprKind::Update(x, Box::new(e)))
    }

    pub fn if_(e1: Expr, e2: Expr, e3: Option<Expr>) -> Expr {
        Expr::new(ExprKind::If(Box::new(e1), Box::new(e2), e3.map(|e3| Box::new(e3))))
    }

    pub fn while_(e1: Expr, e2: Expr) -> Expr {
        Expr::new(ExprKind::While(Box::new(e1), Box::new(e2)))
    }

    pub fn call(f: Variable, locs: Vec<Location>, args: Vec<Variable>) -> Expr {
        Expr::new(ExprKind::Call(f, locs, args))
    }

    pub fn seq(e1: Expr, e2: Expr) -> Expr {
        Expr::new(ExprKind::Seq(Box::new(e1), Box::new(e2)))
    }

    // For printing
    fn fmt_impl(&self, f: &mut fmt::Formatter, depth: usize) -> fmt::Result {
        let indent = " ".repeat(depth * 2);
        write!(f, "{}", indent)?;
        match self.kind() {
            ExprKind::Var(x) => {
                write!(f, "Var({})", x)
            },
            ExprKind::Lit(kind) => {
                match kind {
                    LitKind::Unit => write!(f, "Unit()"),
                    LitKind::Bool(b) => write!(f, "Bool({})", b)
                }
            },
            ExprKind::Init(x, l, e) => {
                write!(f, "Init(\n")?;
                write!(f, "{}", indent)?;
                write!(f, "  {},\n", x)?;
                write!(f, "{}", indent)?;
                write!(f, "  {},\n", l)?;
                e.fmt_impl(f, depth + 1)?;
                write!(f, ")")
            }
            ExprKind::Free(x) => {
                write!(f, "Free({})", x)
            },
            ExprKind::GateCall(gate, x) => {
                write!(f, "GateCall({}, {})", gate, x)
            },
            ExprKind::Measure(x, _mbasis, args, e) => {
                write!(f, "Meas(\n")?;
                write!(f, "{}", indent)?;
                write!(f, "  {},\n", x)?;
                write!(f, "{}", indent)?;
                write!(f, "  [")?;
                for (i, y) in args.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{}", y)?;
                    } else {
                        write!(f, ", {}", y)?;
                    }
                }
                write!(f, "],\n")?;
                e.fmt_impl(f, depth + 1)?;
                write!(f, ")")
            },
            ExprKind::MkRef(x, e1, e2) => {
                write!(f, "MkRef(\n")?;
                write!(f, "{}", indent)?;
                write!(f, "  {},\n", x)?;
                e1.fmt_impl(f, depth + 1)?;
                write!(f, ",\n")?;
                e2.fmt_impl(f, depth + 1)?;
                write!(f, ")")
            },
            ExprKind::Deref(e) => {
                write!(f, "Deref(\n")?;
                e.fmt_impl(f, depth + 1)?;
                write!(f, ")")
            },
            ExprKind::Update(x, e) => {
                write!(f, "Update(\n")?;
                write!(f, "{}", indent)?;
                write!(f, "  {},\n", x)?;
                e.fmt_impl(f, depth + 1)?;
                write!(f, ")")
            },
            ExprKind::If(e1, e2, e3) => {
                write!(f, "If(\n")?;
                e1.fmt_impl(f, depth + 1)?;
                write!(f, ",\n")?;
                e2.fmt_impl(f, depth + 1)?;
                if let Some(e3) = e3 {
                    write!(f, ",\n")?;
                    e3.fmt_impl(f, depth + 1)?;
                }
                write!(f, ")")
            },
            ExprKind::While(e1, e2) => {
                write!(f, "While(\n")?;
                e1.fmt_impl(f, depth + 1)?;
                write!(f, ",\n")?;
                e2.fmt_impl(f, depth + 1)?;
                write!(f, ")")
            },
            ExprKind::Call(x, ls, args) => {
                write!(f, "Call(\n")?;
                write!(f, "  {},\n", x)?;
                write!(f, "  [")?;
                for (i, l) in ls.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{}", l)?;
                    } else {
                        write!(f, ", {}", l)?;
                    }
                }
                write!(f, "],\n")?;
                write!(f, "  [")?;
                for (i, x) in args.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{}", x)?;
                    } else {
                        write!(f, ", {}", x)?;
                    }
                }
                write!(f, "])")
            },
            ExprKind::Seq(e1, e2) => {
                write!(f, "Seq(\n")?;
                e1.fmt_impl(f, depth + 1)?;
                write!(f, ",\n")?;
                e2.fmt_impl(f, depth + 1)?;
                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_impl(f, 0)
    }
}

#[derive(Clone, Debug)]
pub struct FuncDecl {
    fname: Variable,
    locs: Vec<Location>,
    args: Vec<(Variable, Type)>, // types are annotated
    expr: Expr
}

impl FuncDecl {
    pub fn new(fname: Variable, locs: Vec<Location>, args: Vec<(Variable, Type)>, expr: Expr) -> FuncDecl {
        FuncDecl {
            fname,
            locs,
            args,
            expr
        }
    }

    pub fn fname(&self) -> &Variable {
        &self.fname
    }

    pub fn locations(&self) -> &Vec<Location> {
        &self.locs
    }

    pub fn args(&self) -> &Vec<(Variable, Type)> {
        &self.args
    }

    pub fn expr(&self) -> &Expr {
        &self.expr
    }
}

pub type FuncDecls = Vec<FuncDecl>;

#[derive(Clone, Debug)]
pub struct Program {
    decls: FuncDecls,
    expr: Expr
}

impl Program {
    pub fn new(decls: FuncDecls, expr: Expr) -> Program {
        Program {
            decls,
            expr
        }
    }

    pub fn decls(&self) -> &FuncDecls {
        &self.decls
    }

    pub fn main_expr(&self) -> &Expr {
        &self.expr
    }
}

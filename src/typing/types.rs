use std::fmt;
use std::collections::{HashMap, HashSet, LinkedList};
use crate::ast::ast::{Location, Variable};

pub type LocSubst = HashMap<Location, Location>;
pub type CommandSequence = LinkedList<Command>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Unit,
    Qbit(Location),
    Ref(Box<Type>)
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Bool => write!(f, "bool"),
            Type::Unit => write!(f, "unit"),
            Type::Qbit(l) => write!(f, "qbit({})", l),
            Type::Ref(ty) => write!(f, "ref {}", *ty)
        }
    }
}

#[derive(Debug, Clone)]
pub struct FuncType {
    locations: Vec<Location>,
    args: Vec<(Variable, Type)>,
    ret_type: Type,
    ret_tyenv: TypeEnv,
    commands: CommandSequence,
}

impl FuncType {
    pub fn new(locations: Vec<Location>, args: Vec<(Variable, Type)>, ret_type: Type, ret_tyenv: TypeEnv, commands: CommandSequence) -> FuncType {
        FuncType {
            locations,
            args,
            ret_type,
            ret_tyenv,
            commands
        }
    }

    pub fn locations(&self) -> &Vec<Location> {
        &self.locations
    }

    pub fn args(&self) -> &Vec<(Variable, Type)> {
        &self.args
    }

    pub fn ret_type(&self) -> &Type {
        &self.ret_type
    }

    pub fn ret_tyenv(&self) -> &TypeEnv {
        &self.ret_tyenv
    }

    pub fn commands(&self) -> &CommandSequence {
        &self.commands
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeEnv {
    map: HashMap<Variable, Type>,
    locations: HashSet<Location>,
}

#[derive(Debug, Clone)]
pub struct FuncTypeEnv {
    map: HashMap<Variable, FuncType>,
}

#[derive(Debug, Clone)]
pub enum Command {
    Merge(Location, Location),
    Alloc(Location),
    Free(Location),
    Loop(CommandSequence),
    Branch(CommandSequence, CommandSequence)
}

impl TypeEnv {
    pub fn new() -> TypeEnv {
        TypeEnv {
            map: HashMap::new(),
            locations: HashSet::new(),
        }
    }

    pub fn contains(&self, var: &Variable) -> bool {
        self.map.contains_key(var)
    }

    pub fn get(&self, var: &Variable) -> Option<&Type> {
        self.map.get(var)
    }

    pub fn insert(&mut self, key: Variable, ty: Type) {
        self.map.insert(key, ty);
    }

    pub fn remove(&mut self, key: &Variable) {
        self.map.remove(key);
    }

    pub fn insert_loc(&mut self, l: Location) {
        self.locations.insert(l);
    }

    pub fn remove_loc(&mut self, l: &Location) -> bool {
        self.locations.remove(l)
    }

    pub fn append(&mut self, tyenv: &TypeEnv) {
        self.map.extend(tyenv.map.iter().map(|(k, v)| (k.clone(), v.clone())));
        self.locations.extend(tyenv.locations.clone().into_iter());
    }

    pub fn subst_locs(&mut self, subst: &LocSubst) {
        for (_, ty) in self.map.iter_mut() {
            subst_locs_ty(ty, subst);
        }
        self.locations = self.locations.iter().map(|l| subst.get(l).unwrap().clone()).collect::<HashSet<_>>();
    }
}

impl FuncTypeEnv {
    pub fn new() -> FuncTypeEnv {
        FuncTypeEnv {
            map: HashMap::new()
        }
    }

    pub fn contains(&self, f: &Variable) -> bool {
        self.map.contains_key(f)
    }

    pub fn insert(&mut self, f: Variable, fty: FuncType) {
        self.map.insert(f, fty);
    }

    pub fn get(&self, f: &Variable) -> Option<&FuncType> {
        self.map.get(f)
    }
}

pub fn subst_locs_ty(ty: &Type, subst: &LocSubst) -> Type {
    match ty {
        Type::Bool => Type::Bool,
        Type::Unit => Type::Unit,
        Type::Qbit(l) => Type::Qbit(subst.get(l).unwrap().clone()),
        Type::Ref(ty) => subst_locs_ty(ty, subst)
    }
}

pub fn subst_locs_command(command: &Command, subst: &LocSubst) -> Command {
    match command {
        Command::Merge(l1, l2) => Command::Merge(subst.get(l1).unwrap().clone(), subst.get(l2).unwrap().clone()),
        Command::Alloc(l) => Command::Alloc(subst.get(l).unwrap().clone()),
        Command::Free(l) => Command::Free(subst.get(l).unwrap().clone()),
        Command::Loop(cs) => Command::Loop(subst_locs_commands(cs, subst)),
        Command::Branch(cs1, cs2) => {
            let cs1 = subst_locs_commands(cs1, subst);
            let cs2 = subst_locs_commands(cs2, subst);
            Command::Branch(cs1, cs2)
        }
    }
}

pub fn subst_locs_commands(commands: &CommandSequence, subst: &LocSubst) -> CommandSequence {
    commands.iter().map(|c| subst_locs_command(c, subst)).collect::<CommandSequence>()
}

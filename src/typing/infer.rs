use std::collections::{HashSet, LinkedList};
use crate::typing::types::*;
use crate::typing::error::TypingError;
use crate::ast::ast::*;
use crate::arch_graph::*;

type Result<T> = std::result::Result<T, TypingError>;

// Commands in reversed order
pub fn infer_exp(ftyenv: &FuncTypeEnv, mut tyenv: TypeEnv, exp: &Box<Expr>) -> Result<(Type, TypeEnv, LinkedList<Command>)> {
    // debug print
    //println!("");
    //println!("[infer_exp] exp:\n{}", exp);
    //println!("[infer_exp] ftyenv: {:?}", ftyenv);
    //println!("[infer_exp] tyenv: {:?}", tyenv);
    //println!("");
    match exp.kind() {
        ExprKind::Var(x) => {
            let ty = tyenv.get(x).unwrap().clone();
            Ok((ty, tyenv, LinkedList::new()))
        },
        ExprKind::Lit(lit_kind) => {
            let ty = match lit_kind {
                LitKind::Bool(_) => Type::Bool,
                LitKind::Unit    => Type::Unit
            };
            Ok((ty, tyenv, LinkedList::new()))
        },
        ExprKind::Init(x, loc, e) => {
            let found = tyenv.remove_loc(loc);
            assert!(found);
            tyenv.insert(x.clone(), Type::Qbit(loc.clone()));
            let (ty, mut tyenv, mut commands) = infer_exp(ftyenv, tyenv, e)?;
            tyenv.remove(x);
            commands.push_front(Command::Alloc(loc.clone()));
            Ok((ty, tyenv, commands))
        },
        ExprKind::Free(x) => {
            assert!(tyenv.contains(x));
            match tyenv.get(x).unwrap().clone() {
                Type::Qbit(loc) => {
                    tyenv.remove(x);
                    tyenv.insert_loc(loc.clone());
                    let mut commands = LinkedList::new();
                    commands.push_front(Command::Free(loc.clone()));
                    Ok((Type::Unit, tyenv, commands))
                },
                ty => { Err(TypingError::TypeMismatch(Type::Qbit(Location::from("_")), ty)) }
            }
        },
        ExprKind::GateCall(_, x) => {
            assert!(tyenv.contains(x));
            match tyenv.get(x).unwrap() {
                Type::Qbit(_) => Ok((Type::Unit, tyenv, LinkedList::new())),
                ty => Err(TypingError::TypeMismatch(Type::Qbit(Location::from("_")), ty.clone()))
            }
        },
        ExprKind::Measure(x, mbases, args, e) => {
            assert!(1 <= args.len() && args.len() <= 2);
            assert!(args.len() == mbases.len());
            assert!(args.iter().all(|x| tyenv.contains(x))); // undefined identifier
            let mut locs = Vec::new();
            for arg in args {
                match tyenv.get(arg).unwrap() {
                    Type::Qbit(loc) => locs.push(loc.clone()),
                    ty => { return Err(TypingError::TypeMismatch(Type::Qbit(Location::from("_")), ty.clone())); }
                }
            }
            tyenv.insert(x.clone(), Type::Bool);
            let (ty, mut tyenv, mut commands) = infer_exp(ftyenv, tyenv, e)?;
            tyenv.remove(x);
            if locs.len() == 2 {
                commands.push_front(Command::Merge(locs[0].clone(), locs[1].clone()));
            }
            Ok((ty, tyenv, commands))
        },
        ExprKind::MkRef(x, e1, e2) => {
            let (ty1, mut tyenv, mut commands1) = infer_exp(ftyenv, tyenv, e1)?;
            tyenv.insert(x.clone(), Type::Ref(Box::new(ty1)));
            let (ty2, mut tyenv, mut commands2) = infer_exp(ftyenv, tyenv, e2)?;
            tyenv.remove(x);
            commands1.append(&mut commands2);
            Ok((ty2, tyenv, commands1))
        },
        ExprKind::Deref(e) => {
            let (ty, tyenv, commands) = infer_exp(ftyenv, tyenv, e)?;
            match ty {
                Type::Ref(ty) => Ok((*ty.clone(), tyenv, commands)),
                ty => Err(TypingError::ExpectedReference(ty))
            }
        },
        ExprKind::Update(x, e) => {
            assert!(tyenv.contains(x)); // undefined identifier
            let lty = tyenv.get(x).unwrap().clone();
            let (rty, tyenv, commands) = infer_exp(ftyenv, tyenv, e)?;
            match lty {
                Type::Ref(lty) => {
                    if *lty == rty {
                        Ok((Type::Unit, tyenv, commands))
                    } else {
                        Err(TypingError::TypeMismatch(*lty, rty))
                    }
                },
                lty => {
                    Err(TypingError::TypeMismatch(Type::Ref(Box::new(rty)), lty))
                }
            }
        },
        ExprKind::If(e1, e2, e3) => {
            let (ty1, tyenv, mut commands1) = infer_exp(ftyenv, tyenv, e1)?;
            match ty1 {
                Type::Bool => {
                    let tyenv2 = tyenv.clone();
                    let (ty2, tyenv2, commands2) = infer_exp(ftyenv, tyenv2, e2)?;
                    let (ty3, tyenv3, commands3) = if let Some(e3) = e3 {
                        infer_exp(ftyenv, tyenv, e3)?
                    } else {
                        (Type::Unit, tyenv, LinkedList::new())
                    };
                    if ty2 != ty3 {
                        Err(TypingError::TypeMismatch(ty2.clone(), ty3.clone()))
                    } else if tyenv2 != tyenv3 {
                        Err(TypingError::DifferentAllocationState)
                    } else {
                        commands1.push_back(Command::Branch(commands2, commands3));
                        Ok((ty2, tyenv2, commands1))
                    }
                },
                ty => Err(TypingError::TypeMismatch(Type::Bool, ty))
            }
        },
        ExprKind::While(e1, e2) => {
            let original_tyenv = tyenv.clone();
            let (ty1, tyenv, mut commands1) = infer_exp(ftyenv, tyenv, e1)?;
            let (ty2, tyenv, mut commands2) = infer_exp(ftyenv, tyenv, e2)?;
            if tyenv != original_tyenv {
                return Err(TypingError::ViolateLoopInvariant);
            }
            match (ty1, ty2) {
                (Type::Bool, Type::Unit) => {
                    let mut commands = LinkedList::new();
                    let mut command3 = commands1.clone();
                    commands1.append(&mut commands2);
                    commands.push_back(Command::Loop(commands1));
                    commands.append(&mut command3);
                    Ok((Type::Unit, tyenv, commands))
                },
                (ty1, ty2) => {
                    if ty1 != Type::Bool {
                        Err(TypingError::TypeMismatch(Type::Bool, ty1))
                    } else {
                        Err(TypingError::TypeMismatch(Type::Unit, ty2))
                    }
                }
            }
        },
        ExprKind::Call(f, locs, args) => {
            assert!(ftyenv.contains(f)); // undefined function
            let fty = ftyenv.get(f).unwrap();
            assert!(fty.locations().len() == locs.len()); // length mismatch
            assert!(fty.args().len() == args.len());
            let subst: LocSubst = fty.locations().iter().zip(locs.iter()).map(|(l1, l2)| (l1.clone(), l2.clone())).collect();

            let is_ok_arg_types = args.iter().zip(fty.args()).all(|(x, (_, farg_ty))| {
                let actual_arg_ty = tyenv.get(x).unwrap();
                let expected_arg_ty = subst_locs_ty(farg_ty, &subst);
                *actual_arg_ty == expected_arg_ty
            });
            assert!(is_ok_arg_types); // type mismatch
                                      //
            let locs_in_args = fty.args().iter()
                .fold(HashSet::new(), |mut set, (_, ty)| { free_location_vars(ty, &mut set); set });
            for l in fty.locations() {
                let l = subst.get(l).unwrap();
                if !locs_in_args.contains(l) {
                    tyenv.remove_loc(l);
                }
            }
            for x in args {
                tyenv.remove(x);
            }
            let ret_tyenv = {
                let mut ret_tyenv = fty.ret_tyenv().clone();
                ret_tyenv.subst_locs(&subst);
                ret_tyenv
            };
            tyenv.append(&ret_tyenv);
            Ok((subst_locs_ty(fty.ret_type(), &subst), tyenv, subst_locs_commands(fty.commands(), &subst)))
        },
        ExprKind::Seq(e1, e2) => {
            let (_, tyenv, mut commands1) = infer_exp(ftyenv, tyenv, e1)?;
            let (ty2, tyenv, mut commands2) = infer_exp(ftyenv, tyenv, e2)?;
            commands1.append(&mut commands2);
            Ok((ty2, tyenv, commands1))
        }
    }
}


pub fn infer_program(g: &ArchGraph, prog: Program) -> Result<(Type, CommandSequence)> {
    let mut ftyenv = FuncTypeEnv::new();
    for decl in prog.decls() {
        let mut tyenv = TypeEnv::new();
        for l in decl.locations() {
            tyenv.insert_loc(l.clone());
        }
        for (x, ty) in decl.args() {
            tyenv.insert(x.clone(), ty.clone());
        }
        let (ty, tyenv, commands) = infer_exp(&ftyenv, tyenv, &Box::new(decl.expr().clone()))?;
        let fty = FuncType::new(decl.locations().clone(), decl.args().clone(), ty, tyenv, commands);
        ftyenv.insert(decl.fname().clone(), fty);
    }

    let mut tyenv = TypeEnv::new();
    g.nodes().iter().for_each(|&l| tyenv.insert_loc(l.clone()));
    let (ty, _, commands) = infer_exp(&ftyenv, tyenv, &Box::new(prog.main_expr().clone()))?;
    Ok((ty, commands))
}


fn free_location_vars(ty: &Type, ret: &mut HashSet<Location>) {
    match ty {
        Type::Bool | Type::Unit => {},
        Type::Qbit(l) => { ret.insert(l.clone()); }
        Type::Ref(ty) => free_location_vars(&*ty, ret)
    }
}

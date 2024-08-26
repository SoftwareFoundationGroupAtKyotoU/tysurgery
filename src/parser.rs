use chumsky::prelude::*;

use crate::ast::ast::*;
use crate::typing::types::*;

pub fn variable() -> impl Parser<char, Variable, Error = Simple<char>> {
    text::ident().padded()
}

pub fn variable_seq() -> impl Parser<char, Vec<Variable>, Error = Simple<char>> {
    variable().separated_by(just(','))
}

pub fn quantum_gate() -> impl Parser<char, GateKind, Error = Simple<char>> {
    let z = text::keyword("Z").map(|_| GateKind::Z);
    let x = text::keyword("X").map(|_| GateKind::X);
    let h = text::keyword("H").map(|_| GateKind::H);
    let s = text::keyword("S").map(|_| GateKind::S);
    z.or(x).or(h).or(s)
}

fn measure_basis() -> impl Parser<char, MeasBase, Error = Simple<char>> {
    let z = text::keyword("Z").map(|_| MeasBase::Z);
    let x = text::keyword("X").map(|_| MeasBase::X);
    z.or(x)
}

pub fn expr() -> impl Parser<char, Expr, Error = Simple<char>> {
    let stmt = recursive(|expr| {
        let expr = expr.padded();
        let keyword_let = text::keyword("let").padded();
        let keyword_mkref = text::keyword("mkref").padded();
        let keyword_in = text::keyword("in").padded();
        let keyword_if = text::keyword("if").padded();
        let keyword_else = text::keyword("else").padded();
        let keyword_while = text::keyword("while").padded();
        let keyword_init = text::keyword("init").padded();
        let keyword_free = text::keyword("free").padded();

        let expr_seq = expr.clone()
            .then(just(';').padded()
                  .ignore_then(expr.clone())
                  .repeated())
            .map(|(e1, es)| {
                es.iter().fold(e1, |acc, e| { Expr::seq(acc, e.clone()) } )
            });

        let unit = just("()")
            .map(|_| Expr::new(ExprKind::Lit(LitKind::Unit)));

        let lit_true = text::keyword("true").padded().map(|_| Expr::new(ExprKind::Lit(LitKind::Bool(true))));
        let lit_false = text::keyword("false").padded().map(|_| Expr::new(ExprKind::Lit(LitKind::Bool(false))));

        let var = variable().map(|x| Expr::var(x));

        let qinit = keyword_let.clone()
            .ignore_then(variable())
            .then_ignore(just("=").padded())
            .then_ignore(keyword_init.clone())
            .then(variable().delimited_by(just("("), just(")")))
            .then_ignore(keyword_in.clone())
            .then(expr_seq.clone())
            .map(|((x, loc), e)| Expr::init(x, loc, e));

        let free = keyword_free.clone()
            .ignore_then(variable())
            .map(|x| Expr::free(x));

        let app_gate = quantum_gate()
            .then(variable().delimited_by(just("("), just(")")))
            .map(|(gate, arg)| Expr::new(ExprKind::GateCall(gate, arg)));

        let measure = keyword_let.clone()
            .ignore_then(variable())
            .then_ignore(just("=").padded())
            .then_ignore(text::keyword("measure"))
            .then(measure_basis().separated_by(just(',')).delimited_by(just('['), just(']')))
            .then(variable_seq().delimited_by(just("("), just(")")))
            .then_ignore(keyword_in.clone())
            .then(expr_seq.clone())
            .map(|(((x, mbases), args), e)| {
                Expr::new(ExprKind::Measure(x, mbases, args, Box::new(e)))
            });

        let mkref = keyword_let.clone()
            .ignore_then(variable())
            .then_ignore(just("=").padded())
            .then_ignore(keyword_mkref)
            .then(expr_seq.clone())
            .then_ignore(keyword_in.clone())
            .then(expr_seq.clone())
            .map(|((x, e1), e2)| Expr::mkref(x, e1, e2) );

        let deref = just('*').padded()
            .ignore_then(expr.clone())
            .map(|e| Expr::deref(e));

        let update = variable()
            .then_ignore(just(":=").padded())
            .then(expr.clone())
            .map(|(x, e)| Expr::update(x, e));

        let else_clause = keyword_else
            .ignore_then(expr_seq.clone().delimited_by(just("{"), just("}")).map(|e| Some(e)));
        let else_empty_clause = empty().map(|_| None);
        let if_exp = keyword_if.clone()
            .ignore_then(expr_seq.clone())
            .then(expr_seq.clone().delimited_by(just("{"), just("}")))
            .then(else_clause.or(else_empty_clause))
            .map(|((e1, e2), e3)| Expr::if_(e1, e2, e3));

        let while_exp = keyword_while.clone()
            .ignore_then(expr_seq.clone())
            .then(expr_seq.clone().delimited_by(just("{"), just("}")))
            .map(|(e1, e2)| Expr::while_(e1, e2));

        let call = variable()
            .then(variable_seq().delimited_by(just('[').padded(), just(']').padded()))
            .then(variable_seq().delimited_by(just('(').padded(), just(')').padded()))
            .map(|((f, locs), args)| Expr::call(f, locs, args));

        unit
            .or(lit_true)
            .or(lit_false)
            .or(qinit)
            .or(free)
            .or(app_gate)
            .or(measure)
            .or(mkref)
            .or(deref)
            .or(update)
            .or(if_exp)
            .or(while_exp)
            .or(call)
            .or(var) // must be here
    });

    stmt.clone()
        .then(just(';').padded()
              .ignore_then(stmt.clone())
              .repeated())
        .map(|(e1, es)| {
            es.iter().fold(e1, |acc, e| { Expr::seq(acc, e.clone()) } )
        })
}

pub fn parser_type() -> impl Parser<char, Type, Error = Simple<char>> {
    let tybool = text::keyword("bool").map(|_| Type::Bool);
    let tyunit = text::keyword("unit").map(|_| Type::Unit);
    let tyqbit = text::keyword("qbit")
        .ignore_then(variable().delimited_by(just('(').padded(), just(')').padded()))
        .map(|l| Type::Qbit(l));

    tybool
        .or(tyunit)
        .or(tyqbit)
}

pub fn func_decl() -> impl Parser<char, FuncDecl, Error = Simple<char>> {
    let loc_seq = variable_seq().delimited_by(just('[').padded(), just(']').padded());
    let loc_seq = loc_seq.or(empty().map(|_| Vec::new()));

    let arg = variable()
        .then_ignore(just(':').padded())
        .then(parser_type());
    let args = arg.separated_by(just(',').padded());

    loc_seq
        .then_ignore(text::keyword("def"))
        .then(variable()) // function name
        .then(args.delimited_by(just('(').padded(), just(')').padded()))
        .then(expr().delimited_by(just('{').padded(), just('}').padded()))
        .map(|(((locs, x), args), e)| {
            FuncDecl::new(x, locs, args, e)
        })
}

pub fn parser_program() -> impl Parser<char, Program, Error = Simple<char>> {
    func_decl().repeated()
        .then(expr())
        .map(|(decls, e)| Program::new(decls, e))
}

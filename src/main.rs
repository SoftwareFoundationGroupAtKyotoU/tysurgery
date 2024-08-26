use tysurgery::typing::*;
use tysurgery::parser::*;
use tysurgery::arch_graph::*;

use chumsky::prelude::*;

fn main() -> anyhow::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let src = std::fs::read_to_string(args[1].clone()).unwrap();

    let graph_path = args[2].clone();
    let arch_graph = ArchGraph::from_file(graph_path);
    println!("== ARCH INFO {}", "=".repeat(80));
    println!("{:?}", arch_graph);

    let program = parser_program().parse(src).unwrap();
    println!("== PROGRAM {}", "=".repeat(80));
    println!("{:?}", program);

    let (ty, commands) = infer::infer_program(&arch_graph, program)?;
    println!("== TYPE INFER {}", "=".repeat(80));
    println!("type     = {:?}", ty);
    println!("commands = {:?}", commands);

    let result_naive = type_check_naive(arch_graph.clone(), commands.clone());
    let result = type_check(arch_graph, commands);
    assert!(result_naive == result);
    println!("== TYPE CHECK {}", "=".repeat(80));
    println!("{}", if result { "SAFE" } else { "UNSAFE" });

    Ok(())
}

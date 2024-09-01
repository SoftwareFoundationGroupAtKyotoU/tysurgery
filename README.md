# Type-Based Verification of Connectivity Constraints in Lattice Surgery

## Structure of the Artifact

- `src/`: The directory containing source files of our type checker.
    - `src/ast`: The definition of expressions.
    - `src/typing`: The definition of types and th implementation of type inference and type checking algorithm.
    - `src/ds/undo_union_find.rs`: The implementation of a union find data structure which can perform undo operation.
    - `src/arch_graph.rs`: The definition of an architecture graph.
    - `src/parser.rs`: The parser for $Q_{LS}$.
    - `src/main.rs`: The entry point.
- `examples/`: The directory contains several examples of qtile programs and architecture graphs.
    - `*.qls`: The source file of $Q_{LS}$.
    - `*.txt`: The data of architecture graphs (that is, vertices and edges).

## How to Run

You can test our implementation with a docker image provided in `Dockerfile`.
The following commands allow you to enter a shell in which our type checker (`tysurgery`) is activated:

```
$ docker bulid -t tysurgery
$ docker run --it --name tysurgery tysurgery
```

Then you can see the directory `example` that contains source files and configuration files for an architecture graph to test our implementation.
For example, you can run the type checker with `cx.qls`, which is provided in the paper, with the architecture graph specified in `graph.txt` as follows:

```
$ cd examples
$ tysurgery cx.qls graph.txt
```

The expected result is:

```
== ARCH INFO ================================================================================
ArchGraph { loc_to_index: {"l3": NodeIndex(3), "l1": NodeIndex(1), "l2": NodeIndex(2), "l4": NodeIndex(4), "l0": NodeIndex(0), "l5": NodeIndex(5)}, loc_to_edge_index: {("l2", "l1"): EdgeIndex(1), ("l2", "l5"): EdgeIndex(4), ("l1", "l4"): EdgeIndex(3), ("l4", "l3"): EdgeIndex(5), ("l3", "l0"): EdgeIndex(2), ("l0", "l3"): EdgeIndex(2), ("l5", "l4"): EdgeIndex(6), ("l4", "l5"): EdgeIndex(6), ("l4", "l1"): EdgeIndex(3), ("l1", "l0"): EdgeIndex(0), ("l1", "l2"): EdgeIndex(1), ("l0", "l1"): EdgeIndex(0), ("l5", "l2"): EdgeIndex(4), ("l3", "l4"): EdgeIndex(5)}, g: Graph { Ty: "Undirected", node_count: 6, edge_count: 7, edges: (0, 1), (1, 2), (0, 3), (1, 4), (2, 5), (3, 4), (4, 5), node weights: {0: "l0", 1: "l1", 2: "l2", 3: "l3", 4: "l4", 5: "l5"} } }
== PROGRAM ================================================================================
Program { decls: [FuncDecl { fname: "cx", locs: ["lx", "ly", "lz"], args: [("q0", Qbit("lx")), ("q1", Qbit("ly"))], expr: Expr { kind: Init("aux", "lz", Expr { kind: Measure("a", ["aux", "q1"], Expr { kind: Seq(Expr { kind: If(Expr { kind: Var("a") }, Expr { kind: GateCall(Z, "q0") }, None) }, Expr { kind: Measure("b", ["q0", "aux"], Expr { kind: Seq(Expr { kind: If(Expr { kind: Var("b") }, Expr { kind: GateCall(X, "q1") }, None) }, Expr { kind: Measure("c", ["aux"], Expr { kind: Seq(Expr { kind: If(Expr { kind: Var("c") }, Expr { kind: GateCall(Z, "q0") }, None) }, Expr { kind: Free("aux") }) }) }) }) }) }) }) } }], expr: Expr { kind: Init("q0", "l0", Expr { kind: Init("q1", "l1", Expr { kind: Call("cx", ["l0", "l1", "l2"], ["q0", "q1"]) }) }) } }
== TYPE INFER ================================================================================
type     = Unit
commands = [Alloc("l0"), Alloc("l1"), Alloc("l2"), Merge("l2", "l1"), Branch([], []), Merge("l0", "l2"), Branch([], []), Branch([], []), Free("l2")]
== TYPE CHECK ================================================================================
SAFE
```

A result of our type checker contains

- `ARCH INFO`: a configuration of an architecture graph specified in the second input.
- `PROGRAM`: an AST of the given program.
- `TYPE INFER`: the result of type inference, including a type and a command sequence.
- `TYPE CHECK`: the result of connectivity checking with the command sequence obtained by type inference.

### The Format of a Graph Configuration File

A configuration file for an architecture graph with $n$ vertices and $m$ edges must have the following format:

```
<location 1> <location 2> ... <location n>
<location 11> <location 12>
...
<location m1> <location m2>
```

where

- `<location i>` is the name of i-th location variable.
- `<location i1> <location i2>` represents that the i-th edge is `{<location i1>, <location i2>}`.

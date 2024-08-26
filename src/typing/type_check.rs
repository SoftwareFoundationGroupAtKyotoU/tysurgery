use std::collections::HashSet;
use petgraph::graph::{NodeIndex, EdgeIndex};
use crate::arch_graph::ArchGraph;
use crate::ast::*;
use crate::typing::*;
use crate::ds::UndoUnionFind;

fn serialize(mut commands: CommandSequence) -> CommandSequence {
    match commands.pop_front() {
        None => CommandSequence::new(),
        Some(c) => match c {
            Command::Alloc(l) => {
                let mut cs = serialize(commands);
                cs.push_front(Command::Alloc(l.clone()));
                cs.push_back(Command::Free(l));
                cs
            },
            Command::Free(l) => {
                let mut cs = serialize(commands);
                cs.push_front(Command::Free(l.clone()));
                cs.push_back(Command::Alloc(l));
                cs
            },
            Command::Merge(l1, l2) => {
                let mut cs = serialize(commands);
                cs.push_front(Command::Merge(l1, l2));
                cs
            },
            Command::Branch(cs1, mut cs2) => {
                let mut cs1 = serialize(cs1);
                cs2.append(&mut commands);
                let mut cs2 = serialize(cs2);
                cs1.append(&mut cs2);
                cs1
            },
            Command::Loop(mut cs) => {
                cs.append(&mut commands);
                serialize(cs)
            }
        }
    }
}

type Query = (NodeIndex, NodeIndex);

/// (Offline) dynamic connectivity solver
/// |C| : the size of a command sequence
/// V : the size of vertices of the architecture graph
/// Complexity:
/// - O(|C|VlogVlog|C|) for general graphs
/// - O(|C|logVlog|C|) for sparse graphs
/// Note: Our implementation is expensive by O(log|C|) compared to the paper, but this is because
///       we choose a simpler implementation based on the segment tree and union-find tree.
///       We can achieve O(|C|logV) (offline version) by using Link-cut trees maintaining maximum
///       spanning forests.
#[derive(Debug, Clone)]
pub struct DynamicConnectivity {
    size: u32,
    g: ArchGraph,
    cs: Vec<Command>, // Use a vector for randomize access
    alive_edges: Vec<Vec<EdgeIndex>>,
    merge_query: Vec<Option<Query>>, // time -> query
}

impl DynamicConnectivity {
    pub fn new(g: ArchGraph, cs: CommandSequence) -> Self {
        let len_cs = cs.len();
        let mut size = 1;
        while size < len_cs {
            size *= 2;
        }
        Self {
            size: size as u32,
            g,
            cs: cs.into_iter().collect(),
            alive_edges: vec![Vec::new(); 2 * size],
            merge_query: vec![None; len_cs]
        }
    }

    fn add_edge_range(&mut self, live_l: u32, live_r: u32, eidx: EdgeIndex) {
        self.add_edge_range_impl(live_l, live_r, eidx, 0, 0, self.size)
    }

    fn add_edge_range_impl(&mut self, live_l: u32, live_r: u32, eidx: EdgeIndex, k: u32, l: u32, r: u32) {
        if r <= live_l || live_r <= l {
            return;
        }
        if live_l <= l && r <= live_r {
            self.alive_edges[k as usize].push(eidx);
            return;
        }
        self.add_edge_range_impl(live_l, live_r, eidx, 2 * k + 1, l, (l + r) / 2);
        self.add_edge_range_impl(live_l, live_r, eidx, 2 * k + 2, (l + r) / 2, r);
    }

    fn preprocess(&mut self) {
        let mut edge_live_ranges = vec![vec![(0, self.cs.len())]; self.g.edge_size()];
        let mut count: Vec<i32> = vec![0; self.g.edge_size()]; // if count == 0, the edge lives
        for (i, c) in self.cs.iter().enumerate() {
            match c {
                Command::Alloc(l) => {
                    for eidx in self.g.edges(l) {
                        let eidx = eidx.index();
                        if count[eidx] == 0 { // invalidated now
                            let (live_l, _) = edge_live_ranges[eidx].pop().unwrap();
                            edge_live_ranges[eidx].push((live_l, i));
                        }
                        count[eidx] -= 1;
                    }
                },
                Command::Free(l) => {
                    for eidx in self.g.edges(l) {
                        let eidx = eidx.index();
                        count[eidx] += 1;
                        if count[eidx] == 0 {
                            edge_live_ranges[eidx].push((i, self.cs.len()))
                        }
                    }
                },
                Command::Merge(l1, l2) => {
                    let u = self.g.node_index(&l1);
                    let v = self.g.node_index(&l2);
                    self.merge_query[i] = Some((u, v));
                }
                _ => {}
            }
        }
        for (eidx, ranges) in edge_live_ranges.iter().enumerate() {
            for (live_l, live_r) in ranges {
                let live_l = *live_l as u32;
                let live_r = *live_r as u32;
                self.add_edge_range(live_l, live_r, (eidx as u32).into());
            }
        }
    }

    fn process_merge_queries(&self) -> bool {
        let mut uf = UndoUnionFind::new(self.g.node_count());
        let mut can_used = vec![true; self.g.node_count()];
        self.process_merge_queries_impl(&mut uf, &mut can_used, 0)
    }

    fn process_merge_queries_impl(&self, uf: &mut UndoUnionFind, can_used: &mut Vec<bool>, k: u32) -> bool {
        for eidx in &self.alive_edges[k as usize] {
            let (u, v) = self.g.edge_endpoints(*eidx);
            uf.merge(u.index() as i32, v.index() as i32);
        }
        let mut is_safe = true;
        if k < self.size - 1 {
            is_safe &= self.process_merge_queries_impl(uf, can_used, 2 * k + 1);
            is_safe &= self.process_merge_queries_impl(uf, can_used, 2 * k + 2);
        } else if k - (self.size - 1) < self.cs.len() as u32 {
            let query_index = (k - (self.size - 1)) as usize;
            match &self.cs[query_index] {
                Command::Alloc(l) => {
                    can_used[self.g.node_index(&l).index()] = false;
                },
                Command::Free(l) => {
                    can_used[self.g.node_index(&l).index()] = true;
                },
                _ => {}
            }
            if let Some((u, v)) = self.merge_query[query_index] {
                is_safe = false;
                let mut roots = HashSet::new(); // To avoid nested loops
                for adj_of_u in self.g.adj_node_indices(u) {
                    is_safe |= adj_of_u == v; // directly connected
                    if !can_used[adj_of_u.index()] {
                        continue;
                    }
                    roots.insert(uf.root(adj_of_u.index() as i32));
                }
                for adj_of_v in self.g.adj_node_indices(v) {
                    let root = uf.root(adj_of_v.index() as i32);
                    is_safe |= can_used[adj_of_v.index()] && roots.contains(&root);
                }
            }
        }
        for _ in 0..self.alive_edges[k as usize].len() {
            uf.undo();
        }

        is_safe
    }
}


pub fn type_check(g: ArchGraph, cs: CommandSequence) -> bool {
    let cs = serialize(cs);
    let mut dycon = DynamicConnectivity::new(g, cs);
    dycon.preprocess();
    //println!("[type_check]\n{:?}", dycon);
    dycon.process_merge_queries()
}


//
// Naive version
//
fn path_find(g: &ArchGraph, used: &Vec<bool>, cur: &Location, target: &Location, visited: &mut Vec<bool>) -> bool {
    if cur == target { return true }
    visited[g.node_index(cur).index()] = true;
    for next in g.adj(cur) {
        if next == *target {
            return true;
        }
        let next_index = g.node_index(&next).index();
        if visited[next_index] || used[next_index] {
            continue;
        }
        if path_find(g, used, &next, target, visited) {
            return true
        }
    }
    return false
}

fn type_check_naive_impl(g: &ArchGraph, mut commands: CommandSequence, mut used: Vec<bool>) -> bool {
    if commands.len() == 0 { return true; }
    let com = commands.pop_front().unwrap();
    match com {
        Command::Alloc(l) => {
            let index = g.node_index(&l).index();
            if used[index] { return false; }
            used[index] = true;
            type_check_naive_impl(g, commands, used)
        },
        Command::Free(l) => {
            let index = g.node_index(&l).index();
            if !used[index] { return false; }
            used[index] = false;
            type_check_naive_impl(g, commands, used)
        },
        Command::Merge(l1, l2) => {
            if l1 != l2 && !path_find(&g, &used, &l1, &l2, &mut vec![false; g.node_count()]) {
                return false;
            }
            type_check_naive_impl(g, commands, used)
        },
        Command::Branch(cs1, mut cs2) => {
            let used_tmp = used.clone();
            cs2.append(&mut commands);
            type_check_naive_impl(g, cs1, used)
                && type_check_naive_impl(g, cs2, used_tmp)
        },
        Command::Loop(mut cs) => { 
            cs.append(&mut commands);
            type_check_naive_impl(g, cs, used)
        }
    }
}

pub fn type_check_naive(g: ArchGraph, commands: CommandSequence) -> bool {
    let used: Vec<bool> = vec![false; g.node_count()];
    type_check_naive_impl(&g, commands, used)
}

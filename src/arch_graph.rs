use std::fs::File;
use std::io::{BufReader, BufRead};
use std::collections::HashMap;
use petgraph::graph::{UnGraph, NodeIndex, EdgeIndex};

use crate::ast::Location;

#[derive(Debug, Clone)]
pub struct ArchGraph {
    loc_to_index: HashMap<Location, NodeIndex>,
    loc_to_edge_index: HashMap<(Location, Location), EdgeIndex>,
    g: UnGraph::<Location, ()>
}

impl ArchGraph {
    pub fn adj(&self, node: &Location) -> Vec<Location> {
        self.g.neighbors(*self.loc_to_index.get(node).unwrap())
            .map(|index| self.g.node_weight(index).unwrap().clone())
            .collect()
    }

    pub fn adj_node_indices(&self, index: NodeIndex) -> Vec<NodeIndex> {
        self.g.neighbors(index).collect()
    }

    pub fn nodes(&self) -> Vec<&Location> {
        self.loc_to_index.keys().collect()
    }

    pub fn node_index(&self, l: &Location) -> NodeIndex {
        *self.loc_to_index.get(l).unwrap()
    }
    
    pub fn node_count(&self) -> usize {
        self.g.node_count()
    }

    pub fn edge_size(&self) -> usize {
        self.g.edge_count()
    }

    pub fn edges(&self, node: &Location) -> Vec<EdgeIndex> {
        let adjs = self.adj(node);
        adjs.iter().map(|node2| {
            self.loc_to_edge_index.get(&(node.clone(), node2.clone())).unwrap().clone()
        }).collect()
    }

    pub fn edge_endpoints(&self, eidx: EdgeIndex) -> (NodeIndex, NodeIndex) {
        self.g.edge_endpoints(eidx).unwrap()
    }

    pub fn from_file(path: String) -> Self {
        let file = File::open(path).expect("Error: graph file not found");
        let buf_reader = BufReader::new(file);
        let mut lines = buf_reader.lines();
        //assert!(lines.count() >= 1);

        let mut loc_to_index = HashMap::new();
        let mut loc_to_edge_index = HashMap::new();
        let mut g = UnGraph::<Location, ()>::new_undirected();

        // Read nodes
        lines.by_ref().nth(0).unwrap().expect("").split_whitespace().for_each(|s| {
            let loc = Location::from(s);
            let index = g.add_node(loc.clone());
            loc_to_index.insert(loc, index);
        } );

        // Read edges
        for line in lines {
            let edge: Vec<_> = line.expect("").split_whitespace().map(|s| Location::from(s)).collect();
            let u = loc_to_index.get(&edge[0]).unwrap();
            let v = loc_to_index.get(&edge[1]).unwrap();
            let eidx = g.add_edge(*u, *v, ());
            // because undirected
            loc_to_edge_index.insert((edge[0].clone(), edge[1].clone()), eidx);
            loc_to_edge_index.insert((edge[1].clone(), edge[0].clone()), eidx);
        }

        Self {
            loc_to_index,
            loc_to_edge_index,
            g
        }
    }
}

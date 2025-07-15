use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use crate::{file_path_to_mod_path, Grouping};

pub struct Graph {
    pub edges: HashMap<(String, String), HashSet<String>>,
    mode: Grouping,
}

impl Graph {
    pub fn new(mode: Grouping) -> Self {
        Self {
            edges: HashMap::new(),
            mode,
        }
    }
    pub fn add(&mut self, from: String, to: String, why: String) {
        if from != to {
            self.edges.entry((from, to)).or_default().insert(why);
        }
    }

    fn get_edge_label(whys: &HashSet<String>) -> String {
        if whys.len() > 3 {
            let mut iter = whys.iter().take(3);
            let mut label = iter.next().unwrap().clone();
            for why in iter {
                label.push_str("\\n");
                label.push_str(why);
            }
            label.push_str("\\n…");
            label
        } else {
            whys.iter().cloned().collect::<Vec<_>>().join("\\n")
        }
    }

    fn normalize(&mut self) {
        if self.mode == Grouping::Module {
            // normalize the edges to use module paths
            let mut new_edges: HashMap<(String, String), HashSet<String>> = HashMap::new();
            for ((src, dst), whys) in self.edges.drain() {
                let src_mod = file_path_to_mod_path(&PathBuf::from(&src));
                let dst_mod = file_path_to_mod_path(&PathBuf::from(&dst));
                new_edges
                    .entry((src_mod, dst_mod))
                    .or_default()
                    .extend(whys);
            }
            self.edges = new_edges;
        }
    }

    pub fn dump_dot(&mut self) {
        self.normalize();
        // collect every vertex and bucket it by its root segment
        let mut clusters: HashMap<String, HashSet<String>> = HashMap::new();
        for (src, dst) in self.edges.keys() {
            for v in [src, dst] {
                if let Some(root) = v.split("::").next() {
                    clusters
                        .entry(root.to_string())
                        .or_default()
                        .insert(v.clone());
                }
            }
        }

        let config = r#"
 graph [rankdir=LR];
 node [shape=box, style=rounded];
 edge [fontname="Courier New", fontsize=10];
 graph  [fontname="Helvetica", fontsize=12];
 node   [shape=box,   fontname="Helvetica", fontsize=12];
 edge   [fontname="Helvetica", fontsize=8, color="\#555"];
"#;

        println!("digraph internal_deps {{");
        println!("{config}");
        println!("  compound=true;"); // allow edges into clusters
        println!("  node [shape=box];");

        // 1. emit the clusters (boxes)
        for (root, verts) in &clusters {
            println!("  subgraph cluster_{root} {{");
            println!("    label=\"{root}\";");
            println!("    style=rounded;"); // nice rounded box
            for v in verts {
                println!("    \"{v}\";");
            }
            println!("  }}");
        }

        //dbg!(&self.0);
        // 2. emit the labelled edges
        for ((src, dst), whys) in &self.edges {
            let label = Self::get_edge_label(whys);
            println!("  \"{src}\" -> \"{dst}\" [label=\"{label}\"];");
        }
        println!("}}");
    }
}

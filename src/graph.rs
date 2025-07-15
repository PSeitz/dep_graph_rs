use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use crate::{file_path_to_mod_path, Filter, Grouping};

pub struct Graph {
    pub edges: HashMap<(String, String), HashSet<String>>,
    mode: Grouping,
    filter: Filter,
}

impl Graph {
    pub fn new(mode: Grouping, filter: Filter) -> Self {
        Self {
            edges: HashMap::new(),
            mode,
            filter,
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

    fn apply_filter(&mut self) {
        let mut filtered_edges = self.edges.clone();

        for ((_src, _dst), items) in &mut filtered_edges {
            if let Some(ref item_filt) = self.filter.item {
                *items = items
                    .iter()
                    .filter(|why| item_filt.is_match(why))
                    .cloned()
                    .collect::<HashSet<_>>();
            }
        }
        self.edges = filtered_edges
            .into_iter()
            .filter(|((src, dst), items)| {
                if let Some(ref src_filt) = self.filter.source {
                    if !src_filt.is_match(src) {
                        return false;
                    }
                }
                if let Some(ref dst_filt) = self.filter.destination {
                    if !dst_filt.is_match(dst) {
                        return false;
                    }
                }
                if self.filter.item.is_some() && items.is_empty() {
                    return false; // Filter out edges with no reasons
                }
                true
            })
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
    }

    fn apply_grouping(&mut self) {
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
        self.apply_grouping();
        self.apply_filter();
        // collect every vertex and bucket it by its root segment
        let mut clusters: HashMap<String, HashSet<String>> = HashMap::new();
        for (src, dst) in self.edges.keys() {
            let seperator = if self.mode == Grouping::Module {
                "::"
            } else {
                "/"
            };
            for v in [src, dst] {
                if let Some(root) = v.split(seperator).next() {
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
        let mut clusters_sorted: Vec<_> = clusters.iter().collect();
        clusters_sorted.sort_by_key(|(root, _)| root.to_string());
        for (root, verts) in &clusters_sorted {
            println!("  subgraph \"cluster_{root}\" {{");
            println!("    label=\"{root}\";");
            println!("    style=rounded;"); // nice rounded box
            for v in *verts {
                println!("    \"{v}\";");
            }
            println!("  }}");
        }

        //dbg!(&self.0);
        // 2. emit the labelled edges
        let mut sorted_edges: Vec<_> = self.edges.iter().collect();
        sorted_edges.sort_by_key(|((src, dst), _)| (src.to_string(), dst.to_string()));
        for ((src, dst), whys) in &sorted_edges {
            let label = Self::get_edge_label(whys);
            let mut extra = String::new();
            let mut check_extra = |src: &str, arrow: &str| {
                let src_root = root_of(src).unwrap_or_else(|| src.to_string());
                if clusters.contains_key(&src_root) {
                    extra.push_str(&format!(",{arrow}=\"cluster_{src_root}\""));
                }
            };
            //check_extra(src, "ltail");
            check_extra(dst, "lhead");
            println!("  \"{src}\" -> \"{dst}\" [label=\"{label}\" {extra}];");
        }
        println!("}}");
    }
}

/// "crate::aggregation::..."  →  "aggregation"
fn root_of(s: &str) -> Option<String> {
    s.split("::").next().map(|s| s.to_string())
}

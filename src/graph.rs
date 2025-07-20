use std::{
    collections::{HashMap, HashSet},
    path::PathBuf,
};

use crate::{file_path_to_mod_path, Filter, Grouping};

pub struct Graph {
    pub edges: HashMap<Edge, HashSet<String>>,
    mode: Grouping,
    filter: Filter,
}

#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Edge {
    src: String,
    dst: String,
}
impl From<(String, String)> for Edge {
    fn from((from, to): (String, String)) -> Self {
        Self { src: from, dst: to }
    }
}
impl From<(&str, &str)> for Edge {
    fn from((from, to): (&str, &str)) -> Self {
        Self {
            src: from.to_string(),
            dst: to.to_string(),
        }
    }
}
impl std::fmt::Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.src, self.dst)
    }
}
impl std::fmt::Debug for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", self.src, self.dst)
    }
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
            self.edges.entry((from, to).into()).or_default().insert(why);
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

    pub fn apply_filter(&mut self) {
        for items in self.edges.values_mut() {
            if let Some(ref item_filt) = self.filter.item {
                items.retain(|why| {
                    item_filt.is_match(why)
                        || self
                            .filter
                            .filter
                            .clone()
                            .map(|f| f.is_match(why))
                            .unwrap_or(false)
                });
            }
        }
        self.edges.retain(|edge, items| {
            if !self.filter.is_match(&edge.src, &edge.dst) {
                return false;
            }
            if self.filter.item.is_some() && items.is_empty() {
                return false;
            }
            true
        });
    }

    pub fn apply_grouping(&mut self) {
        if self.mode == Grouping::Module {
            let mut new_edges: HashMap<Edge, HashSet<String>> = HashMap::new();
            for (edge, whys) in self.edges.drain() {
                let src_mod = file_path_to_mod_path(&PathBuf::from(&edge.src));
                let dst_mod = file_path_to_mod_path(&PathBuf::from(&edge.dst));
                new_edges
                    .entry((src_mod, dst_mod).into())
                    .or_default()
                    .extend(whys);
            }
            self.edges = new_edges;
        }
    }

    pub fn dump_dot(&mut self) {
        // build clusters by root segment
        let mut clusters: HashMap<String, HashSet<String>> = HashMap::new();
        let sep = if self.mode == Grouping::Module {
            "::"
        } else {
            "/"
        };
        for module in self
            .edges
            .keys()
            .flat_map(|e| [e.src.clone(), e.dst.clone()])
        {
            if let Some(root) = module.split(sep).next() {
                clusters
                    .entry(root.to_string())
                    .or_default()
                    .insert(module.clone());
            }
        }

        let palette: Vec<(&str, &str)> = get_palette();

        let header = r##"
 graph [rankdir=LR];
 edge [fontname="Courier New", fontsize=8, color="#555"];
 graph  [fontname="Helvetica", fontsize=12];
 node [
    shape=box
    style=filled
    fontname="Helvetica"
    fontsize=12
    fillcolor="white"
    color="#555"
 ];
 "##;

        println!("digraph internal_deps {{");
        println!("{header}");
        println!("  compound=true;");
        println!("  node [shape=box];");

        // emit beautiful colored clusters
        let mut sorted: Vec<_> = clusters.iter().collect();
        sorted.sort_by_key(|(r, _)| r.to_string());
        for (i, (root, verts)) in sorted.iter().enumerate() {
            let (border, fill) = palette[i % palette.len()];
            println!("  subgraph \"cluster_{root}\" {{");
            println!("    label=\"{root}\";");
            println!("    style=\"rounded,filled\";");
            println!("    color=\"{border}\";");
            println!("    fillcolor=\"{fill}\";");
            for v in *verts {
                println!("    \"{v}\";");
            }
            println!("  }}");
        }

        // emit edges
        let mut edges: Vec<_> = self.edges.iter().collect();
        edges.sort_by_key(|(edge, _)| (*edge).clone());
        for (edge, whys) in edges {
            let lbl = Self::get_edge_label(whys);
            let mut extra = String::new();
            if let Some(root) = root_of(&edge.dst) {
                if clusters.contains_key(&root) {
                    extra.push_str(&format!(",lhead=\"cluster_{root}\""));
                }
            }
            let src = &edge.src;
            let dst = &edge.dst;
            println!("  \"{src}\" -> \"{dst}\" [label=\"{lbl}\"{extra}];");
        }

        println!("}}");
    }
}

/// "crate::aggregation::..." → "aggregation"
/// "super::aggregation::..." → "aggregation"
fn root_of(s: &str) -> Option<String> {
    s.split("::").next().map(|s| s.to_string())
}

fn get_palette() -> Vec<(&'static str, &'static str)> {
    vec![
        ("#1f77b4", "#c6dbef"), // blue
        ("#ff7f0e", "#ffe7c6"), // orange
        ("#2ca02c", "#c6efcd"), // green
        ("#d62728", "#f4c6c6"), // red
        ("#9467bd", "#e8d6ef"), // purple
        ("#8c564b", "#e9d6ca"), // brown
        ("#e377c2", "#f7d6e9"), // pink
        ("#7f7f7f", "#d6d6d6"), // grey
        ("#bcbd22", "#ecebc6"), // olive
        ("#17becf", "#c6eef0"), // cyan
    ]
}

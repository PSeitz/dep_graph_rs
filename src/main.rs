use std::{
    collections::{HashMap, HashSet},
    env, fs,
    path::{Path, PathBuf},
    sync::{Mutex, OnceLock},
};

use anyhow::{Context, Result};
use syn::{visit::Visit, Attribute, File, ItemMod, Meta, UseTree};

#[derive(Clone, Copy)]
enum Grouping {
    File,
    Module,
}
fn read_args() -> (PathBuf, Grouping) {
    let mut root = PathBuf::from(".");
    let mut grouping = Grouping::File;
    for arg in env::args().skip(1) {
        match arg.as_str() {
            "module" | "--module" => grouping = Grouping::Module,
            "file" | "--file" => grouping = Grouping::File,
            _ if root == PathBuf::from(".") => root = PathBuf::from(arg),
            other => {
                eprintln!("unrecognised arg: {other}");
                std::process::exit(1);
            }
        }
    }
    (root, grouping)
}

#[derive(Default)]
struct Graph(HashMap<(String, String), HashSet<String>>);

impl Graph {
    fn add(&mut self, from: String, to: String, why: String) {
        if from != to {
            self.0.entry((from, to)).or_default().insert(why);
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

    fn dump_dot(&self) {
        // collect every vertex and bucket it by its root segment
        let mut clusters: HashMap<String, HashSet<String>> = HashMap::new();
        for (src, dst) in self.0.keys() {
            for v in [src, dst] {
                if let Some(root) = v.strip_prefix("crate::").and_then(|s| s.split("::").next()) {
                    clusters
                        .entry(root.to_string())
                        .or_default()
                        .insert(v.clone());
                }
            }
        }

        println!("digraph internal_deps {{");
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

        // 2. emit the labelled edges
        for ((src, dst), whys) in &self.0 {
            let label = Self::get_edge_label(whys);
            println!("  \"{src}\" -> \"{dst}\" [label=\"{label}\"];");
        }
        println!("}}");
    }
}

fn is_cfg_test(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| {
        attr.path().is_ident("cfg")
            && matches!(
                &attr.meta,
                Meta::List(list) if list.tokens.to_string().contains("test")
            )
    })
}

struct Collector<'g, 'w> {
    file_path: PathBuf,
    mod_path: Vec<String>,
    grouping: Grouping,
    root: &'g Path,
    in_test: bool,
    graph: &'g mut Graph,
    worklist: &'w mut Vec<(Vec<String>, PathBuf)>,
    module_files: &'g mut HashMap<String, PathBuf>,
}

impl<'g, 'w, 'ast> Visit<'ast> for Collector<'g, 'w> {
    fn visit_item_use(&mut self, u: &'ast syn::ItemUse) {
        if self.in_test || is_cfg_test(&u.attrs) {
            return;
        }
        if let UseTree::Path(p) = &u.tree {
            if p.ident == "crate" {
                if let Some(seg) = first_segment(&p.tree) {
                    if let Some(target_fp) = self.module_files.get(&seg) {
                        let from = match self.grouping {
                            Grouping::File => rel(&self.file_path, self.root),
                            Grouping::Module => self.mod_path.join("::"),
                        };
                        let to = match self.grouping {
                            Grouping::File => rel(target_fp, self.root),
                            Grouping::Module => format!("crate::{seg}"),
                        };
                        for item in imported_items(&p.tree) {
                            self.graph.add(from.clone(), to.clone(), item);
                        }
                    }
                }
            }
        }
        syn::visit::visit_item_use(self, u);
    }

    fn visit_item_mod(&mut self, m: &'ast ItemMod) {
        let this_is_test = self.in_test || is_cfg_test(&m.attrs);
        if this_is_test {
            return;
        }

        let mut new_path = self.mod_path.clone();
        new_path.push(m.ident.to_string());

        // inline module
        if let Some((_, items)) = &m.content {
            if self.mod_path.len() == 1 {
                self.module_files
                    .insert(m.ident.to_string(), self.file_path.clone());
            }
            let mut inner = Collector {
                file_path: self.file_path.clone(),
                mod_path: new_path,
                grouping: self.grouping,
                root: self.root,
                in_test: this_is_test,
                graph: self.graph,
                worklist: self.worklist,
                module_files: self.module_files,
            };
            for it in items {
                inner.visit_item(it);
            }
            return;
        }

        // outline module
        if let Some(fp) = find_mod_file(&self.file_path, &m.ident.to_string()) {
            if self.mod_path.len() == 1 {
                self.module_files.insert(m.ident.to_string(), fp.clone());
            }
            self.worklist.push((new_path, fp));
        }
    }
}

fn first_segment(tree: &UseTree) -> Option<String> {
    use syn::UseTree::*;
    match tree {
        Path(p) => Some(p.ident.to_string()),
        Name(n) => Some(n.ident.to_string()),
        Rename(r) => Some(r.ident.to_string()),
        Group(g) => g.items.iter().find_map(first_segment),
        Glob(_) => None,
    }
}

/// Collect the **imported item names** inside a `use crate::…` tree
fn imported_items(tree: &UseTree) -> Vec<String> {
    use syn::UseTree::*;
    match tree {
        Name(n) => vec![n.ident.to_string()],
        Rename(r) => vec![r.rename.to_string()],
        Path(p) => imported_items(&p.tree),
        Group(g) => g.items.iter().flat_map(imported_items).collect(),
        // We ignore * imports, as they don't contain specific items
        Glob(_) => vec![],
    }
}

fn rel(p: &Path, root: &Path) -> String {
    p.strip_prefix(root)
        .unwrap_or(p)
        .to_string_lossy()
        .into_owned()
}

fn find_mod_file(parent_file: &Path, ident: &str) -> Option<PathBuf> {
    let dir = parent_file.parent()?;
    let a = dir.join(format!("{ident}.rs"));
    if a.exists() {
        return Some(a);
    }
    let b = dir.join(ident).join("mod.rs");
    b.exists().then_some(b)
}

fn main() -> Result<()> {
    let (crate_root_cli, grouping) = read_args();
    let crate_root = fs::canonicalize(&crate_root_cli).unwrap_or_else(|_| crate_root_cli.clone());

    let root_rs = ["src/lib.rs", "src/main.rs"]
        .into_iter()
        .map(|p| crate_root.join(p))
        .find(|p| p.exists())
        .context("cannot find src/lib.rs or src/main.rs")?;

    let mut graph = Graph::default();
    let mut module_files = HashMap::<String, PathBuf>::new();
    let mut worklist = vec![(vec!["crate".into()], root_rs)];

    while let Some((mod_path, file_path)) = worklist.pop() {
        if visited(&file_path) {
            continue;
        }
        let code = fs::read_to_string(&file_path)?;
        let ast: File = syn::parse_str(&code)?;

        let mut v = Collector {
            file_path: file_path.clone(),
            mod_path,
            grouping,
            root: &crate_root,
            in_test: false,
            graph: &mut graph,
            worklist: &mut worklist,
            module_files: &mut module_files,
        };
        v.visit_file(&ast);
    }

    graph.dump_dot();
    Ok(())
}

/// global seen-file set
fn visited(p: &Path) -> bool {
    static SEEN: OnceLock<Mutex<HashSet<PathBuf>>> = OnceLock::new();
    let mut set = SEEN
        .get_or_init(|| Mutex::new(HashSet::new()))
        .lock()
        .unwrap();
    let canon = fs::canonicalize(p).unwrap_or_else(|_| p.to_path_buf());
    !set.insert(canon)
}

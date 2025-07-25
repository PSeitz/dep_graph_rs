use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::{arg, command, Args, Parser, ValueEnum};
use regex::Regex;
use syn::{visit::Visit, Attribute, File, ItemMacro, ItemMod, Meta, UseTree};

mod graph;
mod module;
use crate::{graph::Graph, module::Module};

#[derive(Debug, Default, Clone, Copy, ValueEnum, PartialEq, Eq)]
enum Grouping {
    File,
    #[default]
    Module,
}

/// Represents a filter for the edges in the graph.
#[derive(Args, Debug, Default)]
#[group(multiple = true, required = false)]
pub struct Filter {
    /// If set, only show edges where source, destination, or item
    /// match the filter.
    ///
    /// This is a Regex, but if it contains no regex‑special characters,
    /// it is treated as exact match and surrounded with `^…$`.
    #[arg(long, short, value_parser = parse_regex)]
    filter: Option<Regex>,

    /// If set, only show edges that match this source filter.
    /// This is a Regex, but if it contains no regex‑special characters,
    /// it is treated as exact match and surrounded with `^…$`.
    #[arg(long, short, value_parser = parse_regex)]
    source: Option<Regex>,
    /// If set, only show edges that match this destination filter.
    /// This is a Regex, but if it contains no regex‑special characters,
    /// it is treated as exact match and surrounded with `^…$`.
    #[arg(long, short, value_parser = parse_regex)]
    destination: Option<Regex>,
    /// If set, only show edges that match this item filter.
    /// e.g. a function name.
    /// This is a Regex, but if it contains no regex‑special characters,
    /// it is treated as exact match and surrounded with `^…$`.
    #[arg(long, short, value_parser = parse_regex)]
    item: Option<Regex>,
}

impl Filter {
    /// Return `true` iff this edge is allowed by the filter set.
    pub fn is_match(&self, src: &str, dst: &str) -> bool {
        if self.filter.is_none() && self.source.is_none() && self.destination.is_none() {
            return true; // No filters ⇒ everything matches.
        }
        // OR-combine the filters
        let mut matches = false;
        if let Some(any_filt) = &self.filter {
            matches |= any_filt.is_match(src) || any_filt.is_match(dst);
        }
        if let Some(source_filt) = &self.source {
            matches |= source_filt.is_match(src);
        }
        if let Some(dest_filt) = &self.destination {
            matches |= dest_filt.is_match(dst);
        }
        matches
    }
}

fn parse_regex(src: &str) -> Result<Regex, String> {
    // if the user didn’t supply any regex‑special characters,
    // treat it as a literal and anchor it as “^…$”
    let is_plain = !src.chars().any(|c| "|.*+?^$()[]{}\\".contains(c));
    let pat = if is_plain {
        format!("^{}$", regex::escape(src))
    } else {
        src.to_owned()
    };
    Regex::new(&pat).map_err(|e| e.to_string())
}

/// Top‑level CLI definition.
#[derive(Debug, Parser, Default)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to `src/lib.rs` / `src/main.rs` *or* to a single Rust source file.
    root: PathBuf,

    /// Group the output by file or module.
    #[arg(value_enum, long, default_value_t = Grouping::default(), alias = "grouping")]
    mode: Grouping,

    /// Optional edge filters.
    #[command(flatten)]
    filter: Option<Filter>,
}

/// True if the attributes contain a `#[cfg(test)]`.
fn is_cfg_test(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|attr| {
        attr.path().is_ident("cfg")
            && matches!(
                &attr.meta,
                Meta::List(list) if list.tokens.to_string().contains("test")
            )
    })
}

struct ImportCollector {
    imports: Vec<(Module, String)>,
    current_path: Vec<String>,
}

impl ImportCollector {
    fn new(prefix: &[String]) -> Self {
        Self {
            imports: Vec::new(),
            current_path: prefix.to_vec(),
        }
    }

    fn walk(&mut self, tree: &UseTree) {
        use syn::UseTree::*;
        match tree {
            Path(p) => {
                self.current_path.push(p.ident.to_string());
                self.walk(&p.tree);
                self.current_path.pop();
            }
            Name(n) => {
                let module_path = if self.current_path.is_empty() {
                    vec![n.ident.to_string()]
                } else {
                    self.current_path.clone()
                };
                self.imports
                    .push((Module::from(module_path), n.ident.to_string()));
            }
            Rename(r) => {
                let module_path = if self.current_path.is_empty() {
                    vec![r.ident.to_string()]
                } else {
                    self.current_path.clone()
                };
                self.imports
                    .push((Module::from(module_path), r.rename.to_string()));
            }
            Group(g) => {
                for it in &g.items {
                    self.walk(it);
                }
            }
            Glob(_) => {
                self.imports
                    .push((Module::from(self.current_path.clone()), "*".into()));
            }
        }
    }
}

/// Collect `(module_path, item)` pairs from a `UseTree`, prefixing each
/// module path with `prefix`.
fn collect_imports(tree: &UseTree, prefix: &[String]) -> Vec<(Module, String)> {
    let mut collector = ImportCollector::new(prefix);
    collector.walk(tree);
    collector.imports
}

/// Handle `use crate::…` imports.
fn collect_crate_imports(u: &syn::ItemUse) -> Vec<(Module, String)> {
    if let UseTree::Path(crate_path) = &u.tree {
        if crate_path.ident == "crate" {
            return collect_imports(&crate_path.tree, &[]);
        }
    }
    Vec::new()
}

/// Handle `use super::…` imports, resolving them into an absolute
/// `crate::…` path based on the current module path.
fn collect_super_imports(u: &syn::ItemUse, mod_path: &Module) -> Vec<(Module, String)> {
    use syn::UseTree::*;

    // Count the leading `super` segments.
    let mut supers = 0;
    let mut cursor = &u.tree;
    while let Path(p) = cursor {
        if p.ident != "super" {
            break;
        }
        supers += 1;
        cursor = &p.tree;
    }
    if supers == 0 {
        return Vec::new(); // not a super‑import
    }
    let mod_path_slice = mod_path.as_slice();
    if mod_path_slice.len() <= supers {
        // would walk past the crate root – ignore.
        // May happen if entry point is not the root maybe?
        return Vec::new();
    }

    // Build the prefix: everything after `crate` minus the removed `supers`.
    let prefix: Vec<String> = mod_path_slice[1..mod_path_slice.len() - supers].to_vec();
    collect_imports(cursor, &prefix)
}

struct Collector<'g> {
    file_path: PathBuf,
    mod_path: Module, // e.g. ["crate", "data", "engine"]
    root: &'g Path,
    in_test: bool,
    graph: &'g mut Graph,
    module_files: &'g mut HashMap<Module, PathBuf>,
}

impl<'g, 'ast> Visit<'ast> for Collector<'g> {
    fn visit_item_use(&mut self, u: &'ast syn::ItemUse) {
        if self.in_test || is_cfg_test(&u.attrs) {
            return;
        }

        // Collect imports from both `crate::…` and `super::…` paths.
        let mut imports = collect_crate_imports(u);
        imports.extend(collect_super_imports(u, &self.mod_path));
        if let UseTree::Path(p) = &u.tree {
            if p.ident == "crate" || p.ident == "super" {
                // already handled
            } else {
                imports.extend(collect_imports(&u.tree, &[]));
            }
        } else {
            imports.extend(collect_imports(&u.tree, &[]));
        }

        for (module, item) in imports {
            if let Some(to_filename) = self.module_files.get(&module) {
                let from = rel(&self.file_path, self.root);
                let to = rel(to_filename, self.root);
                self.graph.add(from.clone(), to.clone(), item);
            }
        }
    }

    fn visit_item_mod(&mut self, m: &'ast ItemMod) {
        let this_is_test = self.in_test || is_cfg_test(&m.attrs);
        if this_is_test {
            return;
        }

        self.mod_path.push(m.ident.to_string());

        if let Some((_, items)) = &m.content {
            // Inline module
            let key = Module::from(self.mod_path.as_slice()[1..].to_vec());
            self.module_files
                .entry(key)
                .or_insert_with(|| self.file_path.clone());

            // Visit items in the inline module
            for it in items {
                self.visit_item(it);
            }
        } else {
            // Module in another file
            let key = Module::from(self.mod_path.as_slice()[1..].to_vec());
            if let Some(filename) = find_mod_file(&self.file_path, &m.ident.to_string()) {
                self.module_files
                    .entry(key.clone())
                    .or_insert(filename.clone());
            }
        }

        self.mod_path.pop();
    }

    fn visit_item_macro(&mut self, i: &'ast ItemMacro) {
        if self.in_test || is_cfg_test(&i.attrs) {
            return;
        }

        // The macro can contain modules, so we need to parse the tokens
        // and visit the resulting AST.
        if let Ok(file) = syn::parse2::<File>(i.mac.tokens.clone()) {
            for item in file.items {
                self.visit_item(&item);
            }
        }
    }
}

fn rel(p: &Path, root: &Path) -> String {
    p.strip_prefix(root)
        .unwrap_or(p)
        .to_string_lossy()
        .into_owned()
}

pub fn file_path_to_mod_path(file_path: &Path) -> Module {
    let file_path = file_path.to_string_lossy();
    let file_path = if let Some(pos) = file_path.rfind("/mod.rs") {
        file_path[..pos].to_string()
    } else {
        file_path.to_string()
    };

    let path_str = file_path.replace(".rs", "");
    Module::from(
        path_str
            .split('/')
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
            .collect::<Vec<_>>(),
    )
}

fn find_mod_file(parent_file: &Path, ident: &str) -> Option<PathBuf> {
    let dir = parent_file.parent()?;
    let candidate_a = dir.join(format!("{ident}.rs"));
    if candidate_a.exists() {
        return Some(candidate_a);
    }
    let candidate_b = dir.join(ident).join("mod.rs");
    candidate_b.exists().then_some(candidate_b)
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let mut graph = scan(cli)?;
    graph.dump_dot();

    Ok(())
}

fn scan(cli: Cli) -> Result<Graph> {
    let mut crate_root = fs::canonicalize(&cli.root).unwrap_or_else(|_| cli.root.clone());

    let root_rs = if crate_root.is_dir() {
        let root_rs = ["src/lib.rs", "src/main.rs"]
            .into_iter()
            .map(|p| crate_root.join(p))
            .find(|p| p.exists())
            .context("cannot find src/lib.rs or src/main.rs. You can also point to a file directly as starting point.")?;
        crate_root = crate_root.join("src");
        root_rs
    } else {
        crate_root.clone()
    };
    let mut graph = Graph::new(cli.mode, cli.filter.unwrap_or_default());

    let mut module_files = HashMap::<Module, PathBuf>::new();
    module_files.insert(Module::default(), root_rs.clone());

    let mut visited_files = HashSet::new();
    loop {
        let modules_to_scan: Vec<(Module, PathBuf)> = module_files
            .iter()
            .map(|(m, p)| (m.clone(), fs::canonicalize(p).unwrap_or_else(|_| p.clone())))
            .filter(|(_, path)| !visited_files.contains(path))
            .collect();

        if modules_to_scan.is_empty() {
            break;
        }

        for (mod_key, file_path) in modules_to_scan {
            visited_files.insert(file_path.clone());

            let mod_path = {
                let mut p = mod_key.clone();
                p.prepend("crate".to_string());
                p
            };

            let code = fs::read_to_string(&file_path)?;
            let ast: File = syn::parse_str(&code)?;

            let mut v = Collector {
                file_path: file_path.clone(),
                mod_path,
                root: &crate_root,
                in_test: false,
                graph: &mut graph,
                module_files: &mut module_files,
            };
            v.visit_file(&ast);
        }
    }

    // Add edges from "crate" to all top-level modules.
    for mod_name in module_files.keys() {
        if !mod_name.to_string().is_empty() && !mod_name.to_string().contains("::") {
            graph.add("crate".to_string(), mod_name.to_string(), "".to_string());
        }
    }

    graph.apply_grouping();
    graph.apply_filter();

    Ok(graph)
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;
    use std::collections::HashMap;
    use syn::{File, ItemUse};

    fn graph_from_str(code: &str, module_files: &mut HashMap<Module, PathBuf>) -> Graph {
        let root = Path::new(".");
        let file_path = PathBuf::from("lib.rs");
        let mut graph = Graph::new(Grouping::File, Filter::default());

        let mut collector = Collector {
            file_path: file_path.clone(),
            mod_path: Module::from(vec!["crate".to_string()]),
            root,
            in_test: false,
            graph: &mut graph,
            module_files,
        };

        let ast: File = syn::parse_str(code).expect("failed to parse code");
        collector.visit_file(&ast);
        graph
    }

    fn check_edge(graph: &Graph, from: &str, to: &str) {
        check_edge_with_item(graph, from, to, None);
    }

    fn check_edge_with_item(graph: &Graph, from: &str, to: &str, item: Option<&str>) {
        let edge = graph.edges.get(&(from.to_string(), to.to_string()).into());
        let mut edges = graph
            .edges
            .keys()
            .map(|edge| edge.to_string())
            .collect::<Vec<_>>();
        edges.sort();

        assert!(
            edge.is_some(),
            "expected edge '{from}'->'{to}' not found in edge set:\n{}",
            edges.join("\n")
        );
        if let Some(item) = item {
            assert!(
                edge.unwrap().contains(item),
                "expected item '{item}' in edge from '{from}' to '{to}'",
            );
        }
    }

    #[test]
    fn test_project() {
        // scan `test_project` folder
        let root = PathBuf::from("test_proj1");
        let cli = Cli {
            root,
            ..Default::default()
        };
        let graph = scan(cli).expect("failed to scan project");
        assert!(!graph.edges.is_empty(), "Graph should not be empty");
        // Check that the graph contains expected edges
        check_edge(&graph, "mod1", "mod3");
    }

    #[test]
    fn test_import_edges() -> Result<()> {
        let code = r#"
            use crate::{mod2::mod2_add, mod3::mod3_add};

            pub fn mod1_add(left: u64, right: u64) -> u64 {
                mod2_add(left, right) + mod3_add(left, right)
            }
        "#;

        let mut module_files = HashMap::new();
        module_files.insert(
            Module::from(vec!["mod2".to_owned()]),
            PathBuf::from("mod2.rs"),
        );
        module_files.insert(
            Module::from(vec!["mod3".to_owned()]),
            PathBuf::from("mod3.rs"),
        );

        let graph = graph_from_str(code, &mut module_files);

        let mut expected = HashMap::new();
        expected.insert(
            ("lib.rs", "mod2.rs").into(),
            vec!["mod2_add".into()].into_iter().collect(),
        );
        expected.insert(
            ("lib.rs", "mod3.rs").into(),
            vec!["mod3_add".into()].into_iter().collect(),
        );

        assert_eq!(graph.edges, expected);
        Ok(())
    }

    #[test]
    fn test_collect_crate_imports_simple_path() {
        let tree: ItemUse = syn::parse_str("use crate::mod2::foo;").unwrap();
        let got = collect_crate_imports(&tree);
        let want = vec![(Module::from(vec!["mod2".to_string()]), "foo".to_string())];
        assert_eq!(got, want);
    }

    #[test]
    fn test_collect_crate_imports_grouped_same_mod() {
        let tree: ItemUse = syn::parse_str("use crate::mod2::{a,b,c};").unwrap();
        let mut got = collect_crate_imports(&tree);
        got.sort();
        let mut want = vec![
            (Module::from(vec!["mod2".to_string()]), "a".to_string()),
            (Module::from(vec!["mod2".to_string()]), "b".to_string()),
            (Module::from(vec!["mod2".to_string()]), "c".to_string()),
        ];
        want.sort();
        assert_eq!(got, want);
    }

    #[test]
    fn test_collect_crate_imports_multi_group() {
        let tree: ItemUse = syn::parse_str("use crate::{m1::x, m2::y};").unwrap();
        let mut got = collect_crate_imports(&tree);
        got.sort();
        let mut want = vec![
            (Module::from(vec!["m1".to_string()]), "x".to_string()),
            (Module::from(vec!["m2".to_string()]), "y".to_string()),
        ];
        want.sort();
        assert_eq!(got, want);
    }

    #[test]
    fn test_crate_root() {
        // scan `test_project` folder
        let root = PathBuf::from("test_proj1");
        let cli = Cli {
            root,
            ..Default::default()
        };
        let graph = scan(cli).expect("failed to scan project");
        assert!(!graph.edges.is_empty(), "Graph should not be empty");
        // Check that the graph contains expected edges
        check_edge(&graph, "crate", "mod1");
        check_edge(&graph, "crate", "mod2");
        check_edge(&graph, "crate", "mod3");
        check_edge(&graph, "crate", "graphics");
        check_edge(&graph, "graphics::plattform::ps1", "graphics::plattform");
        //check_edge(&graph, "graphics", "graphics::plattform");
    }

    #[test]
    /// From tokio
    fn test_macro_wrapped_module() {
        let code = r#"
            macro_rules! cfg_net {
                ($($item:item)*) => {
                    $(
                        #[cfg(feature = "net")]
                        $item
                    )*
                }
            }

            cfg_net! {
                mod lookup_host;
                use lookup_host::do_lookup;

                mod tcp;
                use tcp::connect;
            }
        "#;

        let mut module_files = HashMap::new();
        module_files.insert(
            Module::from(vec!["lookup_host".to_owned()]),
            PathBuf::from("lookup_host.rs"),
        );
        module_files.insert(
            Module::from(vec!["tcp".to_owned()]),
            PathBuf::from("tcp.rs"),
        );

        let graph = graph_from_str(code, &mut module_files);

        check_edge_with_item(&graph, "lib.rs", "lookup_host.rs", Some("do_lookup"));
        check_edge_with_item(&graph, "lib.rs", "tcp.rs", Some("connect"));
    }
}

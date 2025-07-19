use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::{arg, command, Args, Parser, ValueEnum};
use regex::Regex;
use syn::{visit::Visit, Attribute, File, ItemMod, Meta, UseTree};

use crate::graph::Graph;

mod graph;

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
    /// Check if the edge matches this filter.
    pub fn is_match(&self, src: &str, dst: &str) -> bool {
        // Special case: if no filter is set, everything matches
        if self.filter.is_none()
            && self.source.is_none()
            && self.destination.is_none()
            && self.item.is_none()
        {
            return true;
        }
        // OR-combine the filters
        let mut matches = false;
        if let Some(ref filter) = self.filter {
            matches |= filter.is_match(src) || filter.is_match(dst);
        }
        if let Some(ref src_filter) = self.source {
            matches |= src_filter.is_match(src);
        }
        if let Some(ref dst_filter) = self.destination {
            matches |= dst_filter.is_match(dst);
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

/// Command-line interface
///
#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Either a path to a crate root (`src/lib.rs` or `src/main.rs`) or a path to a file.
    root: PathBuf,

    #[arg(value_enum, long, default_value_t = Grouping::default(), alias = "grouping")]
    /// Group the output by file or by module.
    mode: Grouping,

    /// If set, only show edges that match this source filter.
    /// This is a substring match.
    #[command(flatten)]
    filter: Option<Filter>,
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
    root: &'g Path,
    in_test: bool,
    graph: &'g mut Graph,
    files_to_scan: &'w mut Vec<(Vec<String>, PathBuf)>,
    module_files: &'g mut HashMap<String, PathBuf>,
}

impl<'g, 'w, 'ast> Visit<'ast> for Collector<'g, 'w> {
    fn visit_item_use(&mut self, u: &'ast syn::ItemUse) {
        if self.in_test || is_cfg_test(&u.attrs) {
            return;
        }
        // entry: must be `Path(ident="crate", subtree)`
        let imports = collect_crate_imports(u);
        for (seg, item) in imports {
            if let Some(to_filename) = self.module_files.get(&seg) {
                let from = rel(&self.file_path, self.root);
                let to = rel(to_filename, self.root);
                self.graph.add(from.clone(), to.clone(), item);
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

        // inline module is a module that has no `mod.rs` file, but
        // instead has its content in the same file
        if let Some((_, items)) = &m.content {
            if self.mod_path.len() == 1 {
                self.module_files
                    .insert(m.ident.to_string(), self.file_path.clone());
            }
            let mut inner = Collector {
                file_path: self.file_path.clone(),
                mod_path: new_path,
                root: self.root,
                in_test: this_is_test,
                graph: self.graph,
                files_to_scan: self.files_to_scan,
                module_files: self.module_files,
            };
            for it in items {
                inner.visit_item(it);
            }
            return;
        }

        // a module that has its content in a separate file
        if let Some(filename) = find_mod_file(&self.file_path, &m.ident.to_string()) {
            if self.mod_path.len() == 1 {
                self.module_files
                    .insert(m.ident.to_string(), filename.clone());
            }
            self.files_to_scan.push((new_path, filename));
        }
    }
}

fn collect_crate_imports(u: &syn::ItemUse) -> Vec<(String, String)> {
    if let syn::UseTree::Path(crate_path) = &u.tree {
        if crate_path.ident == "crate" {
            return collect_after_crate(&crate_path.tree);
        }
    }
    Vec::new()
}

/// Given `use crate::…`, pull out exactly the (module, item) pairs:
/// - `use crate::mod2::foo;`          → [("mod2","foo")]
/// - `use crate::mod2::{a,b,c};`      → [("mod2","a"),("mod2","b"),("mod2","c")]
/// - `use crate::{m1::x, m2::y};`      → [("m1","x"),("m2","y")]
fn collect_after_crate(tree: &UseTree) -> Vec<(String, String)> {
    use syn::UseTree::*;
    match tree {
        // e.g. `crate::mod2::foo` or `crate::mod2::{a,b}`
        Path(p) => {
            let module = p.ident.to_string();
            imported_items(&p.tree)
                .into_iter()
                .map(move |item| (module.clone(), item))
                .collect()
        }

        // e.g. `crate::{m1::x, m2::y}`
        Group(g) => g.items.iter().flat_map(collect_after_crate).collect(),

        // e.g. `crate::foo`
        Name(n) => {
            vec![(n.ident.to_string(), n.ident.to_string())]
        }

        // e.g. `crate::foo as bar`
        Rename(r) => {
            let module = r.ident.to_string();
            let alias = r.rename.to_string();
            vec![(module, alias)]
        }

        // skip globs
        Glob(_) => Vec::new(),
    }
}

/// Collect the **imported item names** inside a `use crate::…` tree
fn imported_items(tree: &UseTree) -> Vec<String> {
    use syn::UseTree::*;
    match tree {
        Path(p) => imported_items(&p.tree),
        Name(n) => vec![n.ident.to_string()],
        Rename(r) => vec![r.rename.to_string()],
        Group(g) => g.items.iter().flat_map(imported_items).collect(),
        Glob(_) => vec!["*".to_string()],
    }
}

fn rel(p: &Path, root: &Path) -> String {
    p.strip_prefix(root)
        .unwrap_or(p)
        .to_string_lossy()
        .into_owned()
}

pub fn file_path_to_mod_path(file_path: &Path) -> String {
    let file_path = file_path.to_string_lossy();
    let file_path = if let Some(pos) = file_path.rfind("/mod.rs") {
        file_path[..pos].to_string()
    } else {
        file_path.to_string()
    };

    file_path.replace('/', "::").replace(".rs", "")
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
    let cli = Cli::parse();
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

    let mut module_files = HashMap::<String, PathBuf>::new();
    let mut files_to_scan = vec![(vec!["crate".into()], root_rs)];

    let mut visited_files = HashSet::new();
    while let Some((mod_path, file_path)) = files_to_scan.pop() {
        if visited(&file_path, &mut visited_files) {
            continue;
        }
        let code = fs::read_to_string(&file_path)?;
        let ast: File = syn::parse_str(&code)?;

        let mut v = Collector {
            file_path: file_path.clone(),
            mod_path,
            root: &crate_root,
            in_test: false,
            graph: &mut graph,
            files_to_scan: &mut files_to_scan,
            module_files: &mut module_files,
        };
        v.visit_file(&ast);
    }

    graph.dump_dot();
    Ok(())
}

/// global seen-file set
fn visited(p: &Path, visited: &mut HashSet<PathBuf>) -> bool {
    let canon = fs::canonicalize(p).unwrap_or_else(|_| p.to_path_buf());
    !visited.insert(canon)
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };
    use syn::{File, ItemUse};

    /// Parse `code`, run your Collector exactly once, and return the resulting Graph.
    fn graph_from_str(code: &str, module_files: &mut HashMap<String, PathBuf>) -> Graph {
        let root = Path::new(".");
        let file_path = PathBuf::from("lib.rs");
        let mut graph = Graph::new(Grouping::File, Filter::default());
        let mut files_to_scan = Vec::new();

        let mut collector = Collector {
            file_path: file_path.clone(),
            mod_path: vec!["crate".to_string()],
            root,
            in_test: false,
            graph: &mut graph,
            files_to_scan: &mut files_to_scan,
            module_files,
        };

        let ast: File = syn::parse_str(code).expect("failed to parse code");
        collector.visit_file(&ast);
        graph
    }

    #[test]
    fn test_import_edges() -> Result<()> {
        let code = r#"
            use crate::{mod2::mod2_add, mod3::mod3_add};

            pub fn mod1_add(left: u64, right: u64) -> u64 {
                mod2_add(left, right) + mod3_add(left, right)
            }
        "#;

        // seed your fake files so `use crate::mod2` → "mod2.rs", etc.
        let mut module_files = HashMap::new();
        module_files.insert("mod2".to_owned(), PathBuf::from("mod2.rs"));
        module_files.insert("mod3".to_owned(), PathBuf::from("mod3.rs"));

        let graph = graph_from_str(code, &mut module_files);

        // build the expected edges map
        let mut expected = HashMap::new();
        expected.insert(
            ("lib.rs".into(), "mod2.rs".into()),
            vec!["mod2_add".into()].into_iter().collect(),
        );
        expected.insert(
            ("lib.rs".into(), "mod3.rs".into()),
            vec!["mod3_add".into()].into_iter().collect(),
        );

        assert_eq!(graph.edges, expected);
        Ok(())
    }

    #[test]
    fn test_collect_crate_imports_simple_path() {
        // `use crate::mod2::foo;` → [("mod2","foo")]
        let tree: ItemUse = syn::parse_str("use crate::mod2::foo;").unwrap();
        let got = collect_crate_imports(&tree);
        let want = vec![("mod2".to_string(), "foo".to_string())];
        assert_eq!(got, want);
    }

    #[test]
    fn test_collect_crate_imports_grouped_same_mod() {
        // `use crate::mod2::{a,b,c};` → [("mod2","a"),("mod2","b"),("mod2","c")]
        let tree: ItemUse = syn::parse_str("use crate::mod2::{a,b,c};").unwrap();
        let mut got = collect_crate_imports(&tree);
        got.sort();
        let mut want = vec![
            ("mod2".to_string(), "a".to_string()),
            ("mod2".to_string(), "b".to_string()),
            ("mod2".to_string(), "c".to_string()),
        ];
        want.sort();
        assert_eq!(got, want);
    }

    #[test]
    fn test_collect_crate_imports_multi_group() {
        // `use crate::{m1::x, m2::y};` → [("m1","x"),("m2","y")]
        let tree: ItemUse = syn::parse_str("use crate::{m1::x, m2::y};").unwrap();
        let mut got = collect_crate_imports(&tree);
        got.sort();
        let mut want = vec![
            ("m1".to_string(), "x".to_string()),
            ("m2".to_string(), "y".to_string()),
        ];
        want.sort();
        assert_eq!(got, want);
    }
}

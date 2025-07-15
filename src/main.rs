use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::{arg, command, Parser, ValueEnum};
use syn::{visit::Visit, Attribute, File, ItemMod, Meta, UseTree};

use crate::graph::Graph;

mod graph;

#[derive(Debug, Default, Clone, Copy, ValueEnum, PartialEq, Eq)]
enum Grouping {
    File,
    #[default]
    Module,
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
        if let UseTree::Path(p) = &u.tree {
            if p.ident == "crate" {
                if let Some(seg) = first_segment(&p.tree) {
                    if let Some(to_filename) = self.module_files.get(&seg) {
                        let from = rel(&self.file_path, self.root);
                        let to = rel(to_filename, self.root);
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
            .context("cannot find src/lib.rs or src/main.rs")?;
        crate_root = crate_root.join("src");
        root_rs
    } else {
        crate_root.clone()
    };

    let mut graph = Graph::new(cli.mode);
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

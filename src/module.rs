#[derive(Clone, PartialEq, Eq, Hash, Ord, PartialOrd, Debug, Default)]
/// Represents a module path in the Rust code.
/// This is a sequence of segments, e.g. `["crate", "data", "engine"]`.
pub struct Module {
    path: Vec<String>,
}

impl Module {
    pub fn push(&mut self, segment: String) {
        self.path.push(segment);
    }

    pub fn prepend(&mut self, segment: String) {
        self.path.insert(0, segment);
    }

    pub fn pop(&mut self) {
        self.path.pop();
    }

    pub fn as_slice(&self) -> &[String] {
        &self.path
    }
}

impl From<Vec<String>> for Module {
    fn from(path: Vec<String>) -> Self {
        Self { path }
    }
}

impl From<&[String]> for Module {
    fn from(path: &[String]) -> Self {
        Self {
            path: path.to_vec(),
        }
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path.join("::"))
    }
}

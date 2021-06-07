use serde_derive::Deserialize;
use std::fmt::Debug;
use std::fmt;

/// Options for building entire project, or a single module
#[derive(Deserialize, Debug)]
pub struct CompilerOpts {
    /// If not specified, defaults to empty
    include_paths: Option<Vec<String>>,
}

impl Default for CompilerOpts {
    fn default() -> Self {
        Self {
            include_paths: None,
        }
    }
}

// impl Debug for CompilerOpts {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "CompilerOpts({:?})", self.include_paths)
//     }
// }

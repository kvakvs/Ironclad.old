use serde_derive::Deserialize;
use std::fmt;
use std::fmt::Debug;
use std::path::Path;

/// Contains source tree from the project file, or detected from the file system
#[derive(Deserialize)]
pub struct SourceTree {
    pub files: Vec<Box<Path>>,
    pub directories: Vec<SourceTree>,
}

impl SourceTree {
    // /// Given list of names check if some of them are directories, build a tree recursively.
    // pub fn from_list<I>(names: I) -> SourceTree
    //     where I: IntoIterator<Item=Path> {
    //     Self {
    //         files: vec![],
    //         directories: vec![],
    //     }
    // }
}

impl Default for SourceTree {
    fn default() -> Self {
        Self {
            files: vec![],
            directories: vec![],
        }
    }
}

impl Debug for SourceTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SourceTree(files={:?}, directories={:?})", self.files, self.directories)
    }
}

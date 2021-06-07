use serde_derive::Deserialize;
use std::fmt;
use std::fmt::Debug;
use std::path::Path;

/// Contains source tree from the project file, or detected from the file system
#[derive(Deserialize, Debug)]
pub struct InputOpts {
    /// If not specified, defaults to *.erl
    pub files: Option<Vec<Box<Path>>>,

    /// If not specified, defaults to "."
    pub directories: Option<Vec<Box<Path>>>,

    /// If not specified, defaults to empty
    pub exclude_files: Option<Vec<Box<Path>>>,

    /// If not specified, defaults to empty
    pub exclude_directories: Option<Vec<Box<Path>>>,
}

impl InputOpts {
    // /// Given list of names check if some of them are directories, build a tree recursively.
    // pub fn from_list<I>(names: I) -> SourceTree
    //     where I: IntoIterator<Item=Path> {
    //     Self {
    //         files: vec![],
    //         directories: vec![],
    //     }
    // }
}

impl Default for InputOpts {
    fn default() -> Self {
        Self {
            files: None,
            directories: None,
            exclude_files: None,
            exclude_directories: None,
        }
    }
}

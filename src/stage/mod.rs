use std::collections::HashMap;
use std::path::PathBuf;

use crate::erl_error::ErlResult;
use crate::project::ErlProject;

/// This package contains various compile stages
/// A stage takes project, and some input, and maybe some context data like defined macros.
/// A stage outputs something usable by the following stage.

pub mod file_contents_cache;
pub mod ast_cache;
pub mod compile_module;

pub mod preload;
pub mod parse;
pub mod preprocess;

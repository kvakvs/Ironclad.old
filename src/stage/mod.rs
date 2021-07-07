/// This package contains various compile stages
/// A stage takes project, and some input, and maybe some context data like defined macros.
/// A stage outputs something usable by the following stage.

pub mod file_contents_cache;
pub mod ast_cache;
pub mod compile_module;

pub mod s0_preload;
pub mod s2_parse;
pub mod s1_preprocess;

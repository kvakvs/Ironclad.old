//! This package contains various compile stages
//! A stage takes project, and some input, and maybe some context data like defined macros.
//! A stage outputs something usable by the following stage.

pub mod stage_parse;
pub mod stage_preload;
pub mod stage_preprocess;

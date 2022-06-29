//! Erlang project data structures; Group together input files, modules, compiler settings...
use crate::project::project_impl::ErlProjectImpl;
use std::sync::Arc;

pub mod compiler_opts;
pub mod conf;
pub mod input_opts;
pub mod module;
pub mod project_impl;
pub mod project_include;
pub mod project_inputs;

/// Wrapper for shared access
pub type ErlProject = Arc<ErlProjectImpl>;

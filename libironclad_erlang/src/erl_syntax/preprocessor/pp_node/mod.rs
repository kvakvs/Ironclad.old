//! Preprocessor subnode, stored in `ErlAst::Preprocessor()` enum variant

use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;

use std::sync::Arc;

pub mod pp_fmt;
pub mod pp_impl;
pub mod pp_new;
pub mod pp_type;

/// Alias for `Arc<>`
pub type PreprocessorNode = Arc<PreprocessorNodeImpl>;

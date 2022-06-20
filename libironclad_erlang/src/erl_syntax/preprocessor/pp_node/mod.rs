//! Preprocessor subnode, stored in `ErlAst::Preprocessor()` enum variant

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::token_stream::token::Token;
use std::fmt::Formatter;
use std::path::PathBuf;
use std::sync::Arc;

pub mod pp_fmt;
pub mod pp_impl;
pub mod pp_new;
pub mod pp_type;

pub type PreprocessorNode = Arc<PreprocessorNodeImpl>;

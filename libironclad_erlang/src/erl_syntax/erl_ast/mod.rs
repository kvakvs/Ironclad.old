//! AST syntax structure of an Erlang file
use crate::erl_syntax::erl_ast::node_impl::AstNodeImpl;
use std::sync::Arc;

pub mod ast_as;
pub mod ast_expr;
pub mod ast_extract_var;
pub mod ast_is;
pub mod ast_iter;
pub mod ast_new;
pub mod ast_print;
pub mod node_impl;

/// An atomic-refcounted readonly AST node, suitable for sharing and cloning
pub type AstNode = Arc<AstNodeImpl>;

// / A tree of Erlang nodes with attached file name, and root element removed
// pub type ErlAstTree = AstTree<ErlAst>;

// /// A cache of trees of Erlang nodes, keyed by filename or module name
// pub type ErlAstCache = AstCache<ErlAst>;
//
// impl Default for ErlAst {
//   fn default() -> Self {
//     ErlAst::Empty
//   }
// }

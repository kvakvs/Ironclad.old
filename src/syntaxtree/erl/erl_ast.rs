use crate::syntaxtree::ast_cache::{AstCache, AstTree};
use crate::syntaxtree::erl::erl_expr::ErlExpr;

pub enum ErlAst {
  /// Forms list
  Forms(Vec<ErlAst>),

  /// Generic module attribute -"string"(value, ...).
  ModuleAttr(String, Vec<String>),
  Atom(String),
  FunArity(String, usize),
  TypeArity(String, usize),
  Variable(String),
  TypeDef(String),
  Expr(ErlExpr),
  String(String),
}

/// A tree of Erlang nodes with attached file name, and root element removed
pub(crate) type ErlAstTree = AstTree<ErlAst>;

/// A cache of trees of Erlang nodes, keyed by filename or module name
pub(crate) type ErlAstCache = AstCache<ErlAst>;

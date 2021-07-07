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

pub(crate) type ErlAstTree = AstTree<ErlAst>;
pub(crate) type ErlAstCache = AstCache<ErlAst>;

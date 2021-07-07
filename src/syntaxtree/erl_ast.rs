use crate::syntaxtree::ast_cache::{AstCache, AstTree};

pub enum ErlAst {
  /// Generic module attribute -"string"(value, ...).
  ModuleAttr(String, Vec<String>),
  Atom(String),
  FunArity(String, usize),
  TypeArity(String, usize),
  Variable(String),
  TypeDef(String),
}

pub(crate) type ErlAstTree = AstTree<ErlAst>;
pub(crate) type ErlAstCache = AstCache<ErlAst>;

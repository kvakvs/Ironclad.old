use crate::syntaxtree::ast_cache::{AstCache, AstTree};
use crate::syntaxtree::erl::erl_expr::ErlExpr;
use crate::syntaxtree::erl::literal::ErlLiteral;
use crate::typing::erl_type::ErlType;

pub enum ErlAst {
  /// Forms list, root of a module
  Forms(Vec<ErlAst>),

  /// Generic module attribute -"string"(value, ...).
  ModuleAttr { name: String, args: Vec<String> },

  Lit(ErlLiteral),
  Variable { name: String, ty: ErlType },

  /// Any expression, where its type initially is Any
  Expr { ty: ErlType, expr: ErlExpr },

  /// A function has args, of some AST nodes, bindable expressions, and return type, initially Any
  Function { name: String, args: Vec<ErlAst>, ret: ErlType },
}

/// A tree of Erlang nodes with attached file name, and root element removed
pub(crate) type ErlAstTree = AstTree<ErlAst>;

/// A cache of trees of Erlang nodes, keyed by filename or module name
pub(crate) type ErlAstCache = AstCache<ErlAst>;

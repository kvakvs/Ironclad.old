use crate::syntaxtree::ast_cache::{AstCache, AstTree};
use crate::syntaxtree::erl::erl_expr::ErlExpr;
use crate::syntaxtree::erl::fun_clause::FunctionClause;
use crate::syntaxtree::erl::literal::ErlLiteral;
use crate::typing::typevar::TypeVar;

#[derive(Debug, PartialEq)]
pub enum ErlAst {
  /// Forms list, root of a module
  Forms(Vec<ErlAst>),

  /// Generic module attribute -"string"(value, ...).
  ModuleAttr { name: String, args: Vec<String> },

  Lit { value: ErlLiteral, tv: TypeVar },
  Variable { name: String, tv: TypeVar },

  /// Any expression, where its type initially is Any
  Expr { expr: ErlExpr, tv: TypeVar },

  /// A function has args, of some AST nodes, bindable expressions, and return type, initially Any
  Function {
    name: String,
    // Each clause is ErlExpr, and union of clause types will be function return type
    ret: TypeVar,
    clauses: Vec<FunctionClause>,
  },

  /// An automatically detected "string" value in the code
  String(String),
}

impl ErlAst {
  pub fn new_fun(name: &str, clauses: Vec<FunctionClause>) -> Self {
    ErlAst::Function {
      name: name.to_string(),
      clauses,
      ret: TypeVar::new(),
    }
  }

  pub fn new_var(name: &str) -> Self {
    ErlAst::Variable {
      name: name.to_string(),
      tv: TypeVar::new(),
    }
  }

  pub fn new_expr(expr: ErlExpr) -> Self {
    ErlAst::Expr {
      expr,
      tv: TypeVar::new(),
    }
  }
}

/// A tree of Erlang nodes with attached file name, and root element removed
pub(crate) type ErlAstTree = AstTree<ErlAst>;

/// A cache of trees of Erlang nodes, keyed by filename or module name
pub(crate) type ErlAstCache = AstCache<ErlAst>;

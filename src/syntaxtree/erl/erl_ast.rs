use crate::syntaxtree::ast_cache::{AstCache, AstTree};
use crate::syntaxtree::erl::fun_clause::FunctionClause;
use crate::syntaxtree::erl::literal::ErlLiteral;
use crate::typing::typevar::TypeVar;
use crate::typing::erl_type::ErlType;
use crate::syntaxtree::erl::case_clause::CaseClause;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};

#[derive(Debug, PartialEq)]
pub enum ErlAst {
  /// Forms list, root of a module
  Forms(Vec<ErlAst>),

  /// Generic module attribute -"string"(value, ...).
  ModuleAttr { name: String, args: Vec<String> },

  // Lit { value: ErlLiteral, ty: ErlType },
  // Variable { name: String, tv: TypeVar }, // variable is part of ErlExpr enum

  // /// Any expression, where its type initially is Any
  // Expr { expr: ErlExpr, ty: ErlType },

  /// Defines a new function.
  /// A function has clauses.
  /// Each clause has same quantity of args (some AST nodes), bindable expressions,
  /// and a return type, initially Any
  NewFunction {
    name: String,
    // Each clause is ErlExpr, and union of clause types will be function return type
    ret: ErlType,
    clauses: Vec<FunctionClause>,
  },

  /// A named variable
  Var {
    name: String,
    ty: ErlType,
  },

  /// Apply arguments to expression
  App {
    /// Target, to be called, expected to have function or lambda type
    expr: Box<ErlExpr>,
    /// Arguments. Their  inferred types are stored inside.
    args: Vec<ErlExpr>,
    /// Return inferred type.
    ty: ErlType,
  },

  // /// A lambda definition or a function
  // Function { args: Vec<ErlExpr>, expr: Box<ErlExpr> },

  /// A haskell-style new variable introducing a new scope below it:
  /// let x = expr1 in expr2
  Let {
    var: String,
    /// Type which we believe is Var
    var_ty: ErlType,
    /// Value (type is in it)
    value: Box<ErlExpr>,
    /// Let x=y in <body> (type is in it, and becomes type of Expr::Let)
    in_expr: Box<ErlExpr>,
  },

  // // TODO: Remove If because can be replaced with Case
  // If {
  //   cond: Box<ErlExpr>,
  //   on_true: Box<ErlExpr>,
  //   on_false: Box<ErlExpr>,
  // },
  Case {
    /// A union type of all case clauses
    ty: ErlType,
    arg: Box<ErlExpr>,
    clauses: Vec<CaseClause>,
  },

  /// A literal value, constant. Type is known via literal.get_type()
  Lit(ErlLiteral),

  BinaryOp { left: Box<ErlExpr>, right: Box<ErlExpr>, op: ErlBinaryOp },
  UnaryOp { expr: Box<ErlExpr>, op: ErlUnaryOp },
}

impl ErlAst {
  pub fn get_children(&self) -> Vec<&ErlAst> {
    match self {
      ErlAst::Forms(f) => f.iter(),
      ErlAst::ModuleAttr { .. } => unreachable!("Ast::ModuleAttr has no nested nodes"),
      ErlAst::Lit { .. } => unreachable!("Ast::Literal has no nested nodes"),
      ErlAst::NewFunction { .. } => {}
      ErlAst::Var { .. } => {}
      ErlAst::App { .. } => {}
      ErlAst::Let { .. } => {}
      ErlAst::Case { .. } => {}
      ErlAst::BinaryOp { .. } => {}
      ErlAst::UnaryOp { .. } => {}
    }
  }

  pub fn has_children(&self) -> bool {
    match self {
      // Descend into module contents
      ErlAst::Forms(_) => unreachable!("Do not call on module root ErlAst::Forms"),
      ErlAst::ModuleAttr { .. } => false,
      ErlAst::Lit { .. } => false, // TODO: nested literals?
      // Descend into args expressions, and into body
      ErlAst::NewFunction { .. } => true,
      ErlAst::Var { .. } => false,
      // Descend into app expression and args expressions
      ErlAst::App { .. } => true,
      // Descend into variable value, and in-body
      ErlAst::Let { .. } => true,
      // Descend into the condition, and clauses
      ErlAst::Case { .. } => true,
      // Descend into the args
      ErlAst::BinaryOp { .. } => true,
      // Descend into the arg
      ErlAst::UnaryOp { .. } => true,
    }
  }

  pub fn new_fun(name: &str, clauses: Vec<FunctionClause>) -> Self {
    ErlAst::NewFunction {
      name: name.to_string(),
      clauses,
      ret: ErlType::new_typevar(),
    }
  }

  pub fn new_var(name: &str) -> ErlAst {
    ErlAst::Var {
      name: name.to_string(),
      ty: ErlType::new_typevar(),
    }
  }
}

/// A tree of Erlang nodes with attached file name, and root element removed
pub(crate) type ErlAstTree = AstTree<ErlAst>;

/// A cache of trees of Erlang nodes, keyed by filename or module name
pub(crate) type ErlAstCache = AstCache<ErlAst>;

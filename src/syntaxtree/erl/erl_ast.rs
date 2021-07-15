use crate::syntaxtree::ast_cache::{AstCache, AstTree};
use crate::syntaxtree::erl::literal::ErlLiteral;
use crate::typing::typevar::TypeVar;
use crate::typing::erl_type::ErlType;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub enum ErlAst {
  /// Forms list, root of a module
  Forms(Vec<Rc<ErlAst>>),

  /// Generic module attribute -"string"(value, ...).
  ModuleAttr { name: String, args: Vec<String> },

  /// Comma expression receives type of its last AST element
  Comma { exprs: Vec<Rc<ErlAst>>, ty: ErlType },

  /// Defines a new function.
  /// A function has clauses.
  /// Each clause has same quantity of args (some AST nodes), bindable expressions,
  /// and a return type, initially Any
  NewFunction {
    name: String,
    // Each clause is ErlExpr, and union of clause types will be function return type
    ret: ErlType,
    clauses: Vec<Rc<ErlAst>>,
  },

  FClause {
    args: Vec<Rc<ErlAst>>,
    arg_types: Vec<TypeVar>,
    body: Rc<ErlAst>,
    ret: ErlType,
  },

  CClause {
    /// A match expression, matched vs. case arg
    cond: Rc<ErlAst>,
    /// Must resolve to bool, or an exception
    guard: Rc<ErlAst>,
    body: Rc<ErlAst>,
    /// Clause body type
    ty: ErlType,
  },

  /// A named variable
  Var {
    name: String,
    ty: ErlType,
  },

  /// Apply arguments to expression
  App {
    /// Target, to be called, expected to have function or lambda type fun((arg, arg,...) -> ret)
    expr: Rc<ErlAst>,
    /// Arguments. Their  inferred types are stored inside.
    args: Vec<Rc<ErlAst>>,
    /// Inferred type of return
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
    value: Rc<ErlAst>,
    /// Let x=y in <body> (type is in it, and becomes type of Expr::Let)
    in_expr: Rc<ErlAst>,
    in_ty: ErlType,
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
    arg: Rc<ErlAst>,
    clauses: Vec<Rc<ErlAst>>,
  },

  /// A literal value, constant. Type is known via literal.get_type()
  Lit(ErlLiteral),

  BinaryOp {
    left: Rc<ErlAst>,
    right: Rc<ErlAst>,
    op: ErlBinaryOp,
    ty: ErlType,
  },
  UnaryOp {
    expr: Rc<ErlAst>,
    op: ErlUnaryOp,
  },
}

impl ErlAst {
  pub fn get_type(&self) -> ErlType {
    match self {
      ErlAst::Forms(_) => ErlType::Any,
      ErlAst::ModuleAttr { .. } => ErlType::Any,
      ErlAst::NewFunction { ret, .. } => ret.clone(),
      ErlAst::FClause { body, .. } => body.get_type(),
      ErlAst::CClause { body, .. } => body.get_type(),
      ErlAst::Var { ty, .. } => ty.clone(),
      ErlAst::App { ty, .. } => ty.clone(),
      ErlAst::Let { in_expr, .. } => in_expr.get_type(),
      ErlAst::Case { ty, .. } => ty.clone(),
      ErlAst::Lit(l) => l.get_type().clone(),
      ErlAst::BinaryOp { op, .. } => op.get_result_type(),
      ErlAst::UnaryOp { expr, .. } => expr.get_type(), // same type as expr bool or num
      ErlAst::Comma { exprs, .. } => {
        exprs[exprs.len() - 1].get_type()
      }
    }
  }

  /// Create a new function clause
  pub fn new_fclause(args: Vec<Rc<ErlAst>>, body: Rc<ErlAst>) -> Rc<Self> {
    let arg_types = args.iter().map(|_a| TypeVar::new()).collect();
    Rc::new(Self::FClause {
      args,
      arg_types,
      body,
      ret: ErlType::new_typevar(),
    })
  }

  /// Build a vec of references to children
  pub fn get_children(&self) -> Option<Vec<Rc<ErlAst>>> {
    match self {
      ErlAst::Forms(f) => Some(f.clone()),
      ErlAst::ModuleAttr { .. } => None,
      ErlAst::Lit { .. } => None,
      ErlAst::NewFunction { clauses, .. } => {
        Some(clauses.clone())
      }
      ErlAst::FClause { args, body, .. } => {
        // Descend into args, and the body
        let mut args_refs: Vec<Rc<ErlAst>> = args.clone();
        args_refs.push(body.clone());
        Some(args_refs)
      }
      ErlAst::Var { .. } => None,
      ErlAst::App { expr, args, .. } => {
        let mut r = vec![expr.clone()];
        args.iter().for_each(|a| r.push(a.clone()));
        Some(r)
      }
      ErlAst::Let { value, in_expr, .. } => {
        Some(vec![value.clone(),
                  in_expr.clone()])
      }
      ErlAst::Case { arg, clauses, .. } => {
        let mut r = vec![arg.clone()];
        clauses.iter().for_each(|a| r.push(a.clone()));
        Some(r)
      }
      ErlAst::CClause { cond, guard, body, .. } => {
        Some(vec![cond.clone(), guard.clone(), body.clone()])
      }
      ErlAst::BinaryOp { left, right, .. } => {
        Some(vec![left.clone(), right.clone()])
      }
      ErlAst::UnaryOp { expr, .. } => Some(vec![expr.clone()]),
      ErlAst::Comma { exprs, .. } => Some(exprs.clone()),
    }
  }

  pub fn new_fun(name: &str, clauses: Vec<Rc<ErlAst>>) -> Rc<Self> {
    Rc::new(ErlAst::NewFunction {
      name: name.to_string(),
      clauses,
      ret: ErlType::new_typevar(),
    })
  }

  pub fn new_var(name: &str) -> Rc<ErlAst> {
    Rc::new(ErlAst::Var {
      name: name.to_string(),
      ty: ErlType::new_typevar(),
    })
  }
}

/// A tree of Erlang nodes with attached file name, and root element removed
pub(crate) type ErlAstTree = AstTree<ErlAst>;

/// A cache of trees of Erlang nodes, keyed by filename or module name
pub(crate) type ErlAstCache = AstCache<ErlAst>;

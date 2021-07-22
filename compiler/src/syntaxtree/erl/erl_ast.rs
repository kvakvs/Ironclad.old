//! AST syntax structure of an Erlang file
use crate::syntaxtree::ast_cache::{AstCache, AstTree};
use crate::syntaxtree::erl::literal::ErlLit;
use crate::typing::typevar::TypeVar;
use crate::typing::erl_type::ErlType;
use crate::syntaxtree::erl::erl_op::{ErlBinaryOp, ErlUnaryOp};
use std::rc::Rc;

/// Temporary token marking tokens of interest while parsing the AST tree. Must not be present in
/// the final AST produced by the parser.
#[allow(missing_docs)]
#[derive(PartialEq)]
pub enum ErlToken {
  Comma,
  Plus,
  Minus,
  Div,
  Mul,
  IntegerDiv,
  Remainder,
  Not,
  Or,
  Xor,
  And,
  BinaryNot,
  BinaryAnd,
  BinaryOr,
  BinaryXor,
  BinaryShiftLeft,
  BinaryShiftRight,
  ListAppend,
  ListSubtract,
  Eq,
  NotEq,
  LessThanEq,
  LessThan,
  GreaterEq,
  GreaterThan,
  HardEq,
  HardNotEq,
  AndAlso,
  OrElse,
  Assign,
  Send,
  Catch,
}

/// AST node in parsed Erlang source
#[derive(PartialEq)]
pub enum ErlAst {
  /// Default value for when AST tree is empty
  Empty,

  /// A token to be consumed by AST builder, temporary, must not exist in final AST
  Token(ErlToken),

  /// Forms list, root of a module
  Forms(Vec<Rc<ErlAst>>),

  /// -module(name). attribute, defines module start
  ModuleAttr {
    /// Module name atom, stored as string
    name: String
  },

  /// Comma expression receives type of its last AST element
  Comma {
    /// Expressions joined with commas
    exprs: Vec<Rc<ErlAst>>,
    /// Final type for last expression, also is the type of entire comma operator
    ty: ErlType,
  },

  /// Defines a new function, with clauses.
  /// Each clause has same quantity of args (some AST nodes), bindable expressions,
  /// and a return type, initially Any
  NewFunction {
    /// Each clause is ErlExpr, and union of clause types will be function return type
    ret: ErlType,
    /// Function clauses in order
    clauses: Vec<Rc<ErlAst>>,
  },

  /// Function clause for a new function definition
  FClause {
    /// Function name atom, stored as a string. All clauses of the same function must have same name
    name: String,
    /// Function clause arguments, binding/match expressions
    args: Vec<Rc<ErlAst>>,
    /// Types we believe the arguments will have
    arg_types: Vec<TypeVar>,
    /// Function clause body
    body: Rc<ErlAst>,
    /// Return type for this function clause
    ret: ErlType,
  },

  /// Case clause for a `case x of` switch
  CClause {
    /// A match expression, matched vs. case arg
    cond: Rc<ErlAst>,
    /// Must resolve to bool, or an exception
    guard: Rc<ErlAst>,
    /// Case clause body expression
    body: Rc<ErlAst>,
    /// Clause body type, for type inference
    ty: ErlType,
  },

  /// A named variable
  Var {
    /// Variable name
    name: String,
    /// Variable type for inference
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

  /// A haskell-style new variable introducing a new scope below it:
  /// let x = expr1 in expr2
  Let {
    /// The variable name assigned in let..in
    var: String,
    /// Type which we believe the Variable will have
    var_ty: ErlType,
    /// Value (type is in it)
    value: Rc<ErlAst>,
    /// Let x=y in <body> (type is in it, and becomes type of Expr::Let)
    in_expr: Rc<ErlAst>,
    /// The let .. in ... result type
    in_ty: ErlType,
  },

  /// Case switch containing the argument to check, and case clauses
  Case {
    /// A union type of all case clauses
    ty: ErlType,
    /// Argument of the `case X of`
    arg: Rc<ErlAst>,
    /// All case clauses in order
    clauses: Vec<Rc<ErlAst>>,
  },

  /// A literal value, constant. Type is known via literal.get_type()
  Lit(ErlLit),

  /// Binary operation with two arguments
  BinaryOp {
    /// Left operand
    left: Rc<ErlAst>,
    /// Right operand
    right: Rc<ErlAst>,
    /// The operation
    op: ErlBinaryOp,
    /// The type of the operation
    ty: ErlType,
  },
  /// Unary operation with 1 argument
  UnaryOp {
    /// The operand
    expr: Rc<ErlAst>,
    /// The operation
    op: ErlUnaryOp,
  },
}

impl ErlAst {
  /// Gets the type of an AST node
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
      _ => unreachable!("Can't process {:?}", self),
    }
  }

  /// Retrieve a type of a function node (None if node is not a new function definition)
  pub fn get_fun_type(&self) -> Option<ErlType> {
    match self {
      ErlAst::NewFunction { ret, clauses } => {
        assert!(clauses.len() > 0, "Function clauses must not be empty");
        let t = ErlType::new_fun(
          clauses[0].get_fclause_name(),
          clauses.iter().map(|c| c.get_type()).collect(),
          ret.clone());
        Some(t)
      }
      _ => None, // not a function
    }
  }

  /// Retrieve a name of a function node (first clause name)
  pub fn get_fun_name(&self) -> Option<&str> {
    match self {
      ErlAst::NewFunction { clauses, .. } => {
        assert!(clauses.len() > 0, "get_fun_name: FClauses must not be empty");
        clauses[0].get_fun_name()
      }
      ErlAst::FClause { name, .. } => Some(&name),
      _ => None, // not a function
    }
  }

  /// Create a new function clause
  pub fn new_fclause(name: &str, args: Vec<Rc<ErlAst>>, body: Rc<ErlAst>) -> Rc<Self> {
    let arg_types = args.iter().map(|_a| TypeVar::new()).collect();
    Rc::new(Self::FClause {
      name: name.to_string(),
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
      ErlAst::Token(_) => panic!("Token {:?} must be eliminated in AST build phase", self),

      _ => unreachable!("Can't process {:?}", self),
    }
  }

  /// Create a new function definition node
  pub fn new_fun(clauses: Vec<Rc<ErlAst>>) -> Rc<Self> {
    Rc::new(ErlAst::NewFunction {
      clauses,
      ret: ErlType::new_typevar(),
    })
  }

  /// Create a new variable AST node
  pub fn new_var(name: &str) -> Rc<ErlAst> {
    Rc::new(ErlAst::Var {
      name: name.to_string(),
      ty: ErlType::new_typevar(),
    })
  }

  /// Create an new binary operation AST node with left and right operands AST
  pub fn new_binop(left: Rc<ErlAst>, op: ErlBinaryOp, right: Rc<ErlAst>) -> Rc<Self> {
    Rc::new(ErlAst::BinaryOp {
      left,
      right,
      op,
      ty: ErlType::new_typevar(),
    })
  }

  /// Create a new literal AST node of an integer
  pub fn new_lit_int(val: isize) -> Rc<Self> {
    Rc::new(ErlAst::Lit(ErlLit::Integer(val)))
  }

  /// Create a new literal AST node of an atom
  pub fn new_lit_atom(val: &str) -> Rc<Self> {
    Rc::new(ErlAst::Lit(ErlLit::Atom(String::from(val))))
  }

  /// Create a new temporary token, which holds a place temporarily, it must be consumed in the
  /// same function and not exposed to the rest of the program.
  pub fn temporary_token(t: ErlToken) -> Rc<Self> {
    Rc::new(ErlAst::Token(t))
  }

  /// Create a new Comma operator from list of AST expressions
  pub fn new_comma(items: Vec<Rc<ErlAst>>) -> Rc<Self> {
    Rc::new(ErlAst::Comma {
      exprs: items,
      ty: ErlType::new_typevar(),
    })
  }

  /// Retrieve Some(atom text) if AST node is atom
  pub fn get_atom_text(&self) -> Option<String> {
    match self {
      ErlAst::Lit(ErlLit::Atom(s)) => Some(s.clone()),
      _ => None,
    }
  }

  /// Retrieve Some(function clause name) if AST node is a function clause
  pub fn get_fclause_name(&self) -> Option<String> {
    match self {
      ErlAst::FClause { name, .. } => Some(name.clone()),
      _ => None,
    }
  }
}

/// A tree of Erlang nodes with attached file name, and root element removed
pub type ErlAstTree = AstTree<ErlAst>;

/// A cache of trees of Erlang nodes, keyed by filename or module name
pub type ErlAstCache = AstCache<ErlAst>;

impl Default for ErlAst {
  fn default() -> Self {
    ErlAst::Empty
  }
}

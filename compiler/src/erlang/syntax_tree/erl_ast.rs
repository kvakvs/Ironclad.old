//! AST syntax structure of an Erlang file
use ::function_name::named;
use crate::erlang::syntax_tree::node::apply::Apply;
use crate::erlang::syntax_tree::node::case_clause::CaseClause;
use crate::erlang::syntax_tree::node::case::Case;
use crate::erlang::syntax_tree::erl_op::ErlBinaryOp;
use crate::erlang::syntax_tree::node::literal::Literal;
use crate::erlang::syntax_tree::node::expression::{BinaryOperatorExpr, UnaryOperatorExpr};
use crate::erlang::syntax_tree::node::var::Var;
use crate::erlang::syntax_tree::node::token::ErlToken;
use crate::erlang::syntax_tree::node::fn_def::FnDef;
use std::sync::Arc;
use crate::source_loc::SourceLoc;
use crate::mfarity::MFArity;
use crate::typing::typevar::TypeVar;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_type::FunctionType;
use crate::ast_tree::{AstTree, AstCache};

/// AST node in parsed Erlang source
pub enum ErlAst {
  /// Default value for when AST tree is empty. Should create error, not a valid AST node.
  Empty,
  /// Comment text eliminated.
  Comment(SourceLoc),

  /// A token to be consumed by AST builder, temporary, must not exist in final AST
  Token {
    /// Source file pointer
    location: SourceLoc,
    /// The token enum
    token: ErlToken,
  },

  /// Forms list, root of a module
  ModuleForms(Vec<ErlAst>),

  /// -module(name). attribute, defines module start
  ModuleAttr {
    /// Source file pointer
    location: SourceLoc,
    /// Module name atom, stored as string
    name: String,
  },

  // /// Comma expression receives type of its last AST element
  // Comma {
  //   /// Source file pointer
  //   location: SourceLoc,
  //   /// Left expression
  //   left: Box<ErlAst>,
  //   /// Right expression
  //   right: Box<ErlAst>,
  //   /// Type for right expression, also is the type of entire comma operator
  //   ty: TypeVar,
  // },

  /// Defines a new function, with clauses.
  /// Each clause has same quantity of args (some AST nodes), bindable expressions,
  /// and a return type, initially Any.
  FnDef {
    /// Source file pointer
    location: SourceLoc,
    /// Function name and arity
    funarity: MFArity,
    /// Clone of return type variable
    ret_ty: TypeVar,
    /// Function definition struct with clauses. Arc to be accessible separately from function
    /// lookup code, also stored in the `ErlModule::functions`.
    fn_def: Arc<FnDef>,
  },

  /// Case clause for a `case x of` switch
  CClause(SourceLoc, CaseClause),

  /// Name/arity which refers to a function in the current module
  MFA {
    /// Code location
    location: SourceLoc,
    /// fun/arity in the current module, or full mod:fun/arity if external
    mfarity: MFArity,
    /// Known function clause types from the function name lookup, empty if not known
    clause_types: Vec<FnClauseType>,
    /// Unique typevar for the return type, will be matched against what we find out later
    ret_ty: TypeVar,
  },

  /// A named variable
  Var(SourceLoc, Var),

  /// Apply arguments to expression
  Apply(SourceLoc, Apply),

  /// Case switch containing the argument to check, and case clauses
  Case(SourceLoc, Case),

  /// A literal value, constant. Type is known via literal.get_type()
  Lit(SourceLoc, Literal),

  /// Binary operation with two arguments
  BinaryOp(SourceLoc, BinaryOperatorExpr),

  /// Unary operation with 1 argument
  UnaryOp(SourceLoc, UnaryOperatorExpr),

  /// A list of some expressions, TODO: constant folding convert into ErlAst::Lit(ErlLit::List())
  List {
    /// Source code location
    location: SourceLoc,
    /// List elements
    elements: Vec<ErlAst>,
    /// Optional tail element if not NIL
    tail: Option<Box<ErlAst>>,
  },

  /// A tuple of some expressions, TODO: constant folding
  Tuple {
    /// Source code location
    location: SourceLoc,
    /// Tuple elements
    elements: Vec<ErlAst>,
  },
}

impl ErlAst {
  /// Swaps a value and Empty AST, returns the taken value
  pub fn take(from: &mut ErlAst) -> ErlAst {
    let mut swap_in = ErlAst::Empty;
    std::mem::swap(from, &mut swap_in);
    swap_in
  }

  /// Gets the type of an AST node
  #[named]
  pub fn get_type(&self) -> ErlType {
    match self {
      ErlAst::ModuleForms(_) => ErlType::Any,
      ErlAst::ModuleAttr { .. } => ErlType::Any,
      ErlAst::FnDef { ret_ty, .. } => (*ret_ty).into(),
      ErlAst::CClause(_loc, clause) => clause.body.get_type(),
      ErlAst::Var(_loc, v) => v.ty.into(),
      ErlAst::Apply(_loc, app) => app.ret_ty.into(),
      ErlAst::Case(_loc, case) => case.ret_ty.into(),
      ErlAst::Lit(_loc, l) => l.get_type(),
      ErlAst::BinaryOp(_loc, binop) => binop.get_result_type(),
      ErlAst::UnaryOp(_loc, unop) => unop.expr.get_type(), // same type as expr bool or num
      // ErlAst::Comma { right, .. } => right.get_type(),
      ErlAst::List { elements, tail, .. } => {
        assert!(tail.is_none()); // todo
        let union_t = ErlType::union_of(
          elements.iter().map(|e| e.get_type()).collect(),
          true);
        ErlType::List(Box::new(union_t))
      }
      ErlAst::Tuple { elements, .. } => {
        ErlType::Tuple(elements.iter().map(|e| e.get_type()).collect())
      }
      ErlAst::MFA { clause_types, .. } => {
        let fn_type = FunctionType::new(None, clause_types.clone());
        ErlType::Fn(fn_type)
      }
      _ => unreachable!("{}: Can't process {}", function_name!(), self),
    }
  }

  // /// Create a new function definition node, the caller has the responsibility to store it and
  // /// to create `ErlAst::FunctionDef` with the stored index
  // pub fn new_fun(funarity: FunArity,
  //                start_clause: usize,
  //                clause_count: usize,
  //                clauses: &[FunctionClauseNode]) -> FunctionDefNode {
  //   assert_ne!(clause_count, 0, "Clauses must not be empty");
  //
  //   assert!(clauses.iter().all(|fc| fc.arg_types.len() == funarity.arity),
  //           "All clauses must have same arity");
  //   assert!(clauses.iter().all(|fc| fc.arg_types.len() == fc.args.len()),
  //           "All clause arg types must match in length all clauses' arguments");
  //
  //   FunctionDefNode::new(funarity, start_clause, clause_count)
  // }

  /// Create a new variable AST node
  pub fn new_var(location: SourceLoc, name: &str) -> ErlAst {
    ErlAst::Var(location, Var::new(name))
  }

  /// Creates a new AST node to perform a function call (application of args to a func expression)
  pub fn new_application(location: SourceLoc, expr: ErlAst, args: Vec<ErlAst>) -> ErlAst {
    ErlAst::Apply(location, Apply::new(expr, args))
  }

  /// Creates a new AST node to perform a function call (application of 0 args to a func expression)
  pub fn new_application0(location: SourceLoc, expr: ErlAst) -> ErlAst {
    ErlAst::Apply(location, Apply::new(expr, vec![]))
  }

  /// Create an new binary operation AST node with left and right operands AST
  pub fn new_binop(location: SourceLoc,
                   left: ErlAst, op: ErlBinaryOp, right: ErlAst) -> ErlAst {
    ErlAst::BinaryOp(location,
                     BinaryOperatorExpr {
                       left: Box::new(left),
                       right: Box::new(right),
                       operator: op,
                       ty: TypeVar::new(),
                     })
  }

  /// Create a new literal AST node of an integer
  pub fn new_lit_int(location: SourceLoc, val: isize) -> Self {
    ErlAst::Lit(location, Literal::Integer(val))
  }

  /// Create a new literal AST node of an atom
  pub fn new_lit_atom(location: SourceLoc, val: &str) -> ErlAst {
    ErlAst::Lit(location, Literal::Atom(String::from(val)))
  }

  /// Create a new AST node for a list of some expressions
  pub fn new_list(location: SourceLoc, elements: Vec<ErlAst>) -> ErlAst {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    ErlAst::List { location, elements, tail: None }
  }

  /// Create a new AST node for a tuple of some expressions
  pub fn new_tuple(location: SourceLoc, elements: Vec<ErlAst>) -> ErlAst {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    ErlAst::Tuple { location, elements }
  }

  /// Create a new temporary token, which holds a place temporarily, it must be consumed in the
  /// same function and not exposed to the rest of the program.
  pub fn temporary_token(t: ErlToken) -> ErlAst {
    ErlAst::Token { location: SourceLoc::None, token: t }
  }

  // /// Create a new Comma operator from list of AST expressions
  // pub fn new_comma(location: SourceLoc, left: ErlAst, right: ErlAst) -> Self {
  //   ErlAst::Comma {
  //     location,
  //     left: Box::new(left),
  //     right: Box::new(right),
  //     ty: TypeVar::new(),
  //   }
  // }

  /// Retrieve Some(atom text) if AST node is atom
  pub fn get_atom_text(&self) -> Option<String> {
    match self {
      ErlAst::Lit(_loc, Literal::Atom(s)) => Some(s.clone()),
      _ => None,
    }
  }

  // /// Retrieve Some(function clause name) if AST node is a function clause
  // pub fn get_fclause_name(&self) -> Option<String> {
  //   match self {
  //     ErlAst::FClause(_loc, fc) => Some(fc.name.clone()),
  //     _ => None,
  //   }
  // }

  /// Unwrap self as new function, returns index in the `ErlModule::functions` table on success
  pub fn as_fn_def(&self) -> Option<Arc<FnDef>> {
    match self {
      ErlAst::FnDef { fn_def: func_def, .. } => Some(func_def.clone()),
      _ => None,
    }
  }

  /// Given a comma operator, unroll the comma nested tree into a vector of AST, this is used for
  /// function calls, where args are parsed as a single Comma{} and must be converted to a vec.
  /// A non-comma AST-node becomes a single result element.
  pub fn comma_to_vec(comma_ast: ErlAst, dst: &mut Vec<ErlAst>) {
    match comma_ast {
      ErlAst::BinaryOp(_loc, binexpr) if binexpr.operator == ErlBinaryOp::Comma => {
        Self::comma_to_vec(*binexpr.left, dst);
        Self::comma_to_vec(*binexpr.right, dst);
      }
      _ => dst.push(comma_ast),
    }
  }

  /// Retrieve source file location for an AST element
  pub fn location(&self) -> SourceLoc {
    match self {
      ErlAst::Comment(loc) => loc.clone(),
      ErlAst::Token { location: loc, .. } => loc.clone(),
      ErlAst::ModuleAttr { location: loc, .. } => loc.clone(),
      // ErlAst::Comma { location: loc, .. } => loc.clone(),
      ErlAst::FnDef { location: loc, .. } => loc.clone(),
      // ErlAst::FClause(loc, _) => loc.clone(),
      ErlAst::CClause(loc, _) => loc.clone(),
      ErlAst::MFA { location: loc, .. } => loc.clone(),
      ErlAst::Var(loc, _) => loc.clone(),
      ErlAst::Apply(loc, _) => loc.clone(),
      ErlAst::Case(loc, _) => loc.clone(),
      ErlAst::Lit(loc, _) => loc.clone(),
      ErlAst::BinaryOp(loc, _) => loc.clone(),
      ErlAst::UnaryOp(loc, _) => loc.clone(),

      _ => SourceLoc::None,
    }
  }

  /// Scan forms and find a module definition AST node. For finding a function by funarity, check
  /// function registry `ErlModule::env`
  pub fn find_function_def(&self, fa: &MFArity) -> Option<&ErlAst> {
    match self {
      ErlAst::FnDef { funarity: fa2, .. } if fa == fa2 => Some(self),
      ErlAst::ModuleForms(forms) => {
        // Find first in forms for which `find_function_def` returns something
        forms.iter().find(|&f| f.find_function_def(fa).is_some())
      }
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

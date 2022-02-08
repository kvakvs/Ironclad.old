//! AST syntax structure of an Erlang file
use crate::erlang::syntax_tree::node::erl_apply::ErlApply;
use crate::erlang::syntax_tree::node::erl_case_clause::ErlCaseClause;
use crate::erlang::syntax_tree::node::erl_case::ErlCase;
use crate::erlang::syntax_tree::erl_op::ErlBinaryOp;
use crate::literal::Literal;
use crate::erlang::syntax_tree::node::erl_binop::{ErlBinaryOperatorExpr};
use crate::erlang::syntax_tree::node::erl_var::ErlVar;
use crate::erlang::syntax_tree::node::erl_token::ErlToken;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::source_loc::SourceLoc;
use crate::mfarity::MFArity;
use crate::ast_tree::{AstCache, AstTree};
use std::sync::Arc;
use std::ops::Deref;
use crate::erl_error::ErlResult;
use crate::erlang::syntax_tree::node::erl_unop::ErlUnaryOperatorExpr;
use crate::typing::erl_type::ErlType;
use crate::typing::type_error::TypeError;

pub mod ast_iter;
pub mod ast_print;
pub mod ast_as;
pub mod ast_is;

/// AST node in parsed Erlang source
#[derive(Debug)]
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
  ModuleForms(Vec<Arc<ErlAst>>),

  /// -module(name). attribute, defines module start
  ModuleStartAttr {
    /// Source file pointer
    location: SourceLoc,
    /// Module name atom, stored as string
    name: String,
  },

  /// A generic attribute `"-name" ... "."\n` extracted from the source, parsed at a later stage
  UnparsedAttr {
    /// Source file pointer
    location: SourceLoc,
    /// Module name atom, stored as string
    text: String,
  },

  /// Defines a new function, with clauses, or an inline lambda (then name will be `None`).
  /// Each clause has same quantity of args (some AST nodes), bindable expressions,
  /// and a return type, initially Any.
  FnDef(ErlFnDef),

  /// Points to a function, replaced from apply calls and fun exprs, where we know that the
  /// expression must be callable
  FnRef {
    /// Code source location
    location: SourceLoc,
    /// Function name
    mfa: MFArity,
  },

  /// A function spec, written as `-spec myfun(...) -> <ret type> when ... <optional when>.`
  FnSpec {
    /// Code source location
    location: SourceLoc,
    /// The function name and arity, module as None
    funarity: MFArity,
    /// Type for all function clauses
    spec: Arc<ErlType>,
  },

  /// A temporary node wrapper for parsed types. TODO: Use more extensively in typespecs and maybe in the augmented syntax?
  Type {
    /// Code source location
    location: SourceLoc,
    /// The type
    ty: Arc<ErlType>,
  },

  /// Case clause for a `case x of` switch
  CClause(SourceLoc, ErlCaseClause),

  /// Name/arity which refers to a function in the current module
  MFA {
    /// Code location
    location: SourceLoc,
    /// fun/arity in the current module, or full mod:fun/arity if external
    mfarity: MFArity,
    // /// Known function clause types from the function name lookup, empty if not known
    // clause_types: Vec<FnClauseType>,
  },

  /// A named variable
  Var(ErlVar),

  /// Apply arguments to expression
  Apply(ErlApply),

  /// Case switch containing the argument to check, and case clauses
  Case(SourceLoc, ErlCase),

  /// A literal value, constant. Type is known via literal.get_type()
  Lit {
    /// Source code location
    location: SourceLoc,
    /// The literal value
    value: Arc<Literal>,
  },

  /// Binary operation with two arguments
  BinaryOp {
    /// Source code location
    location: SourceLoc,
    /// The contained binary A&B expression
    expr: ErlBinaryOperatorExpr,
  },

  /// Unary operation with 1 argument
  UnaryOp {
    /// Source code location
    location: SourceLoc,
    /// The contained unary &B expression
    expr: ErlUnaryOperatorExpr,
  },

  /// A list of some expressions, TODO: constant folding convert into ErlAst::Lit(ErlLit::List())
  List {
    /// Source code location
    location: SourceLoc,
    /// List elements
    elements: Vec<Arc<ErlAst>>,
    /// Optional tail element if not NIL
    tail: Option<Arc<ErlAst>>,
  },

  /// A tuple of some expressions, TODO: constant folding
  Tuple {
    /// Source code location
    location: SourceLoc,
    /// Tuple elements
    elements: Vec<Arc<ErlAst>>,
  },
}

impl ErlAst {
  /// Returns true for ErlAst::Var
  pub fn is_var(&self) -> bool {
    matches!(self, ErlAst::Var(..))
  }

  /// Swaps a value and Empty AST, returns the taken value
  pub fn take(from: &mut ErlAst) -> ErlAst {
    let mut swap_in = ErlAst::Empty;
    std::mem::swap(from, &mut swap_in);
    swap_in
  }

  /// Create a new variable AST node
  pub fn new_var(location: SourceLoc, name: &str) -> Arc<ErlAst> {
    ErlAst::Var(ErlVar::new(location, name))
        .into()
  }

  /// Creates a new AST node to perform a function call (application of args to a func expression)
  pub fn new_application(location: SourceLoc, expr: Arc<ErlAst>, args: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    ErlAst::Apply(ErlApply::new(location, expr, args))
        .into()
  }

  /// Creates a new AST node to perform a function call (application of 0 args to a func expression)
  pub fn new_application0(location: SourceLoc, expr: Arc<ErlAst>) -> Arc<ErlAst> {
    ErlAst::Apply(ErlApply::new(location, expr, vec![]))
        .into()
  }

  /// Create an new binary operation AST node with left and right operands AST
  pub fn new_binop(location: SourceLoc,
                   left: Arc<ErlAst>, op: ErlBinaryOp, right: Arc<ErlAst>) -> Arc<ErlAst> {
    ErlAst::BinaryOp {
      location,
      expr: ErlBinaryOperatorExpr { left, right, operator: op },
    }.into()
  }

  /// Create a new literal AST node of an integer
  pub fn new_lit_int(location: SourceLoc, val: isize) -> Arc<ErlAst> {
    ErlAst::Lit {
      location,
      value: Literal::Integer(val).into(),
    }.into()
  }

  /// Create a new literal AST node of an atom
  pub fn new_lit_atom(location: SourceLoc, val: &str) -> Arc<ErlAst> {
    ErlAst::Lit {
      location,
      value: Literal::Atom(String::from(val)).into(),
    }.into()
  }

  /// Create a new literal AST node of a floating point number
  pub fn new_lit_float(location: SourceLoc, val: &str) -> Arc<ErlAst> {
    match String::from(val).trim().parse() {
      Ok(flt) => ErlAst::Lit {
        location,
        value: Literal::Float(flt).into(),
      }.into(),
      Err(e) => {
        panic!("Failed to parse float from \"{}\": error {}", val, e)
      }
    }
  }

  /// Create a new literal AST node of a "string"
  pub fn new_lit_string(location: SourceLoc, val: &str) -> Arc<ErlAst> {
    ErlAst::Lit {
      location,
      value: Literal::String(String::from(val)).into(),
    }.into()
  }

  /// Create a new AST node for a list of some expressions
  pub fn new_list(location: SourceLoc, elements: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    ErlAst::List { location, elements, tail: None }
        .into()
  }

  /// Create a new AST node for a tuple of some expressions
  pub fn new_tuple(location: SourceLoc, elements: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    ErlAst::Tuple { location, elements }
        .into()
  }

  /// Create a new temporary token, which holds a place temporarily, it must be consumed in the
  /// same function and not exposed to the rest of the program.
  pub fn temporary_token(t: ErlToken) -> Arc<ErlAst> {
    ErlAst::Token { location: SourceLoc::None, token: t }
        .into()
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
      ErlAst::Lit { value: lit, .. } => {
        if let Literal::Atom(s) = lit.deref() { Some(s.clone()) } else { None }
      }
      _ => None,
    }
  }

  #[deprecated = "Not used since CoreAST is gone"]
  /// Given a comma operator, unroll the comma nested tree into a vector of AST, this is used for
  /// function calls, where args are parsed as a single Comma{} and must be converted to a vec.
  /// A non-comma AST-node becomes a single result element.
  pub fn comma_to_vec(comma_ast: &Arc<ErlAst>, dst: &mut Vec<Arc<ErlAst>>) {
    match comma_ast.deref() {
      ErlAst::BinaryOp { expr: binexpr, .. } if binexpr.operator == ErlBinaryOp::Comma => {
        Self::comma_to_vec(&binexpr.left, dst);
        Self::comma_to_vec(&binexpr.right, dst);
      }
      _ => dst.push(comma_ast.clone()),
    }
  }

  /// Retrieve source file location for an AST element
  pub fn location(&self) -> SourceLoc {
    match self {
      ErlAst::Comment(loc) => loc.clone(),
      ErlAst::Token { location: loc, .. } => loc.clone(),
      ErlAst::ModuleStartAttr { location: loc, .. } => loc.clone(),
      // ErlAst::Comma { location: loc, .. } => loc.clone(),
      ErlAst::FnDef(erl_fndef) => erl_fndef.location.clone(),
      // ErlAst::FClause(loc, _) => loc.clone(),
      ErlAst::CClause(loc, _) => loc.clone(),
      ErlAst::MFA { location: loc, .. } => loc.clone(),
      ErlAst::Var(var) => var.location.clone(),
      ErlAst::Apply(app) => app.location.clone(),
      ErlAst::Case(loc, _) => loc.clone(),
      ErlAst::Lit { location: loc, .. } => loc.clone(),
      ErlAst::BinaryOp { location: loc, .. } => loc.clone(),
      ErlAst::UnaryOp { location: loc, .. } => loc.clone(),

      _ => SourceLoc::None,
    }
  }

  /// Scan forms and find a module definition AST node. For finding a function by funarity, check
  /// function registry `ErlModule::env`
  pub fn find_function_def(this: &Arc<ErlAst>, funarity: &MFArity) -> ErlResult<Arc<ErlAst>> {
    match this.deref() {
      ErlAst::FnDef(erl_fndef) if *funarity == erl_fndef.funarity => {
        return Ok(this.clone());
      }
      ErlAst::ModuleForms(forms) => {
        // Find first in forms for which `find_function_def` returns something
        let find_result = forms.iter()
            .find(|&each_fndef| {
              ErlAst::find_function_def(each_fndef, funarity).is_ok()
            })
            .cloned();
        if find_result.is_some() {
          return Ok(find_result.unwrap());
        }
      }
      _ => {}
    }
    Err(TypeError::FunctionNotFound { mfa: funarity.clone() }.into())
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

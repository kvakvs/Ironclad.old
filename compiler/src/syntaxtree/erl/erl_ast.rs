//! AST syntax structure of an Erlang file
use ::function_name::named;
use crate::syntaxtree::ast_cache::{AstCache, AstTree};
use crate::syntaxtree::erl::node::application_node::ApplicationNode;
use crate::syntaxtree::erl::node::case_clause_node::CaseClauseNode;
use crate::syntaxtree::erl::node::case_expr_node::CaseExprNode;
use crate::syntaxtree::erl::erl_op::ErlBinaryOp;
use crate::syntaxtree::erl::node::fun_clause_node::FunctionClauseNode;
use crate::syntaxtree::erl::node::let_expr_node::LetExprNode;
use crate::syntaxtree::erl::node::literal_node::LiteralNode;
use crate::syntaxtree::erl::node::new_function_node::NewFunctionNode;
use crate::syntaxtree::erl::node::expr_node::{BinaryOperatorExprNode, UnaryOperatorExprNode};
use crate::syntaxtree::erl::node::var_node::VarNode;
use crate::typing::erl_type::ErlType;
use crate::funarity::FunArity;
use crate::source_loc::SourceLoc;
use crate::syntaxtree::erl::node::token::ErlToken;

/// AST node in parsed Erlang source
// #[derive(PartialEq)]
pub enum ErlAst {
  /// Default value for when AST tree is empty
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

  /// Comma expression receives type of its last AST element
  Comma {
    /// Source file pointer
    location: SourceLoc,
    /// Left expression
    left: Box<ErlAst>,
    /// Right expression
    right: Box<ErlAst>,
    /// Type for right expression, also is the type of entire comma operator
    ty: ErlType,
  },

  /// Defines a new function, with clauses.
  /// Each clause has same quantity of args (some AST nodes), bindable expressions,
  /// and a return type, initially Any
  NewFunction(SourceLoc, NewFunctionNode),

  // /// Function clause for a new function definition
  // FClause(SourceLoc, FunctionClauseNode),

  /// Case clause for a `case x of` switch
  CClause(SourceLoc, CaseClauseNode),

  /// Name/arity which refers to a function in the current module
  FunArity(SourceLoc, FunArity),
  // ModFunArity()

  /// A named variable
  Var(SourceLoc, VarNode),

  /// Apply arguments to expression
  App(SourceLoc, ApplicationNode),

  /// A haskell-style new variable introducing a new scope below it:
  /// let x = expr1 in expr2
  Let(SourceLoc, LetExprNode),

  /// Case switch containing the argument to check, and case clauses
  Case(SourceLoc, CaseExprNode),

  /// A literal value, constant. Type is known via literal.get_type()
  Lit(SourceLoc, LiteralNode),

  /// Binary operation with two arguments
  BinaryOp(SourceLoc, BinaryOperatorExprNode),

  /// Unary operation with 1 argument
  UnaryOp(SourceLoc, UnaryOperatorExprNode),

  /// A list of some expressions, TODO: constant folding convert into ErlAst::Lit(ErlLit::List())
  List(SourceLoc, Vec<ErlAst>),
  /// A tuple of some expressions, TODO: constant folding
  Tuple(SourceLoc, Vec<ErlAst>),
}

impl ErlAst {
  /// Gets the type of an AST node
  #[named]
  pub fn get_type(&self) -> ErlType {
    match self {
      ErlAst::ModuleForms(_) => ErlType::Any,
      ErlAst::ModuleAttr { .. } => ErlType::Any,
      ErlAst::NewFunction(_loc, nf) => nf.ret.clone(),
      ErlAst::CClause(_loc, clause) => clause.body.get_type(),
      ErlAst::Var(_loc, v) => v.ty.clone(),
      ErlAst::App(_loc, app) => app.ret_type.clone(),
      ErlAst::Let(_loc, let_expr) => let_expr.in_expr.get_type(),
      ErlAst::Case(_loc, case) => case.ret.clone(),
      ErlAst::Lit(_loc, l) => l.get_type(),
      ErlAst::BinaryOp(_loc, binop) => binop.operator.get_result_type(),
      ErlAst::UnaryOp(_loc, unop) => unop.expr.get_type(), // same type as expr bool or num
      ErlAst::Comma { right, .. } => right.get_type(),
      ErlAst::List(_loc, elems) => {
        let union_t = ErlType::union_of(elems.iter().map(|e| e.get_type()).collect());
        ErlType::List(Box::new(union_t))
      },
      ErlAst::Tuple(_loc, elems) => {
        ErlType::Tuple(elems.iter().map(|e| e.get_type()).collect())
      },
      _ => unreachable!("{}: Can't process {}", function_name!(), self),
    }
  }

  /// Create a new function definition node
  pub fn new_fun(location: SourceLoc,
                 funarity: FunArity,
                 clauses: Vec<FunctionClauseNode>) -> Self {
    assert!(!clauses.is_empty(), "Clauses must not be empty");

    assert!(clauses.iter().all(|fc| fc.arg_types.len() == funarity.arity),
            "All clauses must have same arity");
    assert!(clauses.iter().all(|fc| fc.arg_types.len() == fc.args.len()),
            "All clause arg types must match in length all clauses' arguments");

    let nf = NewFunctionNode::new(funarity, clauses, ErlType::new_typevar());
    ErlAst::NewFunction(location, nf)
  }

  /// Create a new variable AST node
  pub fn new_var(location: SourceLoc, name: &str) -> ErlAst {
    ErlAst::Var(location, VarNode::new(name))
  }

  /// Creates a new AST node to perform a function call (application of args to a func expression)
  pub fn new_application(location: SourceLoc, expr: ErlAst, args: Vec<ErlAst>) -> ErlAst {
    ErlAst::App(location, ApplicationNode::new(expr, args))
  }

  /// Creates a new AST node to perform a function call (application of 0 args to a func expression)
  pub fn new_application0(location: SourceLoc, expr: ErlAst) -> ErlAst {
    ErlAst::App(location, ApplicationNode::new(expr, vec![]))
  }

  /// Create an new binary operation AST node with left and right operands AST
  pub fn new_binop(location: SourceLoc,
                   left: ErlAst, op: ErlBinaryOp, right: ErlAst) -> ErlAst {
    ErlAst::BinaryOp(location,
                     BinaryOperatorExprNode {
                       left: Box::new(left),
                       right: Box::new(right),
                       operator: op,
                       ty: ErlType::new_typevar(),
                     })
  }

  /// Create a new literal AST node of an integer
  pub fn new_lit_int(location: SourceLoc, val: isize) -> Self {
    ErlAst::Lit(location, LiteralNode::Integer(val))
  }

  /// Create a new literal AST node of an atom
  pub fn new_lit_atom(location: SourceLoc, val: &str) -> ErlAst {
    ErlAst::Lit(location, LiteralNode::Atom(String::from(val)))
  }

  /// Create a new AST node for a list of some expressions
  pub fn new_list(location: SourceLoc, elems: Vec<ErlAst>) -> ErlAst {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    ErlAst::List(location, elems)
  }

  /// Create a new AST node for a tuple of some expressions
  pub fn new_tuple(location: SourceLoc, elems: Vec<ErlAst>) -> ErlAst {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    ErlAst::Tuple(location, elems)
  }

  /// Create a new temporary token, which holds a place temporarily, it must be consumed in the
  /// same function and not exposed to the rest of the program.
  pub fn temporary_token(t: ErlToken) -> ErlAst {
    ErlAst::Token { location: SourceLoc::default(), token: t }
  }

  /// Create a new Comma operator from list of AST expressions
  pub fn new_comma(location: SourceLoc, left: ErlAst, right: ErlAst) -> Self {
    ErlAst::Comma {
      location,
      left: Box::new(left),
      right: Box::new(right),
      ty: ErlType::new_typevar(),
    }
  }

  /// Retrieve Some(atom text) if AST node is atom
  pub fn get_atom_text(&self) -> Option<String> {
    match self {
      ErlAst::Lit(_loc, LiteralNode::Atom(s)) => Some(s.clone()),
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

  /// Unwrap self as new function
  pub fn as_new_function(&self) -> Option<&NewFunctionNode> {
    match self {
      ErlAst::NewFunction(_location, nf) => Some(nf),
      _ => None,
    }
  }

  /// Given a comma operator, unroll the comma nested tree into a vector of AST, this is used for
  /// function calls, where args are parsed as a single Comma{} and must be converted to a vec.
  /// A non-comma AST-node becomes a single result element.
  pub fn comma_to_vec(comma_ast: ErlAst, dst: &mut Vec<ErlAst>) {
    match comma_ast {
      ErlAst::Comma { left, right, .. } => {
        Self::comma_to_vec(*left, dst);
        Self::comma_to_vec(*right, dst);
      }
      _ => dst.push(comma_ast),
    }
  }

  /// Retrieve source file location for an AST element
  pub fn location(&self) -> SourceLoc {
    match self {
      ErlAst::Comment(loc) => *loc,
      ErlAst::Token { location: loc, .. } => *loc,
      ErlAst::ModuleAttr { location: loc, .. } => *loc,
      ErlAst::Comma { location: loc, .. } => *loc,
      ErlAst::NewFunction(loc, _) => *loc,
      // ErlAst::FClause(loc, _) => *loc,
      ErlAst::CClause(loc, _) => *loc,
      ErlAst::FunArity(loc, _) => *loc,
      ErlAst::Var(loc, _) => *loc,
      ErlAst::App(loc, _) => *loc,
      ErlAst::Let(loc, _) => *loc,
      ErlAst::Case(loc, _) => *loc,
      ErlAst::Lit(loc, _) => *loc,
      ErlAst::BinaryOp(loc, _) => *loc,
      ErlAst::UnaryOp(loc, _) => *loc,
      _ => SourceLoc::default(),
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

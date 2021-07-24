//! AST syntax structure of an Erlang file
use std::ops::Deref;
use std::rc::Rc;
use enum_as_inner::EnumAsInner;

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
#[derive(PartialEq, EnumAsInner)]
pub enum ErlAst {
  /// Default value for when AST tree is empty
  Empty,
  /// Comment text eliminated.
  Comment,

  /// A token to be consumed by AST builder, temporary, must not exist in final AST
  Token(ErlToken),

  /// Forms list, root of a module
  ModuleForms(Vec<Rc<ErlAst>>),

  /// -module(name). attribute, defines module start
  ModuleAttr {
    /// Module name atom, stored as string
    name: String
  },

  /// Comma expression receives type of its last AST element
  Comma {
    /// Left expression
    left: Rc<ErlAst>,
    /// Right expression
    right: Rc<ErlAst>,
    /// Type for right expression, also is the type of entire comma operator
    ty: ErlType,
  },

  /// Defines a new function, with clauses.
  /// Each clause has same quantity of args (some AST nodes), bindable expressions,
  /// and a return type, initially Any
  NewFunction(NewFunctionNode),

  /// Function clause for a new function definition
  FClause(FunctionClauseNode),

  /// Case clause for a `case x of` switch
  CClause(CaseClauseNode),

  /// A named variable
  Var(VarNode),

  /// Apply arguments to expression
  App(ApplicationNode),

  /// A haskell-style new variable introducing a new scope below it:
  /// let x = expr1 in expr2
  Let(LetExprNode),

  /// Case switch containing the argument to check, and case clauses
  Case(CaseExprNode),

  /// A literal value, constant. Type is known via literal.get_type()
  Lit(LiteralNode),

  /// Binary operation with two arguments
  BinaryOp(BinaryOperatorExprNode),

  /// Unary operation with 1 argument
  UnaryOp(UnaryOperatorExprNode),
}

impl ErlAst {
  /// Gets the type of an AST node
  pub fn get_type(&self) -> ErlType {
    match self {
      ErlAst::ModuleForms(_) => ErlType::Any,
      ErlAst::ModuleAttr { .. } => ErlType::Any,
      ErlAst::NewFunction(nf) => nf.ret.clone(),
      ErlAst::FClause(fc) => fc.body.get_type(),
      ErlAst::CClause(clause) => clause.body.get_type(),
      ErlAst::Var(v) => v.ty.clone(),
      ErlAst::App(app) => app.ret_type.clone(),
      ErlAst::Let(let_expr) => let_expr.in_expr.get_type(),
      ErlAst::Case(case) => case.ret.clone(),
      ErlAst::Lit(l) => l.get_type().clone(),
      ErlAst::BinaryOp(binop) => binop.operator.get_result_type(),
      ErlAst::UnaryOp(unop) => unop.expr.get_type(), // same type as expr bool or num
      ErlAst::Comma { right, .. } => {
        right.get_type()
      }
      _ => unreachable!("Can't process {:?}", self),
    }
  }

  /// Retrieve a name of a function node (first clause name)
  pub fn get_fun_name(&self) -> Option<&str> {
    match self {
      ErlAst::NewFunction(nf) => {
        assert!(nf.clauses.len() > 0, "get_fun_name: NewFunction must have more than 0 function clauses");
        Some(&nf.clauses[0].name)
      }
      ErlAst::FClause(fc) => Some(&fc.name),
      _ => None, // not a function
    }
  }

  /// Build a vec of references to children
  pub fn get_children(&self) -> Option<Vec<Rc<ErlAst>>> {
    match self {
      ErlAst::ModuleForms(f) => Some(f.clone()),
      ErlAst::ModuleAttr { .. } => None,
      ErlAst::Lit { .. } => None,
      ErlAst::Comment => None,
      ErlAst::NewFunction(nf) => {
        let all_clause_bodies = nf.clauses.iter()
            .map(|clause| clause.body.clone())
            .collect();
        Some(all_clause_bodies)
      }
      ErlAst::FClause(fc) => {
        // Descend into args, and the body
        let mut args_refs: Vec<Rc<ErlAst>> = fc.args.clone();
        args_refs.push(fc.body.clone());
        Some(args_refs)
      }
      ErlAst::Var { .. } => None,
      ErlAst::App(app) => {
        let mut r = vec![app.expr.clone()];
        app.args.iter().for_each(|a| r.push(a.clone()));
        Some(r)
      }
      ErlAst::Let(let_expr) => {
        Some(vec![let_expr.value.clone(), let_expr.in_expr.clone()])
      }
      ErlAst::Case(case) => {
        let mut r = vec![case.arg.clone()];
        case.clauses.iter().for_each(|a| r.push(a.clone()));
        Some(r)
      }
      ErlAst::CClause(clause) => {
        Some(vec![clause.cond.clone(),
                  clause.guard.clone(),
                  clause.body.clone()])
      }
      ErlAst::BinaryOp(binop) => {
        Some(vec![binop.left.clone(),
                  binop.right.clone()])
      }
      ErlAst::UnaryOp(unop) => Some(vec![unop.expr.clone()]),
      ErlAst::Comma { left, right, .. } => {
        Some(vec![left.clone(), right.clone()])
      }
      ErlAst::Token(_) => panic!("Token {:?} must be eliminated in AST build phase", self),

      _ => unreachable!("Can't process {:?}", self),
    }
  }

  /// Create a new function definition node
  pub fn new_fun(clauses: Vec<FunctionClauseNode>) -> Rc<Self> {
    assert_eq!(clauses.is_empty(), false, "Clauses must not be empty");

    let arity = clauses[0].arg_types.len();
    assert!(clauses.iter().all(|fc| fc.arg_types.len() == arity),
            "All clauses must have same arity");
    assert!(clauses.iter().all(|fc| fc.arg_types.len() == fc.args.len()),
            "All clause arg types must match in length all clauses' arguments");

    let nf = NewFunctionNode::new(arity, clauses, ErlType::new_typevar());
    Rc::new(ErlAst::NewFunction(nf))
  }

  /// Create a new variable AST node
  pub fn new_var(name: &str) -> Rc<ErlAst> {
    Rc::new(ErlAst::Var(VarNode::new(name)))
  }

  /// Creates a new AST node to perform a function call (application of args to a func expression)
  pub fn new_application(expr: Rc<ErlAst>, args: Vec<Rc<ErlAst>>) -> Rc<ErlAst> {
    Rc::new(ErlAst::App(ApplicationNode::new(expr, args)))
  }

  /// Creates a new AST node to perform a function call (application of 0 args to a func expression)
  pub fn new_application0(expr: Rc<ErlAst>) -> Rc<ErlAst> {
    Rc::new(ErlAst::App(ApplicationNode::new(expr, vec![])))
  }

  /// Create an new binary operation AST node with left and right operands AST
  pub fn new_binop(left: Rc<ErlAst>, op: ErlBinaryOp, right: Rc<ErlAst>) -> Rc<Self> {
    Rc::new(ErlAst::BinaryOp(BinaryOperatorExprNode {
      left,
      right,
      operator: op,
      ty: ErlType::new_typevar(),
    }))
  }

  /// Create a new literal AST node of an integer
  pub fn new_lit_int(val: isize) -> Rc<Self> {
    Rc::new(ErlAst::Lit(LiteralNode::Integer(val)))
  }

  /// Create a new literal AST node of an atom
  pub fn new_lit_atom(val: &str) -> Rc<Self> {
    Rc::new(ErlAst::Lit(LiteralNode::Atom(String::from(val))))
  }

  /// Create a new temporary token, which holds a place temporarily, it must be consumed in the
  /// same function and not exposed to the rest of the program.
  pub fn temporary_token(t: ErlToken) -> Rc<Self> {
    Rc::new(ErlAst::Token(t))
  }

  /// Create a new Comma operator from list of AST expressions
  pub fn new_comma(left: Rc<ErlAst>, right: Rc<ErlAst>) -> Rc<Self> {
    Rc::new(ErlAst::Comma {
      left,
      right,
      ty: ErlType::new_typevar(),
    })
  }

  /// Retrieve Some(atom text) if AST node is atom
  pub fn get_atom_text(&self) -> Option<String> {
    match self {
      ErlAst::Lit(LiteralNode::Atom(s)) => Some(s.clone()),
      _ => None,
    }
  }

  /// Retrieve Some(function clause name) if AST node is a function clause
  pub fn get_fclause_name(&self) -> Option<String> {
    match self {
      ErlAst::FClause(fc) => Some(fc.name.clone()),
      _ => None,
    }
  }

  /// Given a comma operator, unroll the comma nested tree into a vector of AST, this is used for
  /// function calls, where args are parsed as a single Comma{} and must be converted to a vec.
  /// A non-comma AST-node becomes a single result element.
  pub fn comma_to_vec(ast: &Rc<ErlAst>, dst: &mut Vec<Rc<ErlAst>>) {
    match ast.deref() {
      ErlAst::Comma { left, right, .. } => {
        Self::comma_to_vec(&left, dst);
        Self::comma_to_vec(&right, dst);
      }
      _ => dst.push(ast.clone()),
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

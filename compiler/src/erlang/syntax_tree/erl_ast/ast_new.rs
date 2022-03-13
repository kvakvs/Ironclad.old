//! Creation code for ErlAst

use std::sync::Arc;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::erl_op::ErlBinaryOp;
use crate::erlang::syntax_tree::node::erl_apply::ErlApply;
use crate::erlang::syntax_tree::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erlang::syntax_tree::node::erl_case_clause::ErlCaseClause;
use crate::erlang::syntax_tree::node::erl_catch_clause::CatchClause;
use crate::erlang::syntax_tree::node::erl_var::ErlVar;
use crate::literal::Literal;
use crate::mfarity::MFArity;
use crate::source_loc::SourceLoc;

impl ErlAst {
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

  /// Create a new AST node for a comma-expression
  pub fn new_comma_expr(location: SourceLoc, elements: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    ErlAst::CommaExpr { location, elements }.into()
  }

  /// Create a new AST node for a list comprehension
  pub fn new_list_comprehension(location: SourceLoc,
                                expr: Arc<ErlAst>,
                                generators: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    ErlAst::ListComprehension { location, expr, generators }.into()
  }

  /// Create a new AST node for a list comprehension generator `Expr <- Expr`
  pub fn new_list_comprehension_generator(location: SourceLoc,
                                          left: Arc<ErlAst>,
                                          right: Arc<ErlAst>) -> Arc<ErlAst> {
    ErlAst::ListComprehensionGenerator { location, left, right }.into()
  }

  /// Create a new `-module(m).` module attr.
  pub fn new_module_start_attr(name: String) -> Arc<ErlAst> {
    ErlAst::ModuleStartAttr {
      location: SourceLoc::None,
      name,
    }.into()
  }

  /// Create a new `-export([...]).` module attr.
  pub fn new_export_attr(exports: Vec<MFArity>) -> Arc<ErlAst> {
    ErlAst::ExportAttr {
      location: SourceLoc::None,
      exports,
    }.into()
  }

  /// Create a new `-import(modulename, [...]).` module attr.
  pub fn new_import_attr(import_from: String, imports: Vec<MFArity>) -> Arc<ErlAst> {
    ErlAst::ImportAttr {
      location: SourceLoc::None,
      import_from,
      imports,
    }.into()
  }

  /// Create a new try-catch AST node
  pub fn new_try_catch(location: SourceLoc, body: Arc<ErlAst>,
                       of_branches: Option<Vec<ErlCaseClause>>,
                       catch_clauses: Vec<CatchClause>) -> Arc<ErlAst> {
    ErlAst::TryCatch {
      location, body, of_branches, catch_clauses
    }.into()
  }
}
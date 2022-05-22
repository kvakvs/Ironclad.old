//! Creation code for ErlAst

use std::sync::Arc;
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::erl_op::ErlBinaryOp;
use crate::syntax_tree::node::erl_apply::{ErlApply};
use crate::syntax_tree::node::erl_binary_element::BinaryElement;
use crate::syntax_tree::node::erl_binop::ErlBinaryOperatorExpr;
use crate::syntax_tree::node::erl_callable_target::CallableTarget;
use crate::syntax_tree::node::erl_case_clause::ErlCaseClause;
use crate::syntax_tree::node::erl_catch_clause::CatchClause;
use crate::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::syntax_tree::node::erl_if_clause::ErlIfClause;
use crate::syntax_tree::node::erl_var::ErlVar;
use crate::literal::Literal;
use libironclad_util::mfarity::MFArity;
use libironclad_error::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;

impl ErlAst {
  /// Create a new variable AST node
  pub fn new_var(location: SourceLoc, name: &str) -> Arc<ErlAst> {
    ErlAst::Var(ErlVar::new(location, name))
        .into()
  }

  /// Creates a new AST node to perform a function call (application of args to a func expression)
  pub fn new_application(location: SourceLoc, target: CallableTarget, args: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    ErlAst::Apply(ErlApply::new(location, target, args))
        .into()
  }

  /// Creates a new AST node to perform a function call (application of 0 args to a func expression)
  pub fn new_application0(location: SourceLoc, target: CallableTarget) -> Arc<ErlAst> {
    ErlAst::Apply(ErlApply::new(location, target, Vec::default()))
        .into()
  }

  /// Create an new ironclad_exe operation AST node with left and right operands AST
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
  pub fn new_list(location: SourceLoc, elements: Vec<Arc<ErlAst>>, tail: Option<Arc<ErlAst>>) -> Arc<ErlAst> {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    ErlAst::List { location, elements, tail }
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
    match elements.len() {
      0 => panic!("Empty elements when creating a ErlAst::CommaExpr"),
      1 => elements[0].clone(),
      _ => ErlAst::CommaExpr { location, elements }.into()
    }
  }

  /// Create a new AST node for a list comprehension
  pub fn new_list_comprehension(location: SourceLoc,
                                expr: Arc<ErlAst>,
                                generators: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    ErlAst::ListComprehension { location, expr, generators }.into()
  }

  /// Create a new AST node for a function `-spec FN(ARG, ...) -> RETURN.`
  pub fn new_fn_spec(location: SourceLoc, funarity: MFArity, spec: Arc<ErlType>) -> Arc<ErlAst> {
    ErlAst::FnSpec { location, funarity, spec }.into()
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

  /// Create a new `-TAG(TERM).` generic module attribute.
  pub fn new_generic_attr(location: SourceLoc, tag: String, term: Option<Arc<ErlAst>>) -> Arc<ErlAst> {
    ErlAst::GenericAttr { location, tag, term }.into()
  }

  /// Create a new `-export([...]).` module attr.
  pub fn new_export_attr(exports: Vec<MFArity>) -> Arc<ErlAst> {
    ErlAst::ExportAttr {
      location: SourceLoc::None,
      exports,
    }.into()
  }

  /// Create a new `-export_type([...]).` module attr.
  pub fn new_export_type_attr(exports: Vec<MFArity>) -> Arc<ErlAst> {
    ErlAst::ExportTypeAttr {
      location: SourceLoc::None,
      exports,
    }.into()
  }

  /// Create a new `-type IDENT(ARG1, ...) :: TYPE.` module attr.
  pub fn new_type_attr(name: String, vars: Vec<String>, ty: Arc<ErlType>) -> Arc<ErlAst> {
    ErlAst::TypeAttr {
      location: SourceLoc::None,
      name,
      vars,
      ty,
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
      location,
      body,
      of_branches,
      catch_clauses,
    }.into()
  }

  /// Create a new `if` AST Node for `if COND -> EXPR; ... end`
  pub fn new_if_statement(location: SourceLoc, clauses: Vec<ErlIfClause>) -> Arc<ErlAst> {
    ErlAst::IfStatement { location, clauses }.into()
  }

  /// Create a new `case` AST Node for `case EXPR of MATCH -> EXPR; ... end`
  pub fn new_case_statement(location: SourceLoc, expr: Arc<ErlAst>, clauses: Vec<ErlCaseClause>) -> Arc<ErlAst> {
    ErlAst::CaseStatement { location, expr, clauses }.into()
  }

  /// Create a new function AST node, or a lambda AST node.
  pub fn new_fndef(location: SourceLoc, funarity: MFArity, clauses: Vec<ErlFnClause>) -> Arc<ErlAst> {
    let fndef = ErlFnDef {
      location,
      funarity,
      clauses,
    };
    ErlAst::FnDef(fndef).into()
  }

  /// Create a new ironclad_exe expression
  pub fn new_binary_expr(location: SourceLoc, elements: Vec<BinaryElement>) -> Arc<ErlAst> {
    Self::BinaryExpr {
      location,
      elements,
    }.into()
  }
}
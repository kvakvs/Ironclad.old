//! Creation code for ErlAst

use crate::erl_syntax::erl_ast::ErlAstType::{
  Apply, BinaryExpr, BinaryOp, CaseStatement, CommaExpr, Empty, ExportAttr, ExportTypeAttr, FnDef,
  FnSpec, GenericAttr, IfStatement, ImportAttr, List, ListComprehension,
  ListComprehensionGenerator, Lit, MapBuilder, ModuleStartAttr, TryCatch, Tuple, TypeAttr, Var,
};
use crate::erl_syntax::erl_ast::{ErlAst, ErlAstType};
use crate::erl_syntax::erl_op::ErlBinaryOp;
use crate::erl_syntax::node::erl_apply::ErlApply;
use crate::erl_syntax::node::erl_binary_element::BinaryElement;
use crate::erl_syntax::node::erl_binop::ErlBinaryOperatorExpr;
use crate::erl_syntax::node::erl_callable_target::CallableTarget;
use crate::erl_syntax::node::erl_case_clause::ErlCaseClause;
use crate::erl_syntax::node::erl_catch_clause::CatchClause;
use crate::erl_syntax::node::erl_fn_clause::ErlFnClause;
use crate::erl_syntax::node::erl_fn_def::ErlFnDef;
use crate::erl_syntax::node::erl_if_clause::ErlIfClause;
use crate::erl_syntax::node::erl_map::MapBuilderMember;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::node::erl_var::ErlVar;
use crate::literal::Literal;
use crate::typing::erl_integer::ErlInteger;
use crate::typing::erl_type::ErlType;
use libironclad_error::source_loc::SourceLoc;
use libironclad_util::mfarity::MFArity;
use std::sync::Arc;

impl ErlAst {
  /// Generic constructor no location
  #[inline]
  pub fn construct_without_location(node_type: ErlAstType) -> Arc<ErlAst> {
    ErlAst { location: SourceLoc::None, content: node_type }.into()
  }

  /// Generic constructor + location
  #[inline]
  pub fn construct_with_location(loc: &SourceLoc, node_type: ErlAstType) -> Arc<ErlAst> {
    ErlAst { location: loc.clone(), content: node_type }.into()
  }

  /// Construct an `Empty` node
  #[inline]
  pub fn new_empty() -> Arc<ErlAst> {
    Self::construct_without_location(Empty)
  }

  /// Create a new variable AST node
  pub fn new_var(location: &SourceLoc, name: &str) -> Arc<ErlAst> {
    Self::construct_with_location(location, Var(ErlVar::new(name)))
  }

  /// Creates a new AST node to perform a function call (application of args to a func expression)
  pub fn new_application(
    location: &SourceLoc,
    target: CallableTarget,
    args: Vec<Arc<ErlAst>>,
  ) -> Arc<ErlAst> {
    let apply = ErlApply::new(target, args);
    ErlAst::construct_with_location(location, Apply(apply))
  }

  /// Creates a new AST node to perform a function call (application of 0 args to a func expression)
  pub fn new_application0(location: &SourceLoc, target: CallableTarget) -> Arc<ErlAst> {
    let apply = ErlApply::new(target, Vec::default());
    ErlAst::construct_with_location(location, Apply(apply))
  }

  /// Create an new ironclad_exe operation AST node with left and right operands AST
  pub fn new_binop(
    location: &SourceLoc,
    left: Arc<ErlAst>,
    op: ErlBinaryOp,
    right: Arc<ErlAst>,
  ) -> Arc<ErlAst> {
    let binop_node = BinaryOp {
      expr: ErlBinaryOperatorExpr { left, right, operator: op },
    };
    ErlAst::construct_with_location(location, binop_node)
  }

  /// Create a new literal AST node of an integer
  pub fn new_lit_int(location: &SourceLoc, val: ErlInteger) -> Arc<ErlAst> {
    let lit_node = Lit { value: Literal::Integer(val).into() };
    ErlAst::construct_with_location(location, lit_node)
  }

  /// Create a new literal AST node of an atom
  pub fn new_lit_atom(location: &SourceLoc, val: &str) -> Arc<ErlAst> {
    let lit_node = Lit { value: Literal::Atom(String::from(val)).into() };
    ErlAst::construct_with_location(location, lit_node)
  }

  /// Create a new literal AST node of a floating point number
  pub fn new_lit_float(location: &SourceLoc, val: &str) -> Arc<ErlAst> {
    match String::from(val).trim().parse() {
      Ok(flt) => {
        let lit_node = Lit { value: Literal::Float(flt).into() };
        ErlAst::construct_with_location(location, lit_node)
      }
      Err(e) => {
        panic!("Failed to parse float from \"{}\": error {}", val, e)
      }
    }
  }

  /// Create a new literal AST node of a "string"
  pub fn new_lit_string(location: &SourceLoc, val: &str) -> Arc<ErlAst> {
    let lit_node = Lit { value: Literal::String(String::from(val)).into() };
    ErlAst::construct_with_location(location, lit_node)
  }

  /// Create a new AST node for a list of some expressions
  pub fn new_list(
    location: &SourceLoc,
    elements: Vec<Arc<ErlAst>>,
    tail: Option<Arc<ErlAst>>,
  ) -> Arc<ErlAst> {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    // Use Self::walk_litexpr
    ErlAst::construct_with_location(location, List { elements, tail })
  }

  /// Create a new AST node for a tuple of some expressions
  pub fn new_tuple(location: &SourceLoc, elements: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    // Use Self::walk_litexpr
    ErlAst::construct_with_location(location, Tuple { elements })
  }

  /// Create a new AST node for a map builder
  pub fn new_map_builder(location: &SourceLoc, members: Vec<MapBuilderMember>) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, MapBuilder { members })
  }

  /// Create a new AST node for a comma-expression
  pub fn new_comma_expr(location: &SourceLoc, elements: Vec<Arc<ErlAst>>) -> Arc<ErlAst> {
    match elements.len() {
      0 => panic!("Empty elements when creating a ErlAst::CommaExpr"),
      1 => elements[0].clone(),
      _ => ErlAst::construct_with_location(location, CommaExpr { elements }),
    }
  }

  /// Create a new AST node for a list comprehension
  pub fn new_list_comprehension(
    location: &SourceLoc,
    expr: Arc<ErlAst>,
    generators: Vec<Arc<ErlAst>>,
  ) -> Arc<ErlAst> {
    let lc_node = ListComprehension { expr, generators };
    ErlAst::construct_with_location(location, lc_node)
  }

  /// Create a new AST node for a function `-spec FN(ARG, ...) -> RETURN.`
  pub fn new_fn_spec(location: &SourceLoc, funarity: MFArity, spec: Arc<ErlType>) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, FnSpec { funarity, spec })
  }

  /// Create a new AST node for a list comprehension generator `Expr <- Expr`
  pub fn new_list_comprehension_generator(
    location: &SourceLoc,
    left: Arc<ErlAst>,
    right: Arc<ErlAst>,
  ) -> Arc<ErlAst> {
    let lc_node = ListComprehensionGenerator { left, right };
    ErlAst::construct_with_location(location, lc_node)
  }

  /// Create a new `-module(m).` module attr.
  pub fn new_module_start_attr(location: &SourceLoc, name: String) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, ModuleStartAttr { name })
  }

  /// Create a new `-TAG(TERM).` generic module attribute.
  pub fn new_generic_attr(
    location: &SourceLoc,
    tag: String,
    term: Option<Arc<ErlAst>>,
  ) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, GenericAttr { tag, term })
  }

  /// Create a new `-export([...]).` module attr.
  pub fn new_export_attr(location: &SourceLoc, exports: Vec<MFArity>) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, ExportAttr { exports })
  }

  /// Create a new `-export_type([...]).` module attr.
  pub fn new_export_type_attr(location: &SourceLoc, exports: Vec<MFArity>) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, ExportTypeAttr { exports })
  }

  /// Create a new `-type IDENT(ARG1, ...) :: TYPE.` module attr.
  pub fn new_type_attr(
    location: &SourceLoc,
    name: String,
    vars: Vec<String>,
    ty: Arc<ErlType>,
  ) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, TypeAttr { name, vars, ty })
  }

  /// Create a new `-import(modulename, [...]).` module attr.
  pub fn new_import_attr(
    location: &SourceLoc,
    import_from: String,
    imports: Vec<MFArity>,
  ) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, ImportAttr { import_from, imports })
  }

  /// Create a new try-catch AST node
  pub fn new_try_catch(
    location: &SourceLoc,
    body: Arc<ErlAst>,
    of_branches: Option<Vec<ErlCaseClause>>,
    catch_clauses: Vec<CatchClause>,
  ) -> Arc<ErlAst> {
    let trycatch_node = TryCatch { body, of_branches, catch_clauses };
    ErlAst::construct_with_location(location, trycatch_node)
  }

  /// Create a new `if` AST Node for `if COND -> EXPR; ... end`
  pub fn new_if_statement(location: &SourceLoc, clauses: Vec<ErlIfClause>) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, IfStatement { clauses })
  }

  /// Create a new `case` AST Node for `case EXPR of MATCH -> EXPR; ... end`
  pub fn new_case_statement(
    location: &SourceLoc,
    expr: Arc<ErlAst>,
    clauses: Vec<ErlCaseClause>,
  ) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, CaseStatement { expr, clauses })
  }

  /// Create a new function AST node, or a lambda AST node.
  pub fn new_fndef(
    location: &SourceLoc,
    funarity: MFArity,
    clauses: Vec<ErlFnClause>,
  ) -> Arc<ErlAst> {
    let fndef = ErlFnDef { location: location.clone(), funarity, clauses };
    ErlAst::construct_without_location(FnDef(fndef))
  }

  /// Create a new binary expression
  pub fn new_binary_expr(location: &SourceLoc, elements: Vec<BinaryElement>) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, BinaryExpr { elements })
  }

  /// Create a new record definition from a `-record(name, {fields...}).` attribute
  pub fn new_record_definition(
    location: &SourceLoc,
    tag: String,
    fields: Vec<RecordField>,
  ) -> Arc<ErlAst> {
    ErlAst::construct_with_location(location, ErlAstType::RecordDefinition { tag, fields })
  }
}

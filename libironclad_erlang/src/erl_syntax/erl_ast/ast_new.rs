//! Creation code for ErlAst

use crate::erl_syntax::erl_ast::node_impl::AstNodeType::{
  Apply, BeginEnd, BinaryComprehension, BinaryExpr, BinaryOp, CaseStatement, CommaExpr, Empty,
  FnDef, IfStatement, List, ListComprehension, ListComprehensionGenerator, Lit, MapBuilder,
  ModuleRoot, RecordBuilder, TryCatch, Tuple, Var,
};
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
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
use crate::erl_syntax::node::erl_record::RecordBuilderMember;
use crate::erl_syntax::node::erl_var::ErlVar;
use crate::literal::Literal;
use crate::source_loc::SourceLoc;
use crate::typing::erl_integer::ErlInteger;
use libironclad_util::mfarity::MFArity;

impl AstNodeImpl {
  /// Generic constructor no location
  #[inline]
  pub(crate) fn construct_without_location(node_type: AstNodeType) -> AstNode {
    AstNodeImpl { location: SourceLoc::None, content: node_type }.into()
  }

  /// Generic constructor + location
  #[inline]
  pub(crate) fn construct_with_location(loc: SourceLoc, node_type: AstNodeType) -> AstNode {
    AstNodeImpl { location: loc, content: node_type }.into()
  }

  /// Construct an `Empty` node
  #[inline]
  pub fn new_empty(comment: String) -> AstNode {
    Self::construct_without_location(Empty { comment })
  }

  /// Create a new variable AST node
  pub(crate) fn new_var(location: SourceLoc, name: &str) -> AstNode {
    Self::construct_with_location(location, Var(ErlVar::new(name)))
  }

  /// Creates a new AST node to perform a function call (application of args to a func expression)
  pub(crate) fn new_application(
    location: SourceLoc,
    target: CallableTarget,
    args: Vec<AstNode>,
  ) -> AstNode {
    let apply = ErlApply::new(target, args);
    AstNodeImpl::construct_with_location(location, Apply(apply))
  }

  /// Creates a new AST node to perform a function call (application of 0 args to a func expression)
  #[allow(dead_code)]
  pub(crate) fn new_application0(location: SourceLoc, target: CallableTarget) -> AstNode {
    let apply = ErlApply::new(target, Vec::default());
    AstNodeImpl::construct_with_location(location, Apply(apply))
  }

  /// Create an new binary operation AST node with left and right operands AST
  #[allow(dead_code)]
  pub(crate) fn new_binop(
    location: SourceLoc,
    left: AstNode,
    op: ErlBinaryOp,
    right: AstNode,
  ) -> AstNode {
    let binop_node = BinaryOp {
      expr: ErlBinaryOperatorExpr { left, right, operator: op },
    };
    AstNodeImpl::construct_with_location(location, binop_node)
  }

  /// Create a new literal AST node of an integer
  #[allow(dead_code)]
  pub(crate) fn new_lit_int(location: SourceLoc, val: ErlInteger) -> AstNode {
    let lit_node = Lit { value: Literal::Integer(val).into() };
    AstNodeImpl::construct_with_location(location, lit_node)
  }

  /// Create a new literal AST node of an atom
  #[allow(dead_code)]
  pub(crate) fn new_lit_atom(location: SourceLoc, val: &str) -> AstNode {
    let lit_node = Lit { value: Literal::Atom(String::from(val)).into() };
    AstNodeImpl::construct_with_location(location, lit_node)
  }

  /// Create a new literal AST node of a floating point number
  #[allow(dead_code)]
  pub(crate) fn new_lit_float(location: SourceLoc, val: &str) -> AstNode {
    match String::from(val).trim().parse() {
      Ok(flt) => {
        let lit_node = Lit { value: Literal::Float(flt).into() };
        AstNodeImpl::construct_with_location(location, lit_node)
      }
      Err(e) => {
        panic!("Failed to parse float from \"{}\": error {}", val, e)
      }
    }
  }

  /// Create a new literal AST node of a "string"
  #[allow(dead_code)]
  pub(crate) fn new_lit_string(location: SourceLoc, val: &str) -> AstNode {
    let lit_node = Lit { value: Literal::new_string(val).into() };
    AstNodeImpl::construct_with_location(location, lit_node)
  }

  /// Create a new AST node for a list of some expressions
  pub(crate) fn new_list(
    location: SourceLoc,
    elements: Vec<AstNode>,
    tail: Option<AstNode>,
  ) -> AstNode {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    // Use Self::walk_litexpr
    AstNodeImpl::construct_with_location(location, List { elements, tail })
  }

  /// Create a new AST node for a tuple of some expressions
  pub(crate) fn new_tuple(location: SourceLoc, elements: Vec<AstNode>) -> AstNode {
    // TODO: Constant folding, detect list to be a literal list and fold it into a literal node
    // Use Self::walk_litexpr
    AstNodeImpl::construct_with_location(location, Tuple { elements })
  }

  /// Create a new AST node for a map builder
  pub(crate) fn new_map_builder(location: SourceLoc, members: Vec<MapBuilderMember>) -> AstNode {
    AstNodeImpl::construct_with_location(location, MapBuilder { members })
  }

  /// Create a new AST node for a record builder
  pub(crate) fn new_record_builder(
    location: SourceLoc,
    base: Option<String>,
    tag: String,
    members: Vec<RecordBuilderMember>,
  ) -> AstNode {
    AstNodeImpl::construct_with_location(location, RecordBuilder { base, tag, members })
  }

  /// Create a new AST node for a comma-expression
  pub(crate) fn new_comma_expr(location: SourceLoc, elements: Vec<AstNode>) -> AstNode {
    match elements.len() {
      0 => panic!("Empty elements when creating a ErlAst::CommaExpr"),
      1 => elements[0].clone(),
      _ => AstNodeImpl::construct_with_location(location, CommaExpr { elements }),
    }
  }

  /// Create a new AST node for a list comprehension
  pub(crate) fn new_list_comprehension(
    location: SourceLoc,
    expr: AstNode,
    generators: Vec<AstNode>,
  ) -> AstNode {
    let lc_node = ListComprehension { expr, generators };
    AstNodeImpl::construct_with_location(location, lc_node)
  }

  /// Create a new AST node for a list comprehension
  pub(crate) fn new_binary_comprehension(
    location: SourceLoc,
    expr: AstNode,
    generators: Vec<AstNode>,
  ) -> AstNode {
    let lc_node = BinaryComprehension { expr, generators };
    AstNodeImpl::construct_with_location(location, lc_node)
  }

  /// Create a new AST node for a list comprehension generator `Expr <- Expr`
  pub(crate) fn new_list_comprehension_generator(
    location: SourceLoc,
    left: AstNode,
    right: AstNode,
  ) -> AstNode {
    let lc_node = ListComprehensionGenerator { left, right };
    AstNodeImpl::construct_with_location(location, lc_node)
  }

  // /// Takes preprocessor group nodes and unfolds them into flat list
  // fn flatten_forms(forms: Vec<AstNode>) -> Vec<AstNode> {
  //   let mut result = Vec::new();
  //   forms.into_iter().for_each(|n| {
  //     if let AstNodeType::Preprocessor(_) = &n.content {
  //       result.extend(n.as_preprocessor_group().iter().cloned())
  //     } else {
  //       result.push(n)
  //     }
  //   });
  //   result
  // }

  /// Create a new `-module(m).` module attr.
  pub(crate) fn new_module_forms(forms: Vec<AstNode>) -> AstNode {
    AstNodeImpl::construct_without_location(ModuleRoot { forms })
  }

  /// Create a new try-catch AST node
  pub(crate) fn new_try_catch(
    location: SourceLoc,
    body: AstNode,
    of_branches: Option<Vec<ErlCaseClause>>,
    catch_clauses: Vec<CatchClause>,
  ) -> AstNode {
    let trycatch_node = TryCatch { body, of_branches, catch_clauses };
    AstNodeImpl::construct_with_location(location, trycatch_node)
  }

  /// Create a new `if` AST Node for `if COND -> EXPR; ... end`
  pub(crate) fn new_if_statement(location: SourceLoc, clauses: Vec<ErlIfClause>) -> AstNode {
    AstNodeImpl::construct_with_location(location, IfStatement { clauses })
  }

  /// Create a new `BeginEnd` block
  pub(crate) fn new_begin_end(location: SourceLoc, exprs: Vec<AstNode>) -> AstNode {
    AstNodeImpl::construct_with_location(location, BeginEnd { exprs })
  }

  /// Create a new `case` AST Node for `case EXPR of MATCH -> EXPR; ... end`
  pub(crate) fn new_case_statement(
    location: SourceLoc,
    expr: AstNode,
    clauses: Vec<ErlCaseClause>,
  ) -> AstNode {
    AstNodeImpl::construct_with_location(location, CaseStatement { expr, clauses })
  }

  /// Create a new function AST node, or a lambda AST node.
  pub(crate) fn new_fndef(
    location: SourceLoc,
    funarity: MFArity,
    clauses: Vec<ErlFnClause>,
  ) -> AstNode {
    let fndef = ErlFnDef { location, funarity, clauses };
    AstNodeImpl::construct_without_location(FnDef(fndef))
  }

  /// Create a new binary expression
  pub(crate) fn new_binary_expr(location: SourceLoc, elements: Vec<BinaryElement>) -> AstNode {
    AstNodeImpl::construct_with_location(location, BinaryExpr { elements })
  }
}

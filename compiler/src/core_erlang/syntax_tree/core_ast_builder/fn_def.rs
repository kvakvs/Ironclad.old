//! Contains code to build function definitions wrapped with case clauses for matching the arguments
//! and Erlang function clauses.
use function_name::named;
use std::sync::Arc;
use std::iter;

use crate::core_erlang::syntax_tree::core_ast_builder::CoreAstBuilder;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::case::{Case, CaseClause};
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use std::ops::Deref;
use crate::core_erlang::syntax_tree::node::fn_def::FnDef;
use crate::typing::typevar::TypeVar;

// Conversion of Erlang function with clauses into Core function with case switch.
impl CoreAstBuilder {
  /// A scenario with single fn clause, analyze the variables in the clause, and optionally create
  /// a case switch for the args. Otherwise return the converted body without the wrapping.
  #[named]
  fn create_fnbody_from_single_fnclause(erl_fn_def: &ErlFnDef,
                                        erl_clause: &ErlFnClause) -> Arc<CoreAst> {
    let all_variables_args = erl_clause.args.iter().all(|a| a.is_var());

    if all_variables_args {
      // If all arguments are variables, we just return the code, no case wrapping

      // AST if the values match the expected patterns. Otherwise will raise a badarg.
      let good_ast = Self::build(&erl_clause.body);
      let cc_good = CaseClause {
        match_exprs: erl_clause.args.iter().map(Self::build).collect(),
        match_expr_types: vec![],
        guard: None,
        guard_ty: TypeVar::default(),
        body: good_ast.into(),
        ret_ty: Default::default(),
      };
      let cc_bad = CaseClause {
        match_exprs: iter::repeat(erl_clause.args.len())
            .map(|_| CoreAst::new_unique_var("fnarg"))
            .map(Arc::new)
            .collect(),
        match_expr_types: vec![],
        guard: None,
        guard_ty: TypeVar::new(),
        body: CoreAst::create_badarg_primop(erl_fn_def.location.clone()).into(),
        ret_ty: TypeVar::new(),
      };

      let case = Case {
        location: erl_fn_def.location.clone(),
        exprs: erl_clause.args.iter().map(Self::build).collect(),
        clauses: vec![cc_good, cc_bad],
        ret_ty: TypeVar::new(),
      };

      CoreAst::Case(case).into()
    } else {
      Self::build(&erl_clause.body)
    }
  }

  /// Given a collection of function clauses, create a case switch for them, mapping arg expressions
  /// from each clause into Core case clauses.
  #[named]
  fn create_fnbody_from_fnclauses(erl_fn_def: &ErlFnDef) -> Arc<CoreAst> {
    assert!(erl_fn_def.clauses.len() > 1,
            "{}: Zero clause function is not acceptable", function_name!());
    if erl_fn_def.clauses.len() == 1 {
      Self::create_fnbody_from_single_fnclause(erl_fn_def, &erl_fn_def.clauses[0])
    } else {
      let case = Case {
        location: erl_fn_def.location.clone(),
        exprs: vec![],
        clauses: vec![],
        ret_ty: Default::default(),
      };
      CoreAst::Case(case).into()
    }
  }

  /// Given a FnDef, produce a CoreAst equivalent new function definition with an optional nested
  /// case for multiple clauses
  #[named]
  pub(crate) fn create_from_fndef(ast: &Arc<ErlAst>) -> Arc<CoreAst> {
    if let ErlAst::FnDef(fn_def) = ast.deref() {
      // Based on how many function clauses are there, we might inject an additional case operator
      // matching function args for all clauses
      let core_body = Self::create_fnbody_from_fnclauses(&fn_def);
      let core_fndef = FnDef {
        location: fn_def.location.clone(),
        funarity: fn_def.funarity.clone(),
        args: iter::repeat(fn_def.funarity.arity)
            .map(|_| TypeVar::new())
            .collect(),
        body: core_body.into(),
        ret_ty: TypeVar::new(),
      };
      return CoreAst::FnDef(core_fndef).into();
    }

    panic!("{}: Not a ErlAst::FnDef - got {}", function_name!(), ast)
  }
}
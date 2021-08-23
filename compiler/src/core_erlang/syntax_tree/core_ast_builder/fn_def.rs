//! Contains code to build function definitions wrapped with case clauses for matching the arguments
//! and Erlang function clauses.
use function_name::named;
use std::sync::Arc;
use std::iter;

use crate::core_erlang::syntax_tree::core_ast_builder::CoreAstBuilder;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::case::Case;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use std::ops::Deref;
use crate::core_erlang::syntax_tree::node::fn_def::FnDef;
use crate::typing::typevar::TypeVar;
use crate::source_loc::SourceLoc;

impl CoreAstBuilder {
  /// Conversion of Erlang function with clauses into Core function with case switch.
  /// A scenario with single fn clause, analyze the variables in the clause, and optionally create
  /// a case switch for the args.
  #[named]
  fn create_case_from_single_fn_clause(erl_fn_def: &ErlFnDef,
                                       erl_clause: &ErlFnClause) -> Arc<CoreAst> {
    let all_variables_args = erl_clause.args.iter().all(|a| a.is_var());

    if all_variables_args {
      // If all arguments are variables, we just return the code, no case wrapping
      let ast = Self::build(erl_clause.body.clone());
      let case = Case {
        location: erl_fn_def.location.clone(),
        exprs: erl_clause.args.iter().cloned().map(Self::build).collect(),
        clauses: vec![],
        ret_ty: Default::default(),
      };
      CoreAst::Case(case).into()
    } else {
      Self::build(erl_clause.body.clone())
    }
  }

  /// Conversion of Erlang function with clauses into Core function with case switch.
  /// Given a collection of function clauses, create a case switch for them, mapping arg expressions
  /// from each clause into Core case clauses.
  #[named]
  fn create_case_from_fn_clauses(erl_fn_def: &ErlFnDef) -> Arc<CoreAst> {
    assert!(erl_fn_def.clauses.len() > 1,
            "{}: Zero clause function is not acceptable", function_name!());
    if erl_fn_def.clauses.len() == 1 {
      Self::create_case_from_single_fn_clause(erl_fn_def, &erl_fn_def.clauses[0])
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

  /// Conversion of Erlang function with clauses into Core function with case switch.
  /// Given a FnDef, produce a CoreAst equivalent new function definition with an optional nested
  /// case for multiple clauses
  #[named]
  pub(crate) fn create_from_fndef(ast: Arc<ErlAst>) -> Arc<CoreAst> {
    if let ErlAst::FnDef(fn_def) = ast.deref() {
      // Based on how many function clauses are there, we might inject an additional case operator
      // matching function args for all clauses
      let core_body = Self::create_case_from_fn_clauses(&fn_def);
      let core_fndef = FnDef {
        location: fn_def.location.clone(),
        funarity: fn_def.funarity.clone(),
        args: iter::repeat(fn_def.funarity.arity)
            .map(|_| TypeVar::new())
            .collect(),
        body: core_body.into(),
        ret_ty: fn_def.ret_ty,
      };
      // return CoreAst::FnDef { location, core_fn_def }
    }

    panic!("{}: Not a ErlAst::FnDef - got {}", function_name!(), ast)
  }
}
//! Contains code to build function definitions wrapped with case clauses for matching the arguments
//! and Erlang function clauses.
use std::iter;
use std::ops::Deref;
use std::sync::{Arc};

use function_name::named;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::core_ast_builder::CoreAstBuilder;
use crate::core_erlang::syntax_tree::node::case::Case;
use crate::core_erlang::syntax_tree::node::case_clause::CaseClause;
use crate::core_erlang::syntax_tree::node::core_fn_clause::CoreFnClause;
use crate::core_erlang::syntax_tree::node::fn_def::FnDef;
use crate::erl_error::ErlResult;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::source_loc::SourceLoc;
use crate::project::module::Module;
use crate::typing::scope::Scope;

// Conversion of Erlang function with clauses into Core function with case switch.
impl CoreAstBuilder {
  /// Function body transformed into Core AST
  /// For 0 argument functions without guard and 1 clause is just the body expr
  /// For functions with guards or pattern matching, is a case expr
  #[named]
  fn create_fnbody(env: &Module, erl_fndef: &ErlFnDef) -> ErlResult<Arc<CoreAst>> {
    // TODO: A function without patterns and without guards should not generate a case
    if erl_fndef.funarity.arity == 0
        && erl_fndef.clauses.len() == 1
        && erl_fndef.clauses[0].guard_expr.is_none() {
      Self::build(env, &erl_fndef.clauses[0].body)
    } else {
      Self::create_fnbody_from_multiple_fnclauses(env, erl_fndef)
    }
  }

  /// A scenario with multiple fn clauses, analyze the variables in each clause.
  // TODO: Check the situation with "clause X covers remaining inputs"
  // TODO: Check the situation with "Not all inputs were covered"
  #[named]
  fn create_fnbody_from_multiple_fnclauses(env: &Module,
                                           erl_fndef: &ErlFnDef) -> ErlResult<Arc<CoreAst>> {
    let arity = erl_fndef.funarity.arity;
    let clauses_r: ErlResult<Vec<CaseClause>> = erl_fndef.clauses.iter()
        .map(|each_clause| {
          // AST if the values match the expected patterns. Otherwise will raise a badarg.
          assert_eq!(each_clause.args.len(), arity,
                     "Arity for all clauses must match function arity: {:?}", erl_fndef);
          let good_ast = Self::build(env, &each_clause.body)?;

          // Good clause, when argument values matched the pattern
          Self::create_case_clause_for_fnclause(env, arity, each_clause, good_ast)
        })
        .collect();
    let mut clauses = clauses_r?;

    // Bad clause, for failed arguments match
    let cc_bad = Self::create_case_badarg_clause(
      arity,
      erl_fndef.location.clone(),
      CoreAst::create_caseclause_primop(erl_fndef.location.clone()).into(),
    );
    clauses.push(cc_bad); // case clause

    // TODO: Arity 0 does not require a case!
    let case = Case::new(
      erl_fndef.location.clone(),
      iter::repeat(()).take(arity)
          .map(|_| CoreAst::new_unique_var("Fndef"))
          .collect(),
      clauses,
    );

    Ok(CoreAst::Case(case).into())
  }

  /// Creates a case clause for a certain ErlFnClause
  fn create_case_clause_for_fnclause(env: &Module,
                                     _arity: usize, clause: &ErlFnClause,
                                     ast: Arc<CoreAst>) -> ErlResult<CaseClause> {
    // let good_types = iter::repeat(())
    //     .take(arity)
    //     .map(|_| TypeVar::new())
    //     .collect();

    // Build a set of argument match expressions from args
    let good_arg_exprs: ErlResult<_> = clause.args.iter()
        .map(|each_arg| Self::build(env, each_arg))
        .collect();

    let clause = CaseClause::new(ast.location(), good_arg_exprs?, ast);
    Ok(clause)
  }

  /// Creates a case clause matching on anything, to guard vs "function clause" or "case clause"
  fn create_case_badarg_clause(arity: usize, loc: SourceLoc, body: Arc<CoreAst>) -> CaseClause {
    // let bad_types = iter::repeat(())
    //     .take(arity)
    //     .map(|_| TypeVar::new())
    //     .collect();
    let bad_arg_exprs = iter::repeat(())
        .take(arity)
        .map(|_| CoreAst::new_unique_var("Arg"))
        .collect();
    CaseClause::new(loc, bad_arg_exprs, body)
  }

  /// Directly translate args and body to core
  fn create_core_fnclause(module: &Module, efnc: &ErlFnClause) -> ErlResult<CoreFnClause> {
    let fn_header_scope = Scope::empty(Arc::downgrade(&module.scope)).into_arc_rwlock();

    let args_as_core: ErlResult<_> = efnc.args.iter()
        .map(|efnc_ast| CoreAstBuilder::build(module, efnc_ast))
        .collect();
    let guard_as_core = efnc.guard_expr.as_ref()
        .map(|ast| CoreAstBuilder::build(module, &ast));

    let fnclause = CoreFnClause::new(
      &fn_header_scope,
      args_as_core?,
      CoreAstBuilder::build(module, &efnc.body)?,
      guard_as_core.map(|r| r.unwrap()),
    );
    Ok(fnclause)
  }

  /// Given a FnDef, produce a CoreAst equivalent new function definition with an optional nested
  /// case for multiple clauses
  #[named]
  pub fn core_from_fndef(module: &Module, ast: &Arc<ErlAst>) -> ErlResult<Arc<CoreAst>> {
    if let ErlAst::FnDef(erl_fndef) = ast.deref() {
      let clauses: ErlResult<_> = erl_fndef.clauses.iter()
          .map(|efnc| Self::create_core_fnclause(module, efnc))
          .collect();
      let core_fndef: Arc<FnDef> = FnDef::new(erl_fndef.location.clone(),
                                              erl_fndef.funarity.clone(),
                                              clauses?).into();

      if let Ok(mut registry) = module.registry.write() {
        registry.add_function(core_fndef.clone());
        Scope::add_function(&module.scope,
                            &core_fndef.funarity,
                            core_fndef.synthesize_function_type(&module.scope)?);
      }
      return Ok(CoreAst::FnDef(core_fndef).into());
    }

    panic!("{}: Not a ErlAst::FnDef - got {:?}", function_name!(), ast)
  }
}
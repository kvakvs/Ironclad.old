//! Contains code to build function definitions wrapped with case clauses for matching the arguments
//! and Erlang function clauses.
use std::iter;
use std::ops::Deref;
use std::sync::Arc;

use function_name::named;

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::core_ast_builder::CoreAstBuilder;
use crate::core_erlang::syntax_tree::node::case::Case;
use crate::core_erlang::syntax_tree::node::case_clause::CaseClause;
use crate::core_erlang::syntax_tree::node::fn_def::FnDef;
use crate::erlang::syntax_tree::erl_ast::ErlAst;
use crate::erlang::syntax_tree::node::erl_fn_clause::ErlFnClause;
use crate::erlang::syntax_tree::node::erl_fn_def::ErlFnDef;
use crate::source_loc::SourceLoc;
use crate::typing::typevar::TypeVar;
use crate::project::module::Module;

// Conversion of Erlang function with clauses into Core function with case switch.
impl CoreAstBuilder {
  // /// A scenario with single fn clause, analyze the variables in the clause, and optionally create
  // /// a case switch for the args. Otherwise return the converted body without the wrapping.
  // #[named]
  // fn create_fnbody_from_single_fnclause(erl_fndef: &ErlFnDef,
  //                                       erl_clause: &ErlFnClause) -> Arc<CoreAst> {
  //   if erl_clause.is_all_variable_args() {
  //     // Simple approach, we do not need a case expr
  //     Self::build(&erl_clause.body)
  //   } else {
  //     // If all arguments are variables, we just return the code, no case wrapping
  //
  //     // AST if the values match the expected patterns. Otherwise will raise a badarg.
  //     let arity = erl_clause.args.len();
  //     let good_ast = Self::build(&erl_clause.body);
  //     // Good clause, when argument values matched the pattern
  //     let cc_good = Self::create_case_clause_for_fnclause(arity, erl_clause, good_ast);
  //
  //     // Bad clause, for failed arguments match
  //     let cc_bad = Self::create_case_badarg_clause(
  //       arity,
  //       erl_fndef.location.clone(),
  //       CoreAst::create_badarg_primop(erl_fndef.location.clone()).into()
  //     );
  //
  //     let case = Case {
  //       location: erl_fndef.location.clone(),
  //       exprs: erl_clause.args.iter().map(Self::build).collect(),
  //       clauses: vec![cc_good, cc_bad],
  //       ret_ty: TypeVar::new(),
  //     };
  //
  //     CoreAst::Case(case).into()
  //   }
  // }

  /// A scenario with multiple fn clauses, analyze the variables in each clause.
  // TODO: Check the situation with "clause X covers remaining inputs"
  // TODO: Check the situation with "Not all inputs were covered"
  #[named]
  fn create_fnbody_from_multiple_fnclauses(env: &Module, erl_fndef: &ErlFnDef) -> Arc<CoreAst> {
    let arity = erl_fndef.funarity.arity;
    let mut clauses: Vec<CaseClause> = erl_fndef.clauses.iter()
        .map(|each_clause| {
          // AST if the values match the expected patterns. Otherwise will raise a badarg.
          assert_eq!(each_clause.args.len(), arity,
                     "Arity for all clauses must match function arity: {:?}", erl_fndef);
          let good_ast = Self::build(env, &each_clause.body);

          // Good clause, when argument values matched the pattern
          Self::create_case_clause_for_fnclause(env, arity, each_clause, good_ast)
        })
        .collect();

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

    CoreAst::Case(case).into()
  }

  /// Creates a case clause for a certain ErlFnClause
  fn create_case_clause_for_fnclause(env: &Module,
                                     arity: usize, clause: &ErlFnClause,
                                     ast: Arc<CoreAst>) -> CaseClause {
    let good_types = iter::repeat(())
        .take(arity)
        .map(|_| TypeVar::new())
        .collect();

    // Build a set of argument match expressions from args
    let good_arg_exprs = clause.args.iter()
        .map(|each_arg| Self::build(env, each_arg))
        .collect();

    CaseClause::new(ast.location(), good_arg_exprs, good_types, ast)
  }

  /// Creates a case clause matching on anything, to guard vs "function clause" or "case clause"
  fn create_case_badarg_clause(arity: usize, loc: SourceLoc, body: Arc<CoreAst>) -> CaseClause {
    let bad_types = iter::repeat(())
        .take(arity)
        .map(|_| TypeVar::new())
        .collect();
    let bad_arg_exprs = iter::repeat(())
        .take(arity)
        .map(|_| CoreAst::new_unique_var("Arg"))
        .collect();
    CaseClause::new(loc, bad_arg_exprs, bad_types, body)
  }

  // /// Given a collection of function clauses, create a case switch for them, mapping arg expressions
  // /// from each clause into Core case clauses.
  // #[named]
  // fn create_fnbody_from_fnclauses(erl_fndef: &ErlFnDef) -> Arc<CoreAst> {
  //   assert!(erl_fndef.clauses.len() > 0,
  //           "{}: Zero clause function is not acceptable", function_name!());
  //   // if erl_fndef.clauses.len() == 1 {
  //   //   Self::create_fnbody_from_single_fnclause(erl_fndef, &erl_fndef.clauses[0])
  //   // } else {
  //   Self::create_fnbody_from_multiple_fnclauses(erl_fndef)
  //   // }
  // }

  /// Given a FnDef, produce a CoreAst equivalent new function definition with an optional nested
  /// case for multiple clauses
  #[named]
  pub(crate) fn create_from_fndef(env: &Module, ast: &Arc<ErlAst>) -> Arc<CoreAst> {
    if let ErlAst::FnDef(fn_def) = ast.deref() {
      // Based on how many function clauses are there, we might inject an additional case operator
      // matching function args for all clauses
      let core_body = Self::create_fnbody_from_multiple_fnclauses(env, fn_def);
      let core_fndef = Arc::new(FnDef {
        location: fn_def.location.clone(),
        funarity: fn_def.funarity.clone(),
        args: iter::repeat(())
            .take(fn_def.funarity.arity)
            .map(|_| TypeVar::new())
            .collect(),
        body: core_body,
        ret_ty: TypeVar::new(),
      });

      if let Ok(mut registry) = env.registry.write() {
        registry.add_function(core_fndef.clone());
      }
      return CoreAst::FnDef(core_fndef).into();
    }

    panic!("{}: Not a ErlAst::FnDef - got {:?}", function_name!(), ast)
  }
}
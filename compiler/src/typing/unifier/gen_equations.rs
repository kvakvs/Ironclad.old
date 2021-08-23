//! Contains code for generating type equations from AST in `Unifier` impl
use std::ops::Deref;
use function_name::named;

use crate::typing::unifier::Unifier;
use crate::typing::unifier::equation::TypeEquation;
use crate::typing::erl_type::ErlType;
use crate::erl_error::ErlResult;
use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::node::fn_def::FnDef;
use crate::core_erlang::syntax_tree::node::apply::Apply;
use crate::project::module::Module;
use std::sync::Arc;
use crate::core_erlang::syntax_tree::node::prim_op::PrimOp;

impl Unifier {
  /// Add a type equation, shortcut
  fn equation(eq: &mut Vec<TypeEquation>, ast: &CoreAst,
              type_deduced: &Arc<ErlType>,
              type_expected: &Arc<ErlType>) {
    eq.push(
      TypeEquation::new(ast.location(),
                        type_deduced.clone(),
                        type_expected.clone(),
                        format!("{:?}", ast))
    )
  }

  /// Type inference wiring
  /// Generate type equations from node. Each type variable is opposed to some type which we know, or
  /// to Any, if we don't know.
  #[named]
  pub fn generate_equations(&self, module: &Module,
                            eq: &mut Vec<TypeEquation>, ast: &CoreAst) -> ErlResult<()> {
    // Recursively descend into AST and visit deepest nodes first
    if let Some(children) = ast.children() {
      for child in children {
        let gen_result = self.generate_equations(module, eq, child.deref());
        match gen_result {
          Ok(_) => {} // nothing, all good
          Err(err) => { module.add_error(err); }
        }
      }
    }

    match ast.deref() {
      CoreAst::Module { .. } => {} // module root creates no equations
      CoreAst::Attributes { .. } => {}
      CoreAst::Lit { value: _value, ty: _ty, .. } => {
        // Literals create no equations, their type is known
        // Self::equation(eq, ast, ty.clone(), value.get_type());
      }
      CoreAst::Var { .. } => {}
      CoreAst::FnDef(fn_def) => {
        self.generate_equations_fn_def(eq, ast, fn_def)?;
        // no clauses, functions are single clause, using `case` to branch
        // for fc in &fn_def.clauses {
        //   self.generate_equations_fn_clause(eq, ast, &fc)?
        // }
      }
      CoreAst::Apply(app) => {
        self.generate_equations_apply(eq, ast, app)?
      }
      CoreAst::Let(letexpr) => {
        Self::equation(eq, ast,
                       &ErlType::TVar(letexpr.ret_ty).into(),
                       &letexpr.in_expr.get_type());
      }
      CoreAst::Case(case) => {
        // For Case expression, type of case must be union of all clause types
        let all_clause_types = case.clauses.iter()
            .map(|c| c.body.get_type())
            .collect();
        let all_clauses_t = ErlType::union_of(all_clause_types, true);

        for clause in case.clauses.iter() {
          // Clause type must match body type
          Self::equation(eq, ast, &ErlType::TVar(clause.ret_ty).into(),
                         &clause.body.get_type());

          // No check for clause condition, but the clause condition guard must be boolean
          if let Some(guard) = &clause.guard {
            Self::equation(eq, ast, &guard.get_type(),
                           &ErlType::AnyBool.into());
          }
        }

        Self::equation(eq, ast, &ErlType::TVar(case.ret_ty).into(),
                       &&all_clauses_t);
      }
      CoreAst::BinOp { op: binop, .. } => {
        // Check result of the binary operation
        Self::equation(eq, ast, &ErlType::TVar(binop.ty).into(),
                       &binop.get_result_type());

        if let Some(arg_type) = binop.get_arg_type() {
          // Both sides of a binary op must have type appropriate for that op
          Self::equation(eq, ast, &binop.left.get_type(), &arg_type);
          Self::equation(eq, ast, &binop.right.get_type(), &arg_type);
        }
      }
      CoreAst::UnOp { op: unop, .. } => {
        // Equation of expression type must match either bool for logical negation,
        // or (int|float) for numerical negation
        Self::equation(eq, ast, &unop.expr.get_type(), &unop.get_type());
        // TODO: Match return type with inferred return typevar?
      }
      CoreAst::List { .. } => {}
      CoreAst::Tuple { .. } => {}
      CoreAst::PrimOp { op, .. } => {
        if let PrimOp::Raise { .. } = op {} else {
          panic!("{}: Don't know how to process PrimOp {:?}", function_name!(), ast)
        }
      } // Any value can be raised, no check

      CoreAst::Empty => panic!("{}: Called on empty AST", function_name!()),
      _ => unreachable!("{}: Can't process {}", function_name!(), ast),
    }
    Ok(())
  }


  /// Type inference wiring
  /// Generate type equations for AST node FunctionDef
  #[named]
  fn generate_equations_fn_def(&self, _eq: &mut Vec<TypeEquation>,
                               _ast: &CoreAst, _fn_def: &FnDef) -> ErlResult<()> {
    // assert!(!fn_def.clauses.is_empty(), "Function definition with 0 clauses is not allowed");
    // println!("Gen eq for {:?}", fn_def);

    println!("{} TODO: fn_def", function_name!());
    // Return type of a function is union of its clauses return types
    // let ret_union_members = fn_def.clauses.iter()
    //     .map(|c| c.ret.clone())
    //     .collect();
    // let ret_union_t = ErlType::union_of(ret_union_members, true);
    // Self::equation(eq, ast, fn_def.ret_ty.into(), ret_union_t);
    Ok(())
  }

  // /// Type inference wiring
  // /// Generate type equations for AST node FClause
  // fn generate_equations_fn_clause(&self, eq: &mut Vec<TypeEquation>,
  //                                 ast: &CoreAst, fc: &FnClause) -> ErlResult<()> {
  //   // For each fun clause its return type is matched with body expression type
  //   Self::equation(eq, ast, fc.ret.clone(), fc.body.get_type());
  //   Ok(())
  // }

  /// Type inference wiring
  /// Generate type equations for AST node Application (a function call) Expr(Arg, ...)
  fn generate_equations_apply(&self, eq: &mut Vec<TypeEquation>,
                              ast: &CoreAst, app: &Apply) -> ErlResult<()> {
    // The expression we're calling must be something callable, i.e. must match a fun(Arg...)->Ret
    // Produce rule: App.Expr.type <=> fun(T1, T2, ...) -> Ret
    Self::equation(eq, ast,
                   &app.target.get_type(),
                   &app.get_function_type());

    // The return type of the application (App.Ret) must match the return type of the fun(Args...)->Ret
    // Equation: Application.Ret <=> Expr(Args...).Ret
    // let expr_type: &FunctionType = app.expr_ty.as_function();
    // Self::equation(eq, ast, app.ret_ty.clone(), *(expr_type.ret_type).clone(), "Apply");
    // todo!("Match app.ret with return type of the expr");
    Ok(())
  }
}
//! Contains code for generating type equations from AST in `Unifier` impl
use function_name::named;
use crate::typing::unifier::Unifier;
use crate::typing::unifier::equation::TypeEquation;
use crate::typing::erl_type::ErlType;
use crate::erlang::module::ErlModule;
use crate::erl_error::ErlResult;
use crate::syntaxtree::core_erl::core_ast_iter::AstChild;
use std::borrow::Borrow;
use std::ops::Deref;
use crate::syntaxtree::core_erl::core_ast::CoreAst;
use crate::syntaxtree::core_erl::node::fn_def::FnDef;
use crate::syntaxtree::core_erl::node::apply::Apply;

impl Unifier {
  /// Add a type equation, shortcut
  fn equation(eq: &mut Vec<TypeEquation>, ast: &CoreAst,
              type_deduced: ErlType,
              type_expected: ErlType) {
    eq.push(
      TypeEquation::new(ast.location(),
                        type_deduced,
                        type_expected,
                        format!("{:?}", ast))
    )
  }

  /// Type inference wiring
  /// Generate type equations from node. Each type variable is opposed to some type which we know, or
  /// to Any, if we don't know.
  #[named]
  pub fn generate_equations(&self, module: &ErlModule,
                            eq: &mut Vec<TypeEquation>, ast: &CoreAst) -> ErlResult<()> {
    // Recursively descend into AST and visit deepest nodes first
    if let Some(children) = ast.children() {
      for child in children {
        let gen_result = match child {
          AstChild::Ref(c) => self.generate_equations(module, eq, c),
          AstChild::RefCell(refc) => self.generate_equations(module, eq, refc.borrow())
        };
        match gen_result {
          Ok(_) => {} // nothing, all good
          Err(err) => { module.add_error(err); }
        }
      }
    }

    match ast.deref() {
      CoreAst::Module { .. } => {} // module root creates no equations
      CoreAst::Attributes { .. } => {}
      CoreAst::Lit { value, ty, .. } => {
        Self::equation(eq, ast, ty.into(), value.get_type());
      }
      CoreAst::Var { .. } => {}
      CoreAst::FnDef { fn_def, .. } => {
        self.generate_equations_fn_def(eq, ast, fn_def)?;
        // no clauses, functions are single clause, using `case` to branch
        // for fc in &fn_def.clauses {
        //   self.generate_equations_fn_clause(eq, ast, &fc)?
        // }
      }
      CoreAst::Apply { app, .. } => {
        self.generate_equations_apply(eq, ast, app)?
      }
      CoreAst::Let { letexpr, .. } => {
        Self::equation(eq, ast,
                       letexpr.ret_ty.into(),
                       letexpr.in_expr.get_type());
      }
      CoreAst::Case { case, .. } => {
        // For Case expression, type of case must be union of all clause types
        let all_clause_types = case.clauses.iter()
            .map(|c| c.body.get_type())
            .collect();
        let all_clauses_t = ErlType::union_of(all_clause_types, true);

        for clause in case.clauses.iter() {
          // Clause type must match body type
          Self::equation(eq, ast, clause.ret_ty.into(), clause.body.get_type());

          // No check for clause condition, but the clause condition guard must be boolean
          if let Some(guard) = &clause.guard {
            Self::equation(eq, ast, guard.get_type(), ErlType::AnyBool);
          }
        }

        Self::equation(eq, ast, case.ret_ty.into(), all_clauses_t);
      }
      CoreAst::BinOp { op: binop, .. } => {
        // Check result of the binary operation
        Self::equation(eq, ast, binop.ty.into(), binop.get_result_type());

        if let Some(arg_type) = binop.get_arg_type() {
          // Both sides of a binary op must have type appropriate for that op
          Self::equation(eq, ast, binop.left.get_type(), arg_type.clone());
          Self::equation(eq, ast, binop.right.get_type(), arg_type);
        }
      }
      CoreAst::UnOp { op: unop, .. } => {
        // Equation of expression type must match either bool for logical negation,
        // or (int|float) for numerical negation
        Self::equation(eq, ast, unop.expr.get_type(), unop.get_type());
        // TODO: Match return type with inferred return typevar?
      }
      CoreAst::List { .. } => {}
      CoreAst::Tuple { .. } => {}

      _ => unreachable!("{}: Can't process {}", function_name!(), ast),
    }
    Ok(())
  }


  /// Type inference wiring
  /// Generate type equations for AST node FunctionDef
  fn generate_equations_fn_def(&self, _eq: &mut Vec<TypeEquation>,
                               _ast: &CoreAst, _fn_def: &FnDef) -> ErlResult<()> {
    // assert!(!fn_def.clauses.is_empty(), "Function definition with 0 clauses is not allowed");
    // println!("Gen eq for {:?}", fn_def);

    todo!("fn_def equations");
    // Return type of a function is union of its clauses return types
    // let ret_union_members = fn_def.clauses.iter()
    //     .map(|c| c.ret.clone())
    //     .collect();
    // let ret_union_t = ErlType::union_of(ret_union_members, true);
    // Self::equation(eq, ast, fn_def.ret_ty.into(), ret_union_t);

    // Ok(())
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
                   (*app.target).borrow().get_type(),
                   app.get_function_type());

    // The return type of the application (App.Ret) must match the return type of the fun(Args...)->Ret
    // Equation: Application.Ret <=> Expr(Args...).Ret
    // let expr_type: &FunctionType = app.expr_ty.as_function();
    // Self::equation(eq, ast, app.ret_ty.clone(), *(expr_type.ret_type).clone(), "Apply");
    // todo!("Match app.ret with return type of the expr");
    Ok(())
  }
}
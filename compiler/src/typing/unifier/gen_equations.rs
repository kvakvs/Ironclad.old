//! Contains code for generating type equations from AST in `Unifier` impl
use crate::typing::unifier::Unifier;
use crate::typing::equation::TypeEquation;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;
use crate::erl_module::ErlModule;
use crate::erl_error::ErlResult;
use crate::syntaxtree::erl::erl_ast_iter::AstChild;
use std::borrow::Borrow;
use std::ops::Deref;
use crate::syntaxtree::erl::node::function_def::FunctionDef;
use crate::syntaxtree::erl::node::fun_clause::FunctionClause;
use crate::syntaxtree::erl::node::apply::Apply;

impl Unifier {
  /// Add a type equation, shortcut
  fn equation(eq: &mut Vec<TypeEquation>, ast: &ErlAst, ty1: ErlType, ty2: ErlType, annotation: &str) {
    eq.push(TypeEquation::new(ast.location(), ty1, ty2, annotation))
  }

  /// Type inference wiring
  /// Generate type equations from node. Each type variable is opposed to some type which we know, or
  /// to Any, if we don't know.
  pub fn generate_equations(&self, module: &ErlModule,
                            eq: &mut Vec<TypeEquation>, ast: &ErlAst) -> ErlResult<()> {
    // Recursively descend into AST and visit deepest nodes first
    if let Some(children) = ast.children(module) {
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
      ErlAst::ModuleForms(_) => {} // module root creates no equations
      ErlAst::ModuleAttr { .. } => {}
      ErlAst::Comment { .. } => {}
      ErlAst::Lit(_loc, _) => {} // creates no equation, type is known
      ErlAst::Var { .. } => {}
      ErlAst::FunctionDef { index, .. } => {
        let nf = &module.functions[*index];
        self.generate_equations_fun_def(eq, ast, nf)?;
        for fc in &nf.clauses {
          self.generate_equations_fclause(eq, ast, &fc)?
        }
      }
      // ErlAst::FClause(_loc, fc) => {
      //   self.generate_equations_fclause(eq, ast, fc)?
      // }
      ErlAst::App(_loc, app) => {
        self.generate_equations_app(eq, ast, app)?
      }
      ErlAst::Let(_loc, let_expr) => {
        Self::equation(eq, ast,
                       let_expr.in_ty.into(),
                       let_expr.in_expr.get_type(), "Let");
      }
      ErlAst::Case(_loc, case) => {
        // For Case expression, type of case must be union of all clause types
        let all_clause_types = case.clauses.iter()
            .map(|c| c.body.get_type())
            .collect();
        let all_clauses_t = ErlType::union_of(all_clause_types, true);

        Self::equation(eq, ast,
                       case.ret_ty.into(),
                       all_clauses_t, "Case");
      }
      ErlAst::CClause(_loc, clause) => {
        // Clause type must match body type
        Self::equation(eq, ast,
                       clause.ty.into(),
                       clause.body.get_type(), "Case clause");

        // No check for clause condition, but the clause condition guard must be boolean
        Self::equation(eq, ast,
                       clause.guard.get_type(),
                       ErlType::AnyBool, "Case clause");
      }
      ErlAst::BinaryOp(_loc, binop) => {
        // Check result of the binary operation
        Self::equation(eq, ast,
                       binop.ty.into(),
                       binop.get_result_type(), "Binop");

        if let Some(arg_type) = binop.get_arg_type() {
          // Both sides of a binary op must have type appropriate for that op
          Self::equation(eq, ast,
                         binop.left.get_type(), arg_type.clone(), "Binop");
          Self::equation(eq, ast,
                         binop.right.get_type(), arg_type, "Binop");
        }
      }
      ErlAst::UnaryOp(_loc, unop) => {
        // Equation of expression type must match either bool for logical negation,
        // or (int|float) for numerical negation
        Self::equation(eq, ast, unop.expr.get_type(), unop.get_type(),
                       &format!("Unary op {}", ast));
        // TODO: Match return type with inferred return typevar?
      }
      ErlAst::Comma { right, ty, .. } => {
        Self::equation(eq, ast,
                       (*ty).into(),
                       right.get_type(), "Comma op");
      }
      ErlAst::List(_loc, _elems) => {}
      ErlAst::Tuple(_loc, _elems) => {}
      ErlAst::FunArity(..) => {}

      _ => unreachable!("Can't process {}", ast),
    }
    Ok(())
  }


  /// Type inference wiring
  /// Generate type equations for AST node FunctionDef
  fn generate_equations_fun_def(&self, eq: &mut Vec<TypeEquation>,
                                ast: &ErlAst, f_def: &FunctionDef) -> ErlResult<()> {
    assert!(!f_def.clauses.is_empty(), "Function definition with 0 clauses is not allowed");
    // Return type of a function is union of its clauses return types
    let ret_union_members = f_def.clauses.iter()
        .map(|c| c.ret.clone())
        .collect();
    let ret_union_t = ErlType::union_of(ret_union_members, true);
    Self::equation(eq, ast,
                   f_def.ret_ty.into(),
                   ret_union_t, "Newfun");
    Ok(())
  }

  /// Type inference wiring
  /// Generate type equations for AST node FClause
  fn generate_equations_fclause(&self, eq: &mut Vec<TypeEquation>,
                                ast: &ErlAst, fc: &FunctionClause) -> ErlResult<()> {
    // For each fun clause its return type is matched with body expression type
    Self::equation(eq, ast, fc.ret.clone(), fc.body.get_type(), "Fun clause");
    Ok(())
  }

  /// Type inference wiring
  /// Generate type equations for AST node Application (a function call) Expr(Arg, ...)
  fn generate_equations_app(&self, eq: &mut Vec<TypeEquation>,
                            ast: &ErlAst, app: &Apply) -> ErlResult<()> {
    // The expression we're calling must be something callable, i.e. must match a fun(Arg...)->Ret
    // Produce rule: App.Expr.type <=> fun(T1, T2, ...) -> Ret
    // TODO: Instead of AnyFunction create a functional type of the correct arity and return type?
    Self::equation(eq, ast, (*app.expr).borrow().get_type(),
                   app.get_function_type(), "Apply");

    // The return type of the application (App.Ret) must match the return type of the fun(Args...)->Ret
    // Equation: Application.Ret <=> Expr(Args...).Ret
    // let expr_type: &FunctionType = app.expr_ty.as_function();
    // Self::equation(eq, ast, app.ret_ty.clone(), *(expr_type.ret_type).clone(), "Apply");
    // todo!("Match app.ret with return type of the expr");
    Ok(())
  }
}
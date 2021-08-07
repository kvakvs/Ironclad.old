//! Module provides logic for unifying the type equations generated for a program's AST, and
//! then using this data is able to infer the type of any AST piece from the same program.

use crate::typing::equation::TypeEquation;
use crate::typing::erl_type::ErlType;
use std::collections::{HashMap, HashSet};
use crate::erl_error::{ErlResult, ErlError};
use crate::typing::error::TypeError;
use crate::typing::typevar::TypeVar;
use std::collections::hash_map::Entry;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::function_type::FunctionType;
use std::ops::Deref;
use crate::syntaxtree::erl::node::new_function_node::NewFunctionNode;
use crate::syntaxtree::erl::node::fun_clause_node::FunctionClauseNode;
use crate::syntaxtree::erl::node::application_node::ApplicationNode;
use crate::erl_module::ErlModule;
use std::rc::Rc;
use std::sync::{RwLock};
use std::borrow::{Borrow};

type SubstMap = HashMap<TypeVar, ErlType>;

/// A program AST is analyzed and type equations are generated outside of this struct.
/// This struct contains substitution map with found solutions for the type equations.
/// This struct is able to provide type inference for any piece of AST thereafter.
pub struct Unifier {
  /// Generated type constraints in the program
  equations: Vec<TypeEquation>,
  /// Substitution map containing solutions matching type variables to real types
  pub subst: SubstMap,
}

impl Default for Unifier {
  fn default() -> Self {
    Self {
      equations: vec![],
      subst: Default::default(),
    }
  }
}

impl Unifier {
  /// Create a new Unifier from AST tree, and setup the equations for this code.
  /// This will scan the AST and prepare data for type inference.
  pub fn new(module: &mut ErlModule) -> ErlResult<Self> {
    let mut unifier = Self {
      equations: vec![],
      subst: Default::default(),
    };

    let mut eq = Vec::new();
    let ast: Rc<RwLock<ErlAst>> = module.ast.clone();

    {
      let ast_r = ast.read().unwrap();
      unifier.generate_equations(module, &mut eq, &ast_r)?;
      unifier.equations = eq;
      for eq in unifier.equations.iter() {
        println!("Eq: {}", eq);
      }

      unifier.unify_all_equations(&ast_r)?;
      println!("Unify map: {:?}", &unifier.subst);
    }

    Ok(unifier)
  }

  /// Goes through all generated type equations and applies self.unify() to arrive to a solution
  fn unify_all_equations(&mut self, ast: &ErlAst) -> ErlResult<()> {
    let mut equations: Vec<TypeEquation> = Vec::new();
    std::mem::swap(&mut self.equations, &mut equations); // move

    let errors: Vec<ErlError> = equations.iter()
        .map(|eq| {
          self.unify(ast, &eq.left, &eq.right)
        })
        .filter(Result::is_err)
        .map(Result::unwrap_err)
        .collect();
    if !errors.is_empty() {
      return Err(ErlError::Multiple(errors));
    }
    Ok(())
  }

  /// Unify for when both sides of equation are function types
  fn unify_fun_fun(&mut self, ast: &ErlAst,
                   fun1: &FunctionType, fun2: &FunctionType) -> ErlResult<()> {
    if fun1.arg_types.len() != fun2.arg_types.len() {
      return Err(ErlError::from(TypeError::FunAritiesDontMatch));
    }

    // Unify functions return types
    self.unify(ast, &fun1.ret_type, &fun2.ret_type)?;

    // Then unify each arg of function1 with corresponding arg of function2
    let errors: Vec<ErlError> = fun1.arg_types.iter()
        .zip(fun2.arg_types.iter())
        .map(|(t1, t2)| {
          self.unify(ast, &t1, &t2)
        })
        .filter(Result::is_err)
        .map(Result::unwrap_err)
        .collect();
    if !errors.is_empty() {
      return Err(ErlError::Multiple(errors));
    }
    Ok(())
  }

  /// Unify two types type1 and type2, with self.subst map
  /// Updates self.subst (map of name->Type) with new record which unifies type1 and type2,
  /// and returns true. Returns false if they can't be unified.
  fn unify(&mut self, ast: &ErlAst, type1: &ErlType, type2: &ErlType) -> ErlResult<()> {
    if type1 == type2 {
      return Ok(());
    }

    match type1 {
      ErlType::TVar(tv1) => {
        return self.unify_variable(ast, tv1, type2);
      }

      ErlType::Function(fun1) => {
        match type2 {
          ErlType::Function(fun2) => {
            // match two function types
            return self.unify_fun_fun(ast, fun1, fun2);
          }
          _any_type2 => {}
        }
      }

      // Left is a LocalFunction and the right is a function type
      // Atom must be an existing local function
      ErlType::LocalFunction { name: name1, arity: arity1 } => {
        match type2 {
          ErlType::Function(fun2) => {
            // TODO: Find_fun can be cached in some dict? Or use a module struct with local fun dict
            // if let m = self.module.upgrade().unwrap().write().unwrap() {
            let local_fun = ast.find_fun(&name1, *arity1).unwrap().new_function;

            // Unify left and right as function types
            // Equation: Expr.Type <=> Fn ( Arg1.Type, Arg2.Type, ... )
            self.unify(ast,
                       &local_fun.borrow().ret_ty.into(),
                       &type2)?;

            // Equation: Application Expr(Args...) <=> Fn.Ret

            let fc = &local_fun.borrow().clauses[0];
            if fc.args.len() == *arity1 && fun2.arg_types.len() == *arity1 {
              return Ok(());
            }
            // }
          }
          _any_type2 => {}
        }
      }

      _any_type1 => {
        // Union should not be broken into subtypes, match left equation part directly vs the union
        match type2 {
          ErlType::Union(types) => {
            if self.check_in_union(ast, &type1, &types) {
              return Ok(());
            }
          }
          _any_type2 => {}
        }
      }
    }

    //
    // Reverse rules type2 then type1
    //
    match type2 {
      ErlType::Number => {
        match type1 {
          // Any numbers and number sets will match a number
          ErlType::Number | ErlType::Integer(_) | ErlType::AnyInteger
          | ErlType::Float => return Ok(()),
          _any_type1 => {}
        }
      }
      ErlType::AnyInteger => {
        match type1 {
          // Any numbers and number sets will match a number
          ErlType::Integer(_) | ErlType::AnyInteger => return Ok(()),
          _any_type1 => {}
        }
      }
      ErlType::Integer(c2) => {
        match type1 {
          // Any numbers and number sets will match a number
          ErlType::Integer(c1) if c1 == c2 => return Ok(()),
          _any_type1 => {}
        }
      }
      ErlType::AnyList => {
        match type1 {
          // Any lists will match a AnyList
          ErlType::List(_) => return Ok(()),
          _any_type1 => {}
        }
      }
      ErlType::List(elem2) => {
        match type1 {
          // Any tuple will match a AnyTuple
          ErlType::List(elem1) => return self.unify(ast, elem1, elem2),
          _any_type1 => {}
        }
      }
      ErlType::AnyTuple => {
        match type1 {
          // Any tuple will match a AnyTuple
          ErlType::Tuple(_) => return Ok(()),
          _any_type1 => {}
        }
      }
      _any_type2 => {}
    }

    Err(ErlError::from(TypeError::TypesDontMatch {
      t1: type1.clone(),
      t2: type2.clone(),
    }))
  }

  /// Whether any member of type union matches type t?
  fn check_in_union(&mut self, ast: &ErlAst, t: &ErlType, union: &HashSet<ErlType>) -> bool {
    union.iter().any(|member| {
      self.unify(ast, &t, &member).is_ok()
    })
  }

  fn unify_variable(&mut self, ast: &ErlAst, tvar: &TypeVar, ty: &ErlType) -> ErlResult<()> {
    if let Entry::Occupied(entry1) = self.subst.entry(tvar.clone()) {
      let subst_entry = entry1.get().clone();
      return self.unify(ast, &subst_entry, &ty);
    }

    if let ErlType::TVar { .. } = ty {
      if let Entry::Occupied(entry2) = self.subst.entry(tvar.clone()) {
        let subst_entry = entry2.get().clone();
        return self.unify(ast, &ErlType::TVar(tvar.clone()), &subst_entry);
      }
    }

    if self.occurs_check(&tvar, &ty) {
      let error = TypeError::OccursCheckFailed {
        tvar: tvar.clone(),
        ty: ty.clone(),
      };
      return Err(ErlError::from(error));
    }

    // tvar is not yet in subst and can't simplify the right side. Extend subst.
    self.subst.insert(tvar.clone(), ty.clone());
    Ok(())
  }

  /// Does the variable v occur anywhere inside typ?
  /// Variables in typ are looked up in subst and the check is applied
  ///     recursively.
  //     elif isinstance(typ, FuncType):
  //         return (occurs_check(v, typ.rettype, subst) or
  //                 any(occurs_check(v, arg, subst) for arg in typ.argtypes))
  fn occurs_check(&self, tv: &TypeVar, ty: &ErlType) -> bool {
    // if ty is a TypeVar and they're equal
    match ty {
      ErlType::TVar(ty_inner) => ty_inner == tv,
      ErlType::Function(fun_type) => {
        return self.occurs_check(tv, &fun_type.ret_type)
            || fun_type.arg_types.iter().any(|a| self.occurs_check(tv, a));
      }
      ErlType::Union(members) => {
        members.iter().any(|m| self.occurs_check(tv, m))
      }
      ErlType::AnyList => false, // does not occur
      ErlType::List(union_t) => self.occurs_check(tv, union_t),
      ErlType::Tuple(elems) => {
        elems.iter().any(|m| self.occurs_check(tv, m))
      }
      // Any simple type cannot have typevar tv in it, so all false
      simple_type if simple_type.is_simple_value_type() => false,

      _ => {
        unimplemented!("Don't know how to occurs_check on typevar {:?} and type {:?}", tv, ty)
      }
    }

    // false
  }

  /// Applies the unifier subst to typ.
  /// Returns a type where all occurrences of variables bound in subst
  /// were replaced (recursively); on failure returns None.
  /// Also known as: Apply Unifier (apply_unifier)
  pub fn infer_type(&mut self, ty: ErlType) -> ErlType {
    if self.subst.is_empty() || ty.is_simple_value_type() {
      return ty;
    }

    if let ErlType::TVar(tvar) = &ty {
      match self.subst.entry(tvar.clone()) {
        Entry::Occupied(entry) => {
          let entry_val = entry.get().clone();
          return self.infer_type(entry_val);
        }
        Entry::Vacant(_) => return ty,
      }
    }

    if let ErlType::Function(fun_type) = &ty {
      return ErlType::Function(FunctionType {
        name: fun_type.name.clone(),
        arg_types: fun_type.arg_types.iter()
            .map(|t| self.infer_type(t.clone()))
            .collect(),
        ret_type: Box::new(self.infer_type(*fun_type.ret_type.clone())),
      });
    }

    ErlType::None
  }

  /// Finds the type of the expression for the given substitution.
  pub fn infer_ast(&mut self, expr: &ErlAst) -> ErlType {
    self.infer_type(expr.get_type())
  }

  /// Type inference wiring
  /// Generate type equations for AST node NewFunction
  fn generate_equations_newfunction(&self, eq: &mut Vec<TypeEquation>, ast: &ErlAst, nf: &NewFunctionNode) -> ErlResult<()> {
    // Return type of a function is union of its clauses return types
    let ret_union_members = nf.clauses.iter()
        .map(|c| c.ret.clone())
        .collect();
    let ret_union_t = ErlType::union_of(ret_union_members);
    Self::equation(eq, ast,
                   nf.ret_ty.into(),
                   ret_union_t, "Newfun");
    Ok(())
  }

  /// Type inference wiring
  /// Generate type equations for AST node FClause
  fn generate_equations_fclause(&self, eq: &mut Vec<TypeEquation>,
                                ast: &ErlAst, fc: &FunctionClauseNode) -> ErlResult<()> {
    // For each fun clause its return type is matched with body expression type
    Self::equation(eq, ast, fc.ret.clone(), fc.body.get_type(), "Fun clause");
    Ok(())
  }

  /// Type inference wiring
  /// Generate type equations for AST node Application (a function call) Expr(Arg, ...)
  fn generate_equations_app(&self, eq: &mut Vec<TypeEquation>,
                            ast: &ErlAst, app: &ApplicationNode) -> ErlResult<()> {
    // The expression we're calling must be something callable, i.e. must match a fun(Arg...)->Ret
    // Produce rule: App.Expr.type <=> fun(T1, T2, ...) -> Ret
    // TODO: Instead of AnyFunction create a functional type of the correct arity and return type?
    Self::equation(eq, ast, app.expr.get_type(), ErlType::AnyFunction, "Apply");

    // The return type of the application (App.Ret) must match the return type of the fun(Args...)->Ret
    // Equation: Application.Ret <=> Expr(Args...).Ret
    // let expr_type: &FunctionType = app.expr_ty.as_function();
    // Self::equation(eq, ast, app.ret_ty.clone(), *(expr_type.ret_type).clone(), "Apply");
    // todo!("Match app.ret with return type of the expr");
    Ok(())
  }

  /// Add a type equation, shortcut
  fn equation(eq: &mut Vec<TypeEquation>, ast: &ErlAst, ty1: ErlType, ty2: ErlType, annotation: &str) {
    eq.push(TypeEquation::new(ast.location(), ty1, ty2, annotation))
  }

  /// Type inference wiring
  /// Generate type equations from node. Each type variable is opposed to some type which we know, or
  /// to Any, if we don't know.
  pub fn generate_equations(&self, module: &mut ErlModule,
                            eq: &mut Vec<TypeEquation>, ast: &ErlAst) -> ErlResult<()> {
    // Recursively descend into AST and visit deepest nodes first
    match ast.children() {
      Some(children) => for nested_ast in children {
        match self.generate_equations(module, eq, nested_ast) {
          Ok(_) => {} // nothing, all good
          Err(err) => { module.add_error(err); }
        }
      }
      None => {}
    }

    match ast.deref() {
      ErlAst::ModuleForms(_) => {} // module root creates no equations
      ErlAst::ModuleAttr { .. } => {}
      ErlAst::Comment { .. } => {}
      ErlAst::Lit(_loc, _) => {} // creates no equation, type is known
      ErlAst::Var { .. } => {}
      ErlAst::NewFunction(_loc, nf) => {
        self.generate_equations_newfunction(eq, ast, nf)?;
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
        let all_clauses_t = ErlType::union_of(all_clause_types);

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

      _ => unreachable!("Can't process {}", ast),
    }
    Ok(())
  }
}
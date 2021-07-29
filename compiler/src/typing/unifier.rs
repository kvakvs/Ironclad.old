//! Module provides logic for unifying the type equations generated for a program's AST, and
//! then using this data is able to infer the type of any AST piece from the same program.

use crate::typing::equation::TypeEquation;
use crate::typing::erl_type::ErlType;
use std::collections::HashMap;
use crate::erl_error::{ErlResult, ErlError};
use crate::typing::error::TypeError;
use crate::typing::typevar::TypeVar;
use std::collections::hash_map::Entry;
use std::rc::Rc;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::function_type::FunctionType;
use std::ops::Deref;
use crate::syntaxtree::erl::node::new_function_node::NewFunctionNode;
use crate::syntaxtree::erl::node::fun_clause_node::FunctionClauseNode;
use crate::syntaxtree::erl::node::application_node::ApplicationNode;

type SubstMap = HashMap<TypeVar, ErlType>;

/// A program AST is analyzed and type equations are generated outside of this struct.
/// This struct contains substitution map with found solutions for the type equations.
/// This struct is able to provide type inference for any piece of AST thereafter.
pub struct Unifier {
  /// Generated type constraints in the program
  equations: Vec<TypeEquation>,
  /// Substitution map containing solutions matching type variables to real types
  pub subst: SubstMap,
  /// Root AST node for source queries, like checking whether a function exists
  root: Rc<ErlAst>,
}

impl Default for Unifier {
  fn default() -> Self {
    Self {
      equations: vec![],
      subst: Default::default(),
      root: Rc::new(ErlAst::Empty),
    }
  }
}

impl Unifier {
  /// Create a new Unifier from AST tree, and setup the equations for this code.
  /// This will scan the AST and prepare data for type inference.
  pub fn new(ast: &Rc<ErlAst>) -> ErlResult<Self> {
    let mut unifier = Self {
      equations: vec![],
      subst: Default::default(),
      root: ast.clone(),
    };

    unifier.generate_equations(&ast)?;
    println!("Equations: {:?}", unifier.equations);

    unifier.unify_all_equations()?;
    println!("Unify map: {:?}", &unifier.subst);

    Ok(unifier)
  }

  /// Goes through all generated type equations and applies self.unify() to arrive to a solution
  fn unify_all_equations(&mut self) -> ErlResult<()> {
    let mut equations: Vec<TypeEquation> = Vec::new();
    std::mem::swap(&mut self.equations, &mut equations); // move

    let errors: Vec<ErlError> = equations.iter()
        .map(|eq| {
          self.unify(&eq.left, &eq.right)
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
  fn unify_fun_fun(&mut self, fun1: &FunctionType, fun2: &FunctionType) -> ErlResult<()> {
    if fun1.arg_types.len() != fun2.arg_types.len() {
      return Err(ErlError::from(TypeError::FunAritiesDontMatch));
    }

    // Unify functions return types
    self.unify(&fun1.ret_type, &fun2.ret_type)?;

    // Then unify each arg of function1 with corresponding arg of function2
    let errors: Vec<ErlError> = fun1.arg_types.iter()
        .zip(fun2.arg_types.iter())
        .map(|(t1, t2)| {
          self.unify(&t1, &t2)
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
  fn unify(&mut self, type1: &ErlType, type2: &ErlType) -> ErlResult<()> {
    if type1 == type2 {
      return Ok(());
    }

    match type1 {
      ErlType::TVar(tv1) => {
        return self.unify_variable(tv1, type2);
      }

      ErlType::Function(fun1) => {
        match type2 {
          ErlType::Function(fun2) => {
            // match two function types
            return self.unify_fun_fun(fun1, fun2);
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
            let local_fun = self.root.find_fun(&name1, *arity1).unwrap()
                .new_function.clone();

            // Unify left and right as function types
            // Equation: Expr.Type <=> Fn ( Arg1.Type, Arg2.Type, ... )
            self.unify(&local_fun.ret, &type2)?;

            // Equation: Application Expr(Args...) <=> Fn.Ret

            let fc = &local_fun.clauses[0];
            if fc.args.len() == *arity1 && fun2.arg_types.len() == *arity1 {
              return Ok(());
            }
          }
          _any_type2 => {}
        }
      }

      _any_type1 => {
        // Union should not be broken into subtypes, match left equation part directly vs the union
        match type2 {
          ErlType::Union(types) => {
            if self.check_in_union(&type1, &types) {
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
          ErlType::Number | ErlType::IntegerConst(_) | ErlType::AnyInteger
          | ErlType::Float => return Ok(()),
          _any_type1 => {},
        }
      }
      ErlType::AnyInteger => {
        match type1 {
          // Any numbers and number sets will match a number
          ErlType::IntegerConst(_) | ErlType::AnyInteger => return Ok(()),
          _any_type1 => {},
        }
      }
      ErlType::IntegerConst(c2) => {
        match type1 {
          // Any numbers and number sets will match a number
          ErlType::IntegerConst(c1) if c1 == c2 => return Ok(()),
          _any_type1 => {},
        }
      }
      _any_type2 => {},
    }

    Err(ErlError::from(TypeError::TypesDontMatch {
      t1: type1.clone(),
      t2: type2.clone(),
    }))
  }

  /// Whether any member of type union matches type t?
  fn check_in_union(&mut self, t: &ErlType, union: &Vec<ErlType>) -> bool {
    union.iter().any(|member| {
      self.unify(&t, &member).is_ok()
    })
  }

  fn unify_variable(&mut self, tvar: &TypeVar, ty: &ErlType) -> ErlResult<()> {
    if let Entry::Occupied(entry1) = self.subst.entry(tvar.clone()) {
      let subst_entry = entry1.get().clone();
      return self.unify(&subst_entry, &ty);
    }

    if let ErlType::TVar { .. } = ty {
      if let Entry::Occupied(entry2) = self.subst.entry(tvar.clone()) {
        let subst_entry = entry2.get().clone();
        return self.unify(&ErlType::TVar(tvar.clone()), &subst_entry);
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
      // Any simple type cannot have typevar tv in it, so all false
      simple_type if simple_type.is_simple_value_type() => false,

      _ => todo!("Don't know how to occurs_check on typevar {:?} and type {:?}", tv, ty)
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
  pub fn infer_ast(&mut self, expr: &Rc<ErlAst>) -> ErlType {
    self.infer_type(expr.get_type())
  }

  /// Type inference wiring
  /// Generate type equations for AST node NewFunction
  fn generate_equations_newfunction(&mut self, ast: &Rc<ErlAst>, nf: &NewFunctionNode) -> ErlResult<()> {
    // Return type of a function is union of its clauses return types
    let ret_union_members = nf.clauses.iter()
        .map(|c| c.ret.clone())
        .collect();
    let ret_union_t = ErlType::union_of(ret_union_members);
    self.equation(ast, nf.ret.clone(), ret_union_t);
    Ok(())
  }

  /// Type inference wiring
  /// Generate type equations for AST node FClause
  fn generate_equations_fclause(&mut self, ast: &Rc<ErlAst>, fc: &FunctionClauseNode) -> ErlResult<()> {
    // For each fun clause its return type is matched with body expression type
    self.equation(ast, fc.ret.clone(), fc.body.get_type());
    Ok(())
  }

  /// Type inference wiring
  /// Generate type equations for AST node Application (a function call)
  fn generate_equations_app(&mut self, ast: &Rc<ErlAst>, app: &ApplicationNode) -> ErlResult<()> {
    // The expression we're calling must be something callable, i.e. must match a fun(Arg...)->Ret
    // Produce rule: App.Expr.type <=> fun(T1, T2, ...) -> Ret
    self.equation(ast, app.expr.get_type(), app.expr_type.clone());

    // The return type of the application (App.Ret) must match the return type of the fun(Args...)->Ret
    // Equation: Application.Ret <=> Expr(Args...).Ret
    let expr_type: &FunctionType = app.expr_type.as_function().unwrap();
    self.equation(ast, app.ret_type.clone(), *(expr_type.ret_type).clone());

    Ok(())
  }

  /// Add a type equation, shortcut
  fn equation(&mut self, ast: &Rc<ErlAst>, ty1: ErlType, ty2: ErlType) {
    self.equations.push(TypeEquation::new(ast, ty1, ty2))
  }

  /// Type inference wiring
  /// Generate type equations from node. Each type variable is opposed to some type which we know, or
  /// to Any, if we don't know.
  pub fn generate_equations(&mut self, ast: &Rc<ErlAst>) -> ErlResult<()> {
    // Recursively descend into AST and visit deepest nodes first
    match ast.get_children() {
      Some(c) => {
        let (_, errors): (Vec<_>, Vec<_>) =
            c.iter()
                .map(|ast_node| { // drop OK results, keep errors
                  self.generate_equations(ast_node)
                })
                .partition(Result::is_ok);
        let mut errors: Vec<ErlError> = errors.into_iter().map(Result::unwrap_err).collect();
        match errors.len() {
          0 => (),
          1 => return Err(errors.pop().unwrap()),
          _ => return Err(ErlError::Multiple(errors)),
        }
      }
      None => {}
    }

    match ast.deref() {
      ErlAst::ModuleForms(_) => {} // module root creates no equations
      ErlAst::ModuleAttr { .. } => {}
      ErlAst::Comment => {}
      ErlAst::Lit(_) => {} // creates no equation, type is known
      ErlAst::Var { .. } => {}
      ErlAst::NewFunction(nf) => self.generate_equations_newfunction(ast, nf)?,
      ErlAst::FClause(fc) => self.generate_equations_fclause(ast, fc)?,
      ErlAst::App(app) => self.generate_equations_app(ast, app)?,
      ErlAst::Let(let_expr) => {
        self.equation(ast, let_expr.in_ty.clone(), let_expr.in_expr.get_type());
      }
      ErlAst::Case(case) => {
        // For Case expression, type of case must be union of all clause types
        let all_clause_types = case.clauses.iter()
            .map(|c| c.get_type())
            .collect();
        let all_clauses_t = ErlType::union_of(all_clause_types);
        self.equation(ast, case.ret.clone(), all_clauses_t);
      }

      ErlAst::CClause(clause) => {
        // Clause type must match body type
        self.equation(ast, clause.ty.clone(), clause.body.get_type());

        // No check for clause condition, but the clause condition guard must be boolean
        self.equation(ast, clause.guard.get_type(), ErlType::AnyBool);
      }

      ErlAst::BinaryOp(binop) => {
        // Check result of the binary operation
        self.equation(ast, binop.ty.clone(), binop.operator.get_result_type());

        match binop.operator.get_arg_type() {
          Some(arg_type) => {
            // Both sides of a binary op must have type appropriate for that op
            self.equation(ast, binop.left.get_type(), arg_type.clone());
            self.equation(ast, binop.right.get_type(), arg_type);
          }
          None => {}
        }
      }
      ErlAst::UnaryOp(unop) => {
        // Equation of expression type must match either bool for logical negation,
        // or (int|float) for numerical negation
        self.equation(ast, unop.expr.get_type(), unop.operator.get_type());
        // TODO: Match return type with inferred return typevar?
      }
      ErlAst::Comma { right, ty, .. } => {
        self.equation(ast, ty.clone(), right.get_type());
      }
      _ => unreachable!("Can't process {:?}", ast),
    }
    Ok(())
  }
}
//! Module provides logic for unifying the type equations generated for a program's AST, and
//! then using this data is able to infer the type of any AST piece from the same program.
pub mod gen_equations;
pub mod infer;

use crate::typing::equation::TypeEquation;
use crate::typing::erl_type::ErlType;
use std::collections::{HashMap, BTreeSet};
use crate::erl_error::{ErlResult, ErlError};
use crate::typing::error::TypeError;
use crate::typing::typevar::TypeVar;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::fn_type::FunctionType;
use crate::syntaxtree::erl::node::fn_def::FnDef;
use crate::erl_module::ErlModule;
use std::rc::Rc;
use std::sync::{RwLock};
use crate::typing::fn_clause_type::FnClauseType;

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
        println!("{:?}", eq);
      }

      unifier.unify_all_equations(module)?;
      println!("Unify map: {:?}", &unifier.subst);
    }

    Ok(unifier)
  }

  /// Goes through all generated type equations and applies self.unify() to arrive to a solution
  fn unify_all_equations(&mut self, module: &ErlModule) -> ErlResult<()> {
    let mut equations: Vec<TypeEquation> = Vec::new();
    std::mem::swap(&mut self.equations, &mut equations); // move

    let errors: Vec<ErlError> = equations.iter()
        .map(|eq| {
          self.unify(module, &eq.left, &eq.right)
        })
        .filter(Result::is_err)
        .map(Result::unwrap_err)
        .collect();
    if !errors.is_empty() {
      return Err(ErlError::multiple(errors));
    }
    Ok(())
  }

  /// Unify for when both sides of equation are function types
  fn unify_fun_fun(&mut self, module: &ErlModule,
                   fun1: &FunctionType, fun2: &FunctionType) -> ErlResult<()> {
    if fun1.arity != fun2.arity {
      return Err(ErlError::from(TypeError::FunAritiesDontMatch));
    }

    // Unify functions return types
    self.unify(module, &fun1.ret_type, &fun2.ret_type)?;

    // Then unify each arg of function1 with corresponding arg of function2
    unimplemented!("Unify {} vs {}", fun1, fun2);
    // let errors: Vec<ErlError> = fun1.clauses.iter()
    //     .zip(fun2.clauses.iter())
    //     .map(|(t1, t2)| {
    //       self.unify(module, ast, t1, t2)
    //     })
    //     .filter(Result::is_err)
    //     .map(Result::unwrap_err)
    //     .collect();
    // if !errors.is_empty() {
    //   return Err(ErlError::multiple(errors));
    // }
    // Ok(())
  }

  /// Unify two types type1 and type2, with self.subst map
  /// Updates self.subst (map of name->Type) with new record which unifies type1 and type2,
  /// and returns true. Returns false if they can't be unified.
  fn unify(&mut self, module: &ErlModule,
           type1: &ErlType, type2: &ErlType) -> ErlResult<()> {
    if type1 == type2 {
      return Ok(());
    }

    match type1 {
      ErlType::TVar(tv1) => {
        return self.unify_variable(module, tv1, type2);
      }

      ErlType::Function(fun1) => {
        match type2 {
          ErlType::Function(fun2) => {
            // match two function types
            return self.unify_fun_fun(module, fun1, fun2);
          }
          _any_type2 => {}
        }
      }

      // Left is a LocalFunction and the right is a function type
      // Atom must be an existing local function
      ErlType::LocalFunction(funarity1) => {
        match type2 {
          ErlType::Function(fun2) => {
            match module.functions_lookup.get(funarity1) {
              Some(fun_index) => {
                // Unify left and right as function types
                // Equation: Expr.Type <=> Fn ( Arg1.Type, Arg2.Type, ... )
                let fun_def: &FnDef = &module.functions[*fun_index];
                self.unify(module, &fun_def.ret_ty.into(), &type2)?;

                // Equation: Application Expr(Args...) <=> Fn.Ret

                let fc = &fun_def.clauses[0];
                if fc.args.len() == funarity1.arity && fun2.clauses.len() == funarity1.arity {
                  return Ok(());
                }
              }
              None => {
                let type_err = TypeError::LocalFunctionUndef {
                  module: module.name.clone(),
                  funarity: funarity1.clone(),
                };
                return Err(ErlError::from(type_err));
              }
            }
          }
          ErlType::AnyFunction => return Ok(()), // good
          _any_type2 => {}
        }
      }

      _any_type1 => {
        // Union should not be broken into subtypes, match left equation part directly vs the union
        match type2 {
          ErlType::Union(types) => {
            if self.unify_check_in_union(module, &type1, &types) {
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
      ErlType::AnyInteger => if let ErlType::Integer(_) | ErlType::AnyInteger = type1 {
        return Ok(());
      },
      ErlType::Integer(c2) => match type1 {
        // Any numbers and number sets will match a number
        ErlType::Integer(c1) if c1 == c2 => return Ok(()),
        _any_type1 => {}
      },
      ErlType::AnyList => if let ErlType::List(_) = type1 {
        return Ok(());
      },
      ErlType::AnyFunction => if let ErlType::Function(_) = type1 {
        return Ok(());
      },
      ErlType::List(elem2) => if let ErlType::List(elem1) = type1 {
        return self.unify(module, elem1, elem2);
      },
      ErlType::AnyTuple => if let ErlType::Tuple(_) = type1 {
        return Ok(());
      },
      _any_type2 => {}
    }

    Err(ErlError::from(TypeError::TypesDontMatch {
      t1: type1.clone(),
      t2: type2.clone(),
    }))
  }

  /// Whether any member of type union matches type t?
  fn unify_check_in_union(&mut self, env: &ErlModule,
                          t: &ErlType, union: &BTreeSet<ErlType>) -> bool {
    union.iter().any(|member| {
      self.unify(env, &t, &member).is_ok()
    })
  }

  fn unify_variable(&mut self, module: &ErlModule,
                    tvar: &TypeVar, ty: &ErlType) -> ErlResult<()> {
    if let Some(entry1) = self.subst.get(tvar) {
      let entry = entry1.clone();
      return self.unify(module, &entry, &ty);
    }

    if let ErlType::TVar { .. } = ty {
      if let Some(entry2_r) = self.subst.get(tvar) {
        let entry2 = entry2_r.clone();
        return self.unify(module, &ErlType::TVar(*tvar), &entry2);
      }
    }

    if self.occurs_check(&tvar, &ty) {
      let error = TypeError::OccursCheckFailed {
        tvar: *tvar,
        ty: ty.clone(),
      };
      return Err(ErlError::from(error));
    }

    // tvar is not yet in subst and can't simplify the right side. Extend subst.
    self.subst.insert(*tvar, ty.clone());
    Ok(())
  }

  fn occurs_check_fun_clause(&self, tv: &TypeVar, fc: &FnClauseType) -> bool {
    fc.arg_types.iter().any(|argt| self.occurs_check(tv, argt))
        || self.occurs_check(tv, &fc.ret_ty)
  }

  /// Does the variable v occur anywhere inside type `ty`?
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
            || fun_type.clauses.iter().any(|a: &FnClauseType| self.occurs_check_fun_clause(tv, a));
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
}
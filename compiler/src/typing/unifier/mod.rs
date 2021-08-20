//! Module provides logic for unifying the type equations generated for a program's AST, and
//! then using this data is able to infer the type of any AST piece from the same program.
use std::collections::{BTreeSet, BTreeMap};
use std::rc::Rc;
use std::sync::RwLock;

use equation::TypeEquation;

use crate::erl_error::{ErlError, ErlResult};
use crate::erl_module::ErlModule;
use crate::typing::erl_type::ErlType;
use crate::typing::error::TypeError;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::fn_type::FunctionType;
use crate::typing::typevar::TypeVar;
use crate::syntaxtree::core_erl::core_ast::CoreAst;

pub mod gen_equations;
pub mod infer;
pub mod equation;

type SubstMap = BTreeMap<TypeVar, ErlType>;

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
    let ast: Rc<RwLock<CoreAst>> = module.core_ast.clone();

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

    for eq in equations.iter() {
      match self.unify(module, &eq.type_left, &eq.type_right) {
        Ok(Some(subst)) => {
          self.subst.extend(subst.into_iter());
        }
        Ok(None) => {}
        Err(e) => return Err(e)
      }
      // TODO: return Err(ErlError::multiple(errors));
    }
    Ok(())
  }

  /// Unify two function clauses whether left matches right
  fn unify_fn_clause(&self, _module: &ErlModule,
                     fc1: &FnClauseType, fc2: &FnClauseType) -> ErlResult<Option<SubstMap>> {
    println!("Unify fn_c {} <> {}", fc1, fc2);
    Ok(None) // TODO
  }

  /// Unify for when both sides of equation are function types
  fn unify_fn(&self, module: &ErlModule,
              fun1: &FunctionType, fun2: &FunctionType) -> ErlResult<Option<SubstMap>> {
    // Check the arities must match
    if fun1.arity != fun2.arity {
      return Err(ErlError::from(TypeError::FunAritiesDontMatch));
    }

    let mut subst: SubstMap = Default::default(); // Contains successful substitutions

    // Try find 1 or more substitutions to make any clause in c1 equal to any clause in c2
    for c1 in &fun1.clauses {
      let unify_successes: Vec<Option<SubstMap>> = fun2.clauses.iter()
          .map(|c2| self.unify(module,
                               &ErlType::FnClause(c1.clone()),
                               &ErlType::FnClause(c2.clone())))
          .filter(Result::is_ok)
          .map(Result::unwrap)
          .collect();

      if !unify_successes.is_empty() {
        // At least one match succeeded, means the clause c1 is compatible with some clauses in fun2
        unify_successes.into_iter()
            .filter(Option::is_some)
            .map(Option::unwrap)
            .for_each(|each_sub| subst.extend(each_sub.into_iter()));
      }
    }

    // At least one clause in fun1 matches at least one in fun2
    if subst.is_empty() {
      // Report an error
      let t_err = TypeError::TypesDontMatch {
        t1: ErlType::Fn(fun1.clone()),
        t2: ErlType::Fn(fun2.clone()),
      };
      return Err(ErlError::TypeError(t_err));
    }

    Ok(Some(subst))
  }

  /// Returns success from Unify call, when a substitution has been found. Merges it into our known
  /// `Unifier::subst` map.
  fn unify_commit(&mut self, subst: Option<SubstMap>) -> ErlResult<()> {
    if let Some(subst1) = subst {
      self.subst.extend(subst1.into_iter());
    }
    Ok(())
  }

  /// Unify two types type1 and type2, with self.subst map
  /// The function will try to find a new substitution map `subst` or use existing `self.subst`
  /// (map of name->Type) which makes type1 and type2 identical.
  ///
  /// Return: `Ok(None)` - types match, `Ok(Some(subst))` - types match with some new substitution
  ///   (we merge it into `self.subst`).
  fn unify(&self, module: &ErlModule,
           type1: &ErlType, type2: &ErlType) -> ErlResult<Option<SubstMap>> {
    if type1 == type2 {
      return Ok(None); // no substitution required
    }

    match type1 {
      ErlType::TVar(tv1) => {
        return self.unify_variable(module, tv1, type2);
      }

      ErlType::Fn(fun1) => {
        if let ErlType::Fn(fun2) = type2 {
          // match two function types
          return self.unify_fn(module, fun1, fun2);
        }
      }

      ErlType::FnClause(fc1) => {
        if let ErlType::FnClause(fc2) = type2 {
          return self.unify_fn_clause(module, fc1, fc2);
        }
      }

      _any_type1 => {
        // Union should not be broken into subtypes, match left equation part directly vs the union
        if let ErlType::Union(types) = type2 {
          if self.unify_check_in_union(module, &type1, &types) {
            return Ok(None); // no substitution required, type is part of the union on the right
          }
        }
      }
    }

    //
    // Reverse rules type2 then type1
    //
    match type2 {
      ErlType::Number => {
        if let ErlType::Number | ErlType::Integer(_) | ErlType::AnyInteger | ErlType::Float = type1 {
          return Ok(None); // no substitution required
        }
      }
      ErlType::AnyInteger => if let ErlType::Integer(_) | ErlType::AnyInteger = type1 {
        return Ok(None); // no substitution required
      },
      ErlType::Integer(c2) => match type1 {
        // Any numbers and number sets will match a number
        ErlType::Integer(c1) if c1 == c2 => return Ok(None), // no substitution required
        _any_type1 => {}
      },
      ErlType::AnyList => if let ErlType::List(_) = type1 {
        return Ok(None); // no substitution required
      },
      ErlType::AnyFn => if let ErlType::Fn(_) = type1 {
        return Ok(None); // no substitution required
      },
      ErlType::List(elem2) => if let ErlType::List(elem1) = type1 {
        return self.unify(module, elem1, elem2);
      },
      ErlType::AnyTuple => if let ErlType::Tuple(_) = type1 {
        return Ok(None); // no substitution required
      },
      _any_type2 => {}
    }

    Err(ErlError::from(TypeError::TypesDontMatch {
      t1: type1.clone(),
      t2: type2.clone(),
    }))
  }

  /// Whether any member of type union matches type t?
  fn unify_check_in_union(&self, env: &ErlModule,
                          t: &ErlType, union: &BTreeSet<ErlType>) -> bool {
    union.iter().any(|member| {
      self.unify(env, &t, &member).is_ok()
    })
  }

  /// Try find a substitution where `tvar` will be identical to `ty` on the right
  fn unify_variable(&self, module: &ErlModule,
                    tvar: &TypeVar, ty: &ErlType) -> ErlResult<Option<SubstMap>> {
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
    let mut subst: SubstMap = Default::default();
    subst.insert(*tvar, ty.clone());
    Ok(Some(subst))
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
      ErlType::Fn(fun_type) => {
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

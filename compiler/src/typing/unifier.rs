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

type SubstMap = HashMap<TypeVar, ErlType>;

/// A program AST is analyzed and type equations are generated outside of this struct.
/// This struct contains substitution map with found solutions for the type equations.
/// This struct is able to provide type inference for any piece of AST thereafter.
pub struct Unifier {
  /// Substitution map containing solutions matching type variables to real types
  pub subst: SubstMap,
}

impl Unifier {
  /// Goes through all generated type equations and applies self.unify() to arrive to a solution
  pub fn unify_all_equations(equations: Vec<TypeEquation>) -> ErlResult<Self> {
    let mut unifier = Self { subst: HashMap::new() };
    let errors: Vec<ErlError> = equations.iter()
        .map(|eq| {
          unifier.unify(eq.left.clone(), eq.right.clone())
        })
        .filter(Result::is_err)
        .map(Result::unwrap_err)
        .collect();
    if !errors.is_empty() {
      return Err(ErlError::Multiple(errors));
    }
    Ok(unifier)
  }

  /// Unify two types type1 and type2, with self.subst map
  /// Updates self.subst (map of name->Type) with new record which unifies type1 and type2,
  /// and returns true. Returns false if they can't be unified.
  fn unify(&mut self, type1: ErlType, type2: ErlType) -> ErlResult<()> {
    if type1 == type2 {
      return Ok(());
    }

    if let ErlType::TypeVar(tv) = type1 {
      return self.unify_variable(tv, type2);
    }

    if let ErlType::Function { arg_ty: arg_ty1, ret: ret1, .. } = &type1 {
      if let ErlType::Function { arg_ty: arg_ty2, ret: ret2, .. } = &type2 {
        if arg_ty1.len() != arg_ty2.len() {
          return Err(ErlError::from(TypeError::FunAritiesDontMatch));
        }

        // Unify functions return types
        self.unify(*ret1.clone(), *ret2.clone())?;

        // Then unify each arg of function1 with corresponding arg of function2
        let errors: Vec<ErlError> = arg_ty1.iter().zip(arg_ty2.iter())
            .map(|(t1, t2)| {
              self.unify(t1.clone(), t2.clone())
            })
            .filter(Result::is_err)
            .map(Result::unwrap_err)
            .collect();
        if !errors.is_empty() {
          return Err(ErlError::Multiple(errors));
        }
        return Ok(());
      }
    }

    // Union should not be broken into subtypes
    if let ErlType::Union(types) = &type2 {
      if self.check_in_union(&type1, types) {
        return Ok(());
      }
    }

    Err(ErlError::from(TypeError::TypesDontMatch {
      t1: type1.clone(),
      t2: type2.clone(),
    }))
  }

  /// Whether any member of type union matches type t?
  fn check_in_union(&mut self, t: &ErlType, union: &Vec<ErlType>) -> bool {
    union.iter().any(|member| {
      self.unify(t.clone(), member.clone()).is_ok()
    })
  }

  fn unify_variable(&mut self, tvar: TypeVar, ty: ErlType) -> ErlResult<()> {
    if let Entry::Occupied(entry1) = self.subst.entry(tvar.clone()) {
      let subst_entry = entry1.get().clone();
      return self.unify(subst_entry, ty);
    }

    if let ErlType::TypeVar { .. } = ty {
      if let Entry::Occupied(entry2) = self.subst.entry(tvar.clone()) {
        let subst_entry = entry2.get().clone();
        return self.unify(ErlType::TypeVar(tvar), subst_entry);
      }
    }

    if self.occurs_check(&tvar, &ty) {
      return Err(ErlError::from(TypeError::OccursCheckFailed { tvar, ty }));
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
      ErlType::TypeVar(ty_inner) if ty_inner == tv => { return true; }
      ErlType::Function { arg_ty, ret, .. } => {
        return self.occurs_check(tv, ret) ||
            arg_ty.iter().any(|a| self.occurs_check(tv, a));
      }
      ErlType::Union(members) => {
        members.iter().any(|m| self.occurs_check(tv, m))
      }
      // Any simple type cannot have typevar tv in it, so all false
      simple_type if simple_type.is_simple_value_type() => false,

      _ => todo!("Don't know how to occurs_check on {:?} and {:?}", tv, ty)
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

    if let ErlType::TypeVar(tvar) = &ty {
      match self.subst.entry(tvar.clone()) {
        Entry::Occupied(entry) => {
          let entry_val = entry.get().clone();
          return self.infer_type(entry_val);
        },
        Entry::Vacant(_) => return ty,
      }
    }

    if let ErlType::Function {arg_ty, ret, name} = &ty {
      let new_fn = ErlType::Function {
        name: name.clone(),
        arg_ty: arg_ty.iter()
            .map(|t| self.infer_type(t.clone()))
            .collect(),
        ret: Box::new(self.infer_type(*ret.clone()))
      };
      return new_fn;
    }

    ErlType::None
  }

  /// Finds the type of the expression for the given substitution.
  pub fn infer_ast(&mut self, expr: Rc<ErlAst>) -> ErlType {
    self.infer_type(expr.get_type())
  }
}
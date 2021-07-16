use crate::typing::equation::TypeEquation;
use crate::typing::erl_type::ErlType;
use std::collections::HashMap;
use crate::erl_error::{ErlResult, ErlError};
use crate::typing::error::TypeError;
use crate::typing::typevar::TypeVar;
use std::collections::hash_map::Entry;

type SubstMap = HashMap<TypeVar, ErlType>;

pub struct Unifier {
  subst: SubstMap,
}

impl Unifier {
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
      if let ErlType::Function { arg_ty: arg_ty2, ret: ret2, .. } = type2 {
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

    Err(ErlError::from(TypeError::TypesDontMatch {
      t1: type1.clone(),
      t2: type2.clone(),
    }))
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
      ErlType::Function {arg_ty, ret, ..} => {
        return self.occurs_check(tv, ret) ||
            arg_ty.iter().any(|a| self.occurs_check(tv, a));
      }
      _ => todo!("Don't know how to occurs_check on {:?} and {:?}", tv, ty)
    }

    // false
  }

  // pub fn get_expression_type(&self, expr: Rc<ErlAst>, rename_types: bool) -> ErlType {
  //
  // }
}
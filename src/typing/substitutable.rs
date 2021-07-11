use std::collections::HashSet;
use std::rc::Rc;

use crate::typing::erltype::{TVar, Type};
use crate::typing::subst::Subst;
use std::collections::hash_map::Entry;
use std::borrow::BorrowMut;
use crate::erl_error::ErlResult;
use crate::erl_error::ErlError::{TypeError, PpParse};
use crate::typing::erltype;
use crate::typing::polymorphic::Scheme;
use crate::typing::type_env::TypeEnv;

pub trait Substitutable {
  /// Apply given substitution over the structure of the type, replacing variables
  fn apply(&self, sub: Rc<Subst>) -> Self;

  /// For a type, return set of all typevars in that type
  fn find_typevars(&self) -> HashSet<TVar>;
}

impl Substitutable for Type {
  //   apply _ (TCon a)       = TCon a
  //   apply s t@(TVar a)     = Map.findWithDefault t a s
  //   apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  fn apply(&self, sub: Rc<Subst>) -> Self {
    match self {
      Type::TConst(_) => self.clone(), // no change, const is not a typevar
      Type::TVar(t_var) => {
        // Substitute one typevar from the sub hashmap, and return it
        match sub.types.entry(t_var.clone()) {
          Entry::Occupied(found) => found.get().clone(),
          Entry::Vacant(_) => self.clone(), // no change, can't substitute
        }
      }
      Type::TArr { left, right } => {
        // Return new arrow where left and right have substitution applied to them
        let l_result = left.apply(sub.clone())?;
        let r_result = right.apply(sub.clone())?;
        Type::TArr {
          left: Rc::from(l_result),
          right: Rc::from(r_result),
        }
      }
    }
  }

  //   ftv TCon{}         = Set.empty
  //   ftv (TVar a)       = Set.singleton a
  //   ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  fn find_typevars(&self) -> HashSet<TVar> {
    match self {
      Type::TConst(_) => HashSet::default(), // const contains no typevars in it
      Type::TVar(a) => {
        // Return set of 1 element
        let mut result = HashSet::with_capacity(1);
        result.insert(a);
        result
      }
      Type::TArr { left, right } => {
        // Merge typevars found in left and right sides of the arrow
        let mut l_set = left.find_typevars();
        l_set.extend(&right.find_typevars());
        l_set
      }
    }
  }
}

impl Substitutable for Scheme {
  fn apply(&self, sub: Rc<Subst>) -> Self {
    // apply s (Forall as t)   = Forall as $ apply s' t
    // where s' = foldr Map.delete s as
    let inner_type_result = self.ty.apply(sub.clone());
    Scheme {
      type_vars: self.type_vars.clone(),
      ty: Rc::from(inner_type_result),
    }
  }

  fn find_typevars(&self) -> HashSet<TVar> {
    // ftv (Forall as t) = ftv t `Set.difference` Set.fromList as
    let result = self.type_vars.find_typevars();
    result.difference(&self.type_vars)
        .into()
  }
}

// instance Substitutable a => Substitutable [a] where
impl<T> Substitutable for HashSet<T> where T: Substitutable {
  // apply = fmap . apply
  fn apply(&self, sub: Rc<Subst>) -> Self {
    self.iter()
        .map(|t| t.apply(sub))
        .collect()
  }

  // ftv   = foldr (Set.union . ftv) Set.empty
  fn find_typevars(&self) -> HashSet<TVar> {
    // Union find_typevars() of all set items
    self.iter()
        .fold(HashSet::new(),
              |accum, elem| {
                let ftv = elem.find_typevars();
                accum.union(&ftv)
                    .collect()
              })
  }
}

// instance Substitutable TypeEnv where
impl Substitutable for TypeEnv {
  // apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  fn apply(&self, sub: Rc<Subst>) -> Self {
    todo!()
  }

  // ftv (TypeEnv env) = ftv $ Map.elems env
  fn find_typevars(&self) -> HashSet<TVar> {
    todo!()
  }
}
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
      Type::TConst(_) => Self, // no change, const is not a typevar
      Type::TVar(t_var) => {
        // Substitute one typevar from the sub hashmap, and return it
        match sub.types.entry(t_var.clone()) {
          Entry::Occupied(found) => found.into(),
          Entry::Vacant(_) => Self, // no change, can't substitute
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
  fn find_typevars<T>(&self) -> HashSet<TVar> {
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
      ty: Rc::from(inner_type_result)
    }
  }

  fn find_typevars(&self) -> HashSet<TVar> {
    // ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

  }
}

// instance Substitutable a => Substitutable [a] where
// apply = fmap . apply
// ftv   = foldr (Set.union . ftv) Set.empty

// instance Substitutable TypeEnv where
// apply s (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
// ftv (TypeEnv env) = ftv $ Map.elems env
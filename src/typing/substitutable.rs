use std::collections::{HashSet, };
use std::iter::{Iterator};

use crate::typing::erltype::{TVar, Type};
use crate::typing::subst::Subst;
use std::collections::hash_map::Entry;
use crate::typing::polymorphic::Scheme;
use crate::typing::type_env::TypeEnv;
use std::ops::{Deref};
use std::hash::Hash;

pub trait Substitutable {
  /// Apply given substitution over the structure of the type, replacing variables
  fn apply(&self, sub: &mut Subst) -> Self;

  /// For a type, return set of all typevars in that type
  fn find_typevars(&self) -> HashSet<TVar>;
}

impl Substitutable for Type {
  //   apply _ (TCon a)       = TCon a
  //   apply s t@(TVar a)     = Map.findWithDefault t a s
  //   apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  fn apply(&self, sub: &mut Subst) -> Self {
    match self {
      Type::TConst(_) => self.clone(), // no change, const is not a typevar
      Type::TVar(t_var) => {
        // Substitute one typevar from the sub hashmap, and return it
        match sub.types.entry(t_var.clone()) {
          Entry::Occupied(found) => found.get().deref().clone(),
          Entry::Vacant(_) => self.clone(), // no change, can't substitute
        }
      }
      Type::TArr { left, right } => {
        // Return new arrow where left and right have substitution applied to them
        let l_result = left.apply(sub);
        let r_result = right.apply(sub);
        Type::TArr {
          left: Box::from(l_result),
          right: Box::from(r_result),
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
        result.insert(a.clone());
        result
      }
      Type::TArr { left, right } => {
        // Merge typevars found in left and right sides of the arrow
        let mut l_set = left.find_typevars();
        right.find_typevars().into_iter().for_each(|r| {
          l_set.insert(r.clone());
        });
        l_set
      }
    }
  }
}

impl Substitutable for Scheme {
  fn apply(&self, sub: &mut Subst) -> Self {
    // apply s (Forall as t)   = Forall as $ apply s' t
    // where s' = foldr Map.delete s as
    let inner_type_result = self.ty.apply(sub);
    Scheme {
      type_vars: self.type_vars.clone(),
      ty: inner_type_result,
    }
  }

  fn find_typevars(&self) -> HashSet<TVar> {
    // ftv (Forall as: [TVar] t: Type) = ftv t `Set.difference` Set.fromList as
    let result = self.ty.find_typevars();
    result.difference(&self.type_vars)
        .cloned()
        .collect()
  }
}

// Impl 1: For HashSet of Substitutable (does not work well with Schema type as its not hashable)
// instance Substitutable a => Substitutable [a] where
impl<TItem: Substitutable + Eq + Hash> Substitutable for HashSet<TItem>
{
  // apply = fmap . apply
  fn apply(&self, sub: &mut Subst) -> Self {
    self.into_iter()
        .map(|t| t.apply(sub))
        .collect()
  }

  // ftv   = foldr (Set.union . ftv) Set.empty
  fn find_typevars(&self) -> HashSet<TVar> {
    // Union find_typevars() of all set items
    self.into_iter()
        .fold(HashSet::new(),
              |accum, elem| {
                let ftv = elem.find_typevars();
                accum.union(&ftv)
                    .cloned()
                    .collect()
              })
  }
}

// Impl 2: For vector of Substitutable
// instance Substitutable a => Substitutable [a] where
impl<TItem: Substitutable + Eq> Substitutable for Vec<TItem>
{
  // apply = fmap . apply
  fn apply(&self, sub: &mut Subst) -> Self {
    self.into_iter()
        .map(|t| t.apply(sub))
        .collect()
  }

  // ftv   = foldr (Set.union . ftv) Set.empty
  fn find_typevars(&self) -> HashSet<TVar> {
    // Union find_typevars() of all set items
    self.into_iter()
        .fold(HashSet::new(),
              |accum, elem| {
                let ftv = elem.find_typevars();
                accum.union(&ftv)
                    .cloned()
                    .collect()
              })
  }
}

// instance Substitutable TypeEnv where
impl Substitutable for TypeEnv {
  // apply s: Subst (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  fn apply(&self, sub: &mut Subst) -> Self {
    let out_env = self.env.iter()
        .map(|(k, v)| {
          return (k.clone(), v.apply(sub));
        })
        .collect();
    TypeEnv {
      env: out_env
    }
  }

  // ftv (TypeEnv env) = ftv $ Map.elems env
  fn find_typevars(&self) -> HashSet<TVar> {
    let env_elems: Vec<Scheme> = self.env
        .values()
        .cloned()
        .collect();
    env_elems.find_typevars()
  }
}

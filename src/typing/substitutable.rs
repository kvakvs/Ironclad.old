use std::collections::{HashSet, HashMap};
use std::iter::{Iterator};

use crate::typing::erltype::{TVar, Type};
use crate::typing::subst::SubstitutionMap;
use std::collections::hash_map::Entry;
use crate::typing::polymorphic::Scheme;
use crate::typing::type_env::TypeEnv;
use std::ops::{Deref};

/// Temporary wrapper for types which can be substituted
/// Only lives as long as the input lives.
/// Extract the results using Self::into_*() helpers.
#[derive(Debug, Eq, PartialEq)]
pub enum Substitutable<'a> {
  /// Borrowed type
  RefType(&'a Type),
  /// New type, cloned or created
  Type(Type),
  /// Borrowed scheme
  RefScheme(&'a Scheme),
  /// New scheme, cloned or created
  Scheme(Scheme),
  // /// Hashset of Substitutables
  // HashSet(HashSet<Substitutable<'a>>),
  /// Vector of Substitutables
  Vector(Vec<Substitutable<'a>>),
  /// Borrowed TypeEnv
  RefTypeEnv(&'a TypeEnv),
  /// Newly created or cloned TypeEnv
  TypeEnv(TypeEnv),
}

// pub trait Substitutable {
//   /// Apply given substitution over the structure of the type, replacing variables
//   fn apply(&self, sub: &mut Subst) -> Self;
//
//   /// For a type, return set of all typevars in that type
//   fn find_typevars(&self) -> HashSet<TVar>;
// }

impl<'a> Substitutable<'a> {
  pub fn into_type(self) -> Type {
    match self {
      Substitutable::RefType(t) => t.clone(),
      Substitutable::Type(t) => t,
      _ => unreachable!("{:?} is expected to be a Type", self),
    }
  }

  pub fn into_scheme(self) -> Scheme {
    match self {
      Substitutable::RefScheme(s) => s.clone(),
      Substitutable::Scheme(s) => s,
      _ => unreachable!("{:?} is expected to be a Scheme", self),
    }
  }

  /// Apply given substitution over the structure of the type, replacing variables
  pub fn apply(self, sub: &mut SubstitutionMap) -> Substitutable<'a> {
    match self {
      Substitutable::RefType(as_type) => Self::apply_to_type(as_type, sub),
      Substitutable::Type(_) => unreachable!("Must never accept NewType"),
      Substitutable::RefScheme(as_scheme) => Self::apply_to_scheme(as_scheme, sub),
      Substitutable::Scheme(_) => unreachable!("Must never accept NewScheme"),
      // Substitutable::HashSet(as_hs) => Self::apply_to_hashset(&as_hs, sub),
      Substitutable::Vector(as_vec) => Self::apply_to_vec(as_vec, sub),
      Substitutable::RefTypeEnv(as_te) => Self::apply_to_typeenv(as_te, sub),
      Substitutable::TypeEnv(_) => unreachable!("Must never accept NewTypeEnv"),
    }
  }

  //   apply _ (TCon a)       = TCon a
  //   apply s t@(TVar a)     = Map.findWithDefault t a s
  //   apply s (t1 `TArr` t2) = apply s t1 `TArr` apply s t2
  /// Apply substitution to Type t
  fn apply_to_type(t: &'a Type, sub: &mut SubstitutionMap) -> Substitutable<'a> {
    match t {
      Type::Const(_) => Substitutable::RefType(t), // no change, const is not a typevar

      Type::Var(t_var) => {
        // Substitute one typevar from the sub hashmap, and return it
        match sub.types.entry(t_var.clone()) {
          Entry::Occupied(found) => {
            Substitutable::Type(found.get().deref().clone())
          }
          Entry::Vacant(_) => {
            // no change, can't substitute
            Substitutable::RefType(t)
          }
        }
      }

      Type::Arrow { left, right } => {
        // Return new arrow where left and right have substitution applied to them
        let l_result = Substitutable::RefType(&left).apply(sub);
        let r_result = Substitutable::RefType(&right).apply(sub);

        Substitutable::Type(Type::Arrow {
          left: Box::from(l_result.into_type()),
          right: Box::from(r_result.into_type()),
        })
      }
    }
  }

  fn apply_to_scheme(s: &Scheme, sub: &mut SubstitutionMap) -> Substitutable<'a> {
    // apply s (Forall as t)   = Forall as $ apply s' t
    // where s' = foldr Map.delete s as
    let inner_type_result = Substitutable::RefType(&s.ty)
        .apply(sub)
        .into_type();
    Substitutable::Scheme(Scheme {
      type_vars: s.type_vars.clone(),
      ty: inner_type_result,
    })
  }

  // // apply = fmap . apply
  // fn apply_to_hashset(hs: &HashSet<Substitutable<'a>>, sub: &mut SubstitutionMap) -> Substitutable<'a> {
  //   let result = hs.into_iter()
  //       .map(|t| t.apply(sub))
  //       .collect();
  //   Substitutable::HashSet(result)
  // }

  // apply = fmap . apply
  fn apply_to_vec(vec: Vec<Substitutable<'a>>, sub: &mut SubstitutionMap) -> Substitutable<'a> {
    let result = vec.into_iter()
        .map(|t| t.apply(sub))
        .collect();
    Substitutable::Vector(result)
  }

  // apply s: Subst (TypeEnv env) =  TypeEnv $ Map.map (apply s) env
  fn apply_to_typeenv(te: &TypeEnv, sub: &mut SubstitutionMap) -> Substitutable<'a> {
    let out_env: HashMap<TVar, Scheme> = te.env.iter()
        .map(|(k, scheme)| {
          let new_scheme = Substitutable::RefScheme(&scheme).apply(sub).into_scheme();
          return (k.clone(), new_scheme);
        })
        .collect();
    Substitutable::TypeEnv(TypeEnv { env: out_env })
  }

  pub fn find_typevars(self) -> HashSet<TVar> {
    match self {
      Substitutable::RefType(as_type) => Self::find_typevars_in_type(as_type),
      Substitutable::Type(_) => unreachable!("Must never accept NewType"),
      Substitutable::RefScheme(as_scheme) => Self::find_typevars_in_scheme(as_scheme),
      Substitutable::Scheme(_) => unreachable!("Must never accept NewScheme"),
      //Substitutable::HashSet(as_hs) => Self::find_typevars_in_hashset(&as_hs),
      Substitutable::Vector(as_vec) => Self::find_typevars_in_vec(as_vec),
      Substitutable::RefTypeEnv(as_te) => Self::find_typevars_in_typeenv(as_te),
      Substitutable::TypeEnv(_) => unreachable!("Must never accept NewTypeEnv"),
    }
  }

  // ftv TCon{}         = Set.empty
  // ftv (TVar a)       = Set.singleton a
  // ftv (t1 `TArr` t2) = ftv t1 `Set.union` ftv t2
  fn find_typevars_in_type(t: &Type) -> HashSet<TVar> {
    match t {
      Type::Const(_) => HashSet::default(), // const contains no typevars in it
      Type::Var(a) => {
        // Return set of 1 element
        let mut result = HashSet::with_capacity(1);
        result.insert(a.clone());
        result
      }
      Type::Arrow { left, right } => {
        // Merge typevars found in left and right sides of the arrow
        let mut l_set = Substitutable::RefType(&left).find_typevars();
        Substitutable::RefType(&right).find_typevars()
            .into_iter()
            .for_each(|r| {
              l_set.insert(r.clone());
            });
        l_set
      }
    }
  }

  // ftv (Forall as: [TVar] t: Type) = ftv t `Set.difference` Set.fromList as
  fn find_typevars_in_scheme(s: &Scheme) -> HashSet<TVar> {
    let result = Substitutable::RefType(&s.ty).find_typevars();
    result.into_iter()
        .filter(|item| !s.type_vars.contains(item))
        .collect()
  }

  // // ftv   = foldr (Set.union . ftv) Set.empty
  // fn find_typevars_in_hashset(hs: &HashSet<Substitutable>) -> HashSet<TVar> {
  //   // Union find_typevars() of all set items
  //   hs.into_iter()
  //       .fold(HashSet::new(),
  //             |accum, elem| {
  //               let ftv = elem.find_typevars();
  //               accum.union(&ftv)
  //                   .cloned()
  //                   .collect()
  //             })
  // }

  // ftv   = foldr (Set.union . ftv) Set.empty
  fn find_typevars_in_vec(vec: Vec<Substitutable>) -> HashSet<TVar> {
    // Union find_typevars() of all set items
    vec.into_iter()
        .fold(HashSet::new(),
              |accum, elem| {
                let ftv = elem.find_typevars();
                accum.union(&ftv)
                    .cloned()
                    .collect()
              })
  }

  // ftv (TypeEnv env) = ftv $ Map.elems env
  fn find_typevars_in_typeenv(te: &TypeEnv) -> HashSet<TVar> {
    let env_elems: Vec<Substitutable> = te.env.values()
        .map(|scheme| Substitutable::RefScheme(scheme))
        .collect();
    Substitutable::Vector(env_elems).find_typevars()
  }
}

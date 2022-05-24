//! Checks whether types are subtypes of other types

use crate::literal::Literal;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_clause_type::FnClauseType;
use std::ops::Deref;
use std::sync::Arc;

/// Hosts code to check the is-subtype-of relation
pub struct SubtypeChecker {}

impl SubtypeChecker {
  /// is_subtype check for `Option<Erltype>` which treats `None` as `[]`
  pub fn is_subtype_for_list_tail(
    sub_ty: &Option<Arc<ErlType>>,
    sup_ty: &Option<Arc<ErlType>>,
  ) -> bool {
    let nil = Arc::new(ErlType::Nil); // TODO: Some global NIL for all our NIL needs?
    let subt = sub_ty.clone().unwrap_or_else(|| nil.clone());
    let supt = sup_ty.clone().unwrap_or(nil);
    Self::is_subtype(&subt, &supt)
  }

  /// Checks whether sub_ty is a subtype of super_ty
  pub fn is_subtype(sub_ty: &ErlType, super_ty: &ErlType) -> bool {
    match super_ty.deref() {
      equal_supertype if equal_supertype.eq(sub_ty) => true, // equal types are mutual subtypes
      ErlType::Typevar(super_tv) => {
        // unwrap typevar for sub and super into nested types and try again
        match sub_ty.deref() {
          ErlType::Typevar(sub_tv) => Self::is_subtype(&sub_tv.ty, &super_tv.ty),
          _ => Self::is_subtype(sub_ty, &super_tv.ty),
        }
      }
      ErlType::Any => true,   // any includes all subtypes
      ErlType::None => false, // none includes nothing
      ErlType::Atom => Self::is_subtype_of_atom(sub_ty),
      ErlType::Boolean => Self::is_subtype_of_boolean(sub_ty),
      ErlType::Number => Self::is_subtype_of_number(sub_ty),
      ErlType::Float => Self::is_subtype_of_float(sub_ty),
      ErlType::Integer => Self::is_subtype_of_integer(sub_ty),
      // ErlType::IntegerRange { from, to } => { todo: only singletons and nested ranges and must be from <= n <= to }
      ErlType::AnyTuple => Self::is_subtype_of_anytuple(sub_ty),
      ErlType::Tuple { elements: supertype_elements } => {
        Self::is_subtype_of_tuple(supertype_elements, sub_ty)
      }
      // ErlType::Record { .. } => {}
      ErlType::AnyList => Self::is_subtype_of_anylist(sub_ty),
      ErlType::List { elements: supertype_elements, tail: supertype_tail } => {
        Self::is_subtype_of_list(supertype_elements, supertype_tail, sub_ty)
      }
      ErlType::StronglyTypedList { elements: supertype_elements, tail: supertype_tail } => {
        Self::is_subtype_of_strongly_typed_list(supertype_elements, supertype_tail, sub_ty)
      }
      ErlType::Nil => false, // can only include other nil

      ErlType::AnyMap => matches!(sub_ty, ErlType::Map { .. }),
      // ErlType::Map { .. } => {}
      ErlType::AnyBinary => matches!(sub_ty, ErlType::Binary { .. }),
      // An equal ironclad_exe type can be a subtype of ironclad_exe, no other matches it (checked at the top)
      ErlType::Binary { .. } => false,

      ErlType::AnyFn => {
        matches!(sub_ty, ErlType::Fn { .. } | ErlType::FnRef { .. } | ErlType::Lambda)
      }
      // only can be subtype of self (equality checked at the top)
      ErlType::Fn(fn_type) => Self::is_subtype_of_fn(fn_type.arity(), fn_type.clauses(), sub_ty),

      ErlType::FnRef { .. } | ErlType::Lambda => false,

      // only can be subtype of self (equality checked at the top)
      ErlType::Pid | ErlType::Reference | ErlType::Port => false,

      ErlType::Singleton { .. } => false,

      _ => unimplemented!(
        "Subtype check for sub={} in super={}\nsub={:?}\nsuper={:?}",
        sub_ty,
        super_ty,
        sub_ty,
        super_ty
      ),
    }
  }

  /// Checks whether sub_ty matches an atom() type.
  /// Atoms include other atoms, booleans, and singleton atom values
  fn is_subtype_of_atom(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Boolean => true,
      ErlType::Singleton { val } => {
        // Singleton atom values are a subtype of atom
        matches!(val.deref(), Literal::Atom(_))
      }
      _other => false,
    }
  }

  /// Checks whether sub_ty matches a boolean() type.
  /// Boolean type includes all boolean, and singleton values 'true' or 'false'
  fn is_subtype_of_boolean(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Singleton { val } => {
        if let Literal::Atom(s) = val.deref() {
          s == "true" || s == "false"
        } else {
          false
        }
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a number() type.
  /// Numbers can include floats, integers and ranges, and singletons of integer
  fn is_subtype_of_number(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Float | ErlType::Integer | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        // Floats and integer singletons are subtype of number
        matches!(val.deref(), Literal::Float(_) | Literal::Integer(_))
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a float() type.
  /// Floats include integers, other floats, and singletons of integer and float
  fn is_subtype_of_float(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Integer | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        // Floats and integer singletons are subtype of float
        matches!(val.deref(), Literal::Float(_) | Literal::Integer(_))
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches an integer() type.
  /// integer() includes integer(), ranges, and singletons of integer
  fn is_subtype_of_integer(sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Integer | ErlType::IntegerRange { .. } => true,
      ErlType::Singleton { val } => {
        // Integer singletons are subtype of integer
        matches!(val.deref(), Literal::Integer(_))
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a list() type.
  /// A list() includes any other lists() and nil []
  fn is_subtype_of_anylist(sub_ty: &ErlType) -> bool {
    matches!(sub_ty, ErlType::List { .. } | ErlType::StronglyTypedList { .. } | ErlType::Nil)
  }

  /// Checks whether sub_ty matches a list `[supertype_elements() | supertype_tail()]` type.
  fn is_subtype_of_list(
    supertype_elements: &ErlType,
    supertype_tail: &Option<Arc<ErlType>>,
    sub_ty: &ErlType,
  ) -> bool {
    match sub_ty {
      // For superlist to include a sublist
      ErlType::List { elements: subtype_elements, tail: subtype_tail } => {
        Self::is_subtype_for_list_tail(subtype_tail, supertype_tail)
          && subtype_elements.is_subtype_of(supertype_elements)
      }
      // For superlist to include typed sublist
      ErlType::StronglyTypedList { elements: subtype_elements, tail: subtype_tail } => {
        Self::is_subtype_for_list_tail(subtype_tail, supertype_tail)
          && subtype_elements
            .iter()
            .all(|subt| subt.is_subtype_of(supertype_elements))
      }
      ErlType::Nil => true,
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a list `[supertype_elements() | supertype_tail()]` type.
  fn is_subtype_of_strongly_typed_list(
    supertype_elements: &[Arc<ErlType>],
    supertype_tail: &Option<Arc<ErlType>>,
    sub_ty: &ErlType,
  ) -> bool {
    match sub_ty {
      // For typed list to include a list
      ErlType::List { elements: subtype_elements, tail: subtype_tail } => {
        // Sublist type must be subtype of each superlist element
        Self::is_subtype_for_list_tail(subtype_tail, supertype_tail)
          && supertype_elements
            .iter()
            .all(|supt| subtype_elements.is_subtype_of(supt))
      }
      // For typed superlist to include another typed sublist
      ErlType::StronglyTypedList { elements: subtype_elements, tail: subtype_tail } => {
        // Length must match
        // AND Each element of sublist must be subtype of corresponding element of superlist
        subtype_elements.len() == supertype_elements.len()
          && Self::is_subtype_for_list_tail(subtype_tail, supertype_tail)
          && subtype_elements
            .iter()
            .zip(supertype_elements.iter())
            .all(|(subt, supert)| subt.is_subtype_of(supert))
      }
      ErlType::Nil => true,
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a tuple() type.
  /// A tuple() includes any other tuples() typed and untyped, and records
  fn is_subtype_of_anytuple(sub_ty: &ErlType) -> bool {
    matches!(sub_ty, ErlType::Tuple { .. } | ErlType::Record { .. })
  }

  /// Checks whether sub_ty matches a tuple(T1, T2, ...) type.
  /// A tuple(T1, T2, ...) only includes tuples of the same size, where each element is a subtype,
  /// and records of tuple-1 size, where subrecord's tag would serve as supertuple's first element
  fn is_subtype_of_tuple(supertuple_elements: &[Arc<ErlType>], sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Tuple { elements: subtuple_elements } => {
        // lengths must match, and each element in subtuple must be a subtype of each corresponding
        // element of the supertuple
        subtuple_elements.len() == supertuple_elements.len()
          && subtuple_elements
            .iter()
            .zip(supertuple_elements.iter())
            .all(|(subt, supert)| subt.is_subtype_of(supert))
      }
      ErlType::Record { tag: subrecord_tag, fields: subrecord_fields } => {
        // lengths must match counting the record tag as an element
        // record tag (atom) must be a subtype of 1st element of supertuple
        // remaining record fields must be subtypes of the following supertuple elements
        !supertuple_elements.is_empty()
          && subrecord_fields.len() + 1 == supertuple_elements.len()
          && ErlType::new_atom(subrecord_tag).is_subtype_of(&supertuple_elements[0])
          && subrecord_fields
            .iter()
            .zip(supertuple_elements[1..].iter())
            .all(|(sub_t, super_t)| sub_t.ty.is_subtype_of(super_t))
      }
      _ => false,
    }
  }

  /// Checks whether sub_ty matches a regular Erlang function type with possibly multiple clauses
  /// and multiple return types.
  fn is_subtype_of_fn(sup_arity: usize, sup_clauses: &[FnClauseType], sub_ty: &ErlType) -> bool {
    match sub_ty {
      ErlType::Fn(sub_fntype) => {
        // Arities must match, and
        // any clause of subfn must be compatible with any clause of (be a subtype of) superfn
        sup_arity == sub_fntype.arity()
          && sub_fntype
            .clauses()
            .iter()
            .any(|subc| subc.is_any_clause_compatible(sup_clauses))
      }
      ErlType::FnRef { .. } => unimplemented!("Matching reference to a function"),
      _ => false,
    }
  }
}
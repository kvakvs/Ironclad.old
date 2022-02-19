//! Constructors for complex ErlTypes

use std::iter;
use std::sync::Arc;
use crate::literal::Literal;
use crate::typing::erl_type::ErlType;
use crate::typing::erl_type::ErlType::UserDefinedType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::typing::fn_type::FnType;
use crate::typing::type_union::TypeUnion;
use crate::typing::typevar::Typevar;

//
// Constructors and Generators
//
impl ErlType {
  /// Clones literal's refcounted pointer and returns a singleton type
  pub fn new_singleton(lit: &Arc<Literal>) -> Arc<ErlType> {
    ErlType::Singleton { val: lit.clone() }.into()
  }

  /// Creates a new singleton atom of name `s`
  pub fn new_atom(s: &str) -> Arc<ErlType> {
    ErlType::Singleton {
      val: Literal::Atom(s.to_string()).into()
    }.into()
  }

  /// Creates a type for a proper list with a NIL tail.
  pub fn list_of(t: Arc<ErlType>) -> Arc<ErlType> {
    let result = ErlType::List {
      elements: t,
      tail: None,
    };
    result.into()
  }

  /// Creates a type for a proper list with a NIL tail.
  pub fn list_of_types(types: Vec<Arc<ErlType>>) -> Arc<ErlType> {
    let result = ErlType::StronglyTypedList {
      elements: types,
      tail: None,
    };
    result.into()
  }

  /// Creates new function type with clauses
  pub fn new_fn_type(clauses: &[FnClauseType]) -> ErlType {
    assert!(!clauses.is_empty(), "Attempt to build a fn type with zero clauses");

    let arity = clauses[0].arity();
    assert!(clauses.iter().all(|c| c.arity() == arity),
            "Attempt to build a fn type with clauses of different arity (first clause had arity {})",
            arity);

    let fn_type = FnType::new(arity, clauses);
    ErlType::Fn(fn_type.into())
  }

  /// Creates a new function type with 1 clause, a count of `any()` args and a given return type
  pub fn new_fn_type_of_any_args(arity: usize, ret_ty: &Arc<ErlType>) -> ErlType {
    let any_args = iter::repeat(()).take(arity)
        .map(|_| Typevar::new(None, None))
        .collect();
    let clause = FnClauseType::new(any_args, Typevar::from_erltype(ret_ty));
    let fn_type = FnType::new(arity, &[clause]);
    ErlType::Fn(fn_type.into())
  }

  /// Wrapper to access type union construction
  pub fn new_union(types: &[Arc<ErlType>]) -> Arc<ErlType> {
    match types.len() {
      0 => ErlType::none(),
      1 => types[0].clone(),
      _ => {
        let mut u = TypeUnion::new(types);

        u.normalize();

        match u.types().len() {
          0 => panic!("Can't create type union of 0 types after normalization"),
          1 => u.types()[0].clone(),
          _ => ErlType::Union(u).into()
        }
      }
    }
  }

  /// Construct a new tuple-type
  pub fn new_tuple(elements: &[Arc<ErlType>]) -> Arc<ErlType> {
    ErlType::Tuple {
      elements: elements.into()
    }.into()
  }

  /// Consumes argument.
  /// Construct a new tuple-type
  pub fn new_tuple_move(elements: Vec<Arc<ErlType>>) -> Arc<ErlType> {
    ErlType::Tuple { elements }.into()
  }

  /// Construct a new type variable wrapper
  pub fn new_typevar(tv: Typevar) -> Arc<ErlType> {
    ErlType::Typevar(tv).into()
  }

  /// Try match type name and arity vs known basic types
  pub fn from_name(name: String, args: &[Typevar]) -> Arc<ErlType> {
    #[allow(clippy::single_match)]
    match args.len() {
      0 => {
        match name.as_ref() {
          "any" => return ErlType::any(),
          "none" => return ErlType::none(),

          "number" => return ErlType::number(),
          "integer" => return ErlType::integer(),
          "float" => return ErlType::float(),

          "atom" => return ErlType::atom(),
          "boolean" => return ErlType::boolean(),

          "list" => return ErlType::any_list(),
          "nil" => return ErlType::nil(),

          "tuple" => return ErlType::any_tuple(),

          "pid" => return ErlType::pid(),
          "port" => return ErlType::port(),
          "reference" => return ErlType::reference(),
          _ => {}
        }
      }
      _ => {}
    }
    // We were not able to find a basic type of that name and arity
    UserDefinedType {
      name,
      args: args.to_vec(),
    }.into()
  }
}

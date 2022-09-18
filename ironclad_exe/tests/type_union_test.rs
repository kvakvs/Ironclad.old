extern crate function_name;
extern crate libironclad_erlang;

mod test_util;

use ::function_name::named;
use libironclad_erlang::error::ic_error::IroncladResult;
use libironclad_erlang::typing::erl_type::typekind::TypeKind;
use libironclad_erlang::typing::erl_type::TypeImpl;

#[named]
#[test]
/// Check that unions are capable of shrinking matching multiple types into single compound types
fn union_auto_shrink_numbers() -> IroncladResult<()> {
  test_util::start(function_name!(), "TypeUnion.ShrinkNum");
  let union_t = TypeImpl::new_unnamed(TypeKind::new_union(&[
    TypeImpl::integer(),
    TypeImpl::float(),
    TypeImpl::new_unnamed(TypeKind::new_atom("atomliteral")),
  ]));

  if let TypeKind::Union(u) = &union_t.kind {
    assert_eq!(
      u.types.len(),
      2,
      "Union of int|float|'atomliteral' length must be 2 (number() and 'atomliteral')"
    );
    assert!(
      u.contains_kind(&TypeKind::Number),
      "Union of int|float|'atomliteral' must contain number(): got {}",
      union_t
    );
    assert!(
      u.contains_kind(&TypeKind::new_atom("atomliteral")),
      "Union of int|float|'atomliteral' must contain 'atomliteral': got {}",
      union_t
    );
  } else {
    panic!("Union of int|float|'atomliteral' should be a type union, got {}", &union_t)
  }

  Ok(())
}

#[named]
#[test]
/// Check that union of 0 is None-type
fn union_auto_shrink_0() -> IroncladResult<()> {
  test_util::start(function_name!(), "TypeUnion.Shrink0");
  let union_t = TypeImpl::new_unnamed(TypeKind::new_union(&Vec::default()));
  assert!(
    union_t.as_ref().eq(&TypeImpl::none()),
    "Union type of 0 elements should be none() type"
  );
  Ok(())
}

#[named]
#[test]
/// Check that union of 1 type is that type
fn union_auto_shrink_1() -> IroncladResult<()> {
  test_util::start(function_name!(), "TypeUnion.Shrink1");
  let union_t = TypeImpl::new_unnamed(TypeKind::new_union(&vec![TypeImpl::nil()]));
  assert!(
    union_t.as_ref().eq(&TypeImpl::nil()),
    "Union type of nil should be just nil, got {}",
    union_t
  );
  Ok(())
}

#[named]
#[test]
/// Check that union of int|int is int
fn union_int_int() -> IroncladResult<()> {
  test_util::start(function_name!(), "TypeUnion.IntInt");
  let union_t =
    TypeImpl::new_unnamed(TypeKind::new_union(&vec![TypeImpl::integer(), TypeImpl::integer()]));
  assert!(
    union_t.as_ref().eq(&TypeImpl::integer()),
    "Union type of int|int should be just int(), got {}",
    union_t
  );
  Ok(())
}

#[named]
#[test]
/// Check that union of nothing is none()
fn union_none() -> IroncladResult<()> {
  test_util::start(function_name!(), "TypeUnion.None");
  let union_t = TypeImpl::new_unnamed(TypeKind::new_union(&vec![]));
  assert!(
    union_t.as_ref().eq(&TypeImpl::none()),
    "Union type of nothing should be none(), got {}",
    union_t
  );
  Ok(())
}

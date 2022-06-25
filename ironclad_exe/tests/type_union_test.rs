extern crate function_name;
extern crate libironclad_erlang;

mod test_util;

use ::function_name::named;
use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::typing::erl_type::ErlTypeImpl;
use std::ops::Deref;

#[named]
#[test]
/// Check that unions are capable of shrinking matching multiple types into single compound types
fn union_auto_shrink_numbers() -> IcResult<()> {
  test_util::start(function_name!(), "TypeUnion.ShrinkNum");
  let union_t = ErlTypeImpl::new_union(&[
    ErlTypeImpl::integer(),
    ErlTypeImpl::float(),
    ErlTypeImpl::new_atom("atomliteral").into(),
  ]);

  if let ErlTypeImpl::Union(u) = union_t.deref() {
    assert_eq!(
      u.types.len(),
      2,
      "Union of int|float|'atomliteral' length must be 2 (number() and 'atomliteral')"
    );
    assert!(
      u.contains(&ErlTypeImpl::Number),
      "Union of int|float|'atomliteral' must contain number(): got {}",
      union_t
    );
    assert!(
      u.contains(&ErlTypeImpl::new_atom("atomliteral")),
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
fn union_auto_shrink_0() -> IcResult<()> {
  test_util::start(function_name!(), "TypeUnion.Shrink0");
  let union_t = ErlTypeImpl::new_union(&Vec::default());
  assert!(
    union_t.as_ref().eq(&ErlTypeImpl::None),
    "Union type of 0 elements should be none() type"
  );
  Ok(())
}

#[named]
#[test]
/// Check that union of 1 type is that type
fn union_auto_shrink_1() -> IcResult<()> {
  test_util::start(function_name!(), "TypeUnion.Shrink1");
  let union_t = ErlTypeImpl::new_union(&vec![ErlTypeImpl::nil()]);
  assert!(
    union_t.as_ref().eq(&ErlTypeImpl::Nil),
    "Union type of nil should be just nil, got {}",
    union_t
  );
  Ok(())
}

#[named]
#[test]
/// Check that union of int|int is int
fn union_int_int() -> IcResult<()> {
  test_util::start(function_name!(), "TypeUnion.IntInt");
  let union_t = ErlTypeImpl::new_union(&vec![ErlTypeImpl::integer(), ErlTypeImpl::integer()]);
  assert!(
    union_t.as_ref().eq(&ErlTypeImpl::Integer),
    "Union type of int|int should be just int(), got {}",
    union_t
  );
  Ok(())
}

#[named]
#[test]
/// Check that union of nothing is none()
fn union_none() -> IcResult<()> {
  test_util::start(function_name!(), "TypeUnion.None");
  let union_t = ErlTypeImpl::new_union(&vec![]);
  assert!(
    union_t.as_ref().eq(&ErlTypeImpl::None),
    "Union type of nothing should be none(), got {}",
    union_t
  );
  Ok(())
}

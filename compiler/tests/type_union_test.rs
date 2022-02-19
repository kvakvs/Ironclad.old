extern crate compiler;
extern crate function_name;

mod test_util;

use std::ops::Deref;
use ::function_name::named;
use compiler::erl_error::ErlResult;
use compiler::typing::erl_type::ErlType;

#[named]
#[test]
/// Check that unions are capable of shrinking matching multiple types into single compound types
fn union_auto_shrink_numbers() -> ErlResult<()> {
  test_util::start(function_name!(), "TypeUnion.ShrinkNum");
  let union_t = ErlType::new_union(
    &[ErlType::integer(),
      ErlType::float(),
      ErlType::new_atom("atomliteral").into()]);

  if let ErlType::Union(u) = union_t.deref() {
    assert_eq!(u.types.len(), 2,
               "Union of int|float|'atomliteral' length must be 2 (number() and 'atomliteral')");
    assert!(u.contains(&ErlType::Number),
            "Union of int|float|'atomliteral' must contain number(): got {}", union_t);
    assert!(u.contains(&ErlType::new_atom("atomliteral")),
            "Union of int|float|'atomliteral' must contain 'atomliteral': got {}", union_t);
  } else {
    panic!("Union of int|float|'atomliteral' should be a type union, got {}", &union_t)
  }

  Ok(())
}

#[named]
#[test]
/// Check that union of 0 is None-type
fn union_auto_shrink_0() -> ErlResult<()> {
  test_util::start(function_name!(), "TypeUnion.Shrink0");
  let union_t = ErlType::new_union(&Vec::default());
  assert!(union_t.as_ref().eq(&ErlType::None), "Union type of 0 elements should be none() type");
  Ok(())
}

#[named]
#[test]
/// Check that union of 1 type is that type
fn union_auto_shrink_1() -> ErlResult<()> {
  test_util::start(function_name!(), "TypeUnion.Shrink1");
  let union_t = ErlType::new_union(&vec![ErlType::nil()]);
  assert!(union_t.as_ref().eq(&ErlType::Nil), "Union type of nil should be just nil, got {}", union_t);
  Ok(())
}

#[named]
#[test]
/// Check that union of int|int is int
fn union_int_int() -> ErlResult<()> {
  test_util::start(function_name!(), "TypeUnion.IntInt");
  let union_t = ErlType::new_union(&vec![ErlType::integer(), ErlType::integer()]);
  assert!(union_t.as_ref().eq(&ErlType::Integer),
          "Union type of int|int should be just int(), got {}", union_t);
  Ok(())
}

#[named]
#[test]
/// Check that union of nothing is none()
fn union_none() -> ErlResult<()> {
  test_util::start(function_name!(), "TypeUnion.None");
  let union_t = ErlType::new_union(&vec![]);
  assert!(union_t.as_ref().eq(&ErlType::None),
          "Union type of nothing should be none(), got {}", union_t);
  Ok(())
}
extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::erl_error::ErlResult;
use compiler::typing::erl_type::ErlType;
use compiler::typing::erl_type_prefab::TypePrefab;

#[named]
#[test]
/// Check that unions are capable of shrinking matching multiple types into single compound types
fn union_auto_shrink_numbers() -> ErlResult<()> {
  let t1 = ErlType::union_of(
    vec![TypePrefab::any_integer(), TypePrefab::float()],
    true);
  assert_eq!(t1, TypePrefab::number());

  Ok(())
}


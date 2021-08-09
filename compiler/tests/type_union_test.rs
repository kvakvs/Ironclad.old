extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::erl_error::ErlResult;
use compiler::typing::erl_type::ErlType;

#[named]
#[test]
/// Check that unions are capable of shrinking matching multiple types into single compound types
fn union_auto_shrink_numbers() -> ErlResult<()> {
  let t1 = ErlType::union_of(vec![ErlType::AnyInteger, ErlType::Float], true);
  assert_eq!(t1, ErlType::Number);

  Ok(())
}


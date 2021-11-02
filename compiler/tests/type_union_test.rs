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
  let t1 = ErlType::new_union(
    vec![ErlType::Integer.into(), ErlType::Float.into()]);
  if let ErlType::Union(_) = t1 {
    assert!(t1.eq(&ErlType::Number), "Not equal {:?} <=> Number", t1);
  } else {
    panic!("Constructing union from [integer(), float()] should return ErlType::Union, got {}", &t1)
  }

  Ok(())
}


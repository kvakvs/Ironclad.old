extern crate compiler;
extern crate function_name;
extern crate core;

use std::path::PathBuf;

use ::function_name::named;

use compiler::erl_error::{ErlResult};
use compiler::project::module::Module;

mod test_util;

#[named]
#[test]
fn parse_bin1() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a basic binary");

  let filename = PathBuf::from(function_name!());
  let src = "<<1, 2, 3>>";
  let module = Module::from_expr_source(&filename, src)?;
  println!("{} From «{}» parsed: {}", function_name!(), src, module.ast);
  assert!(module.ast.is_binary());

  Ok(())
}

extern crate core;
extern crate function_name;
extern crate libironclad;
extern crate libironclad_erlang;

use std::path::PathBuf;

use ::function_name::named;
use libironclad::project::module::ErlModule;
use libironclad_erlang::error::ic_error::IcResult;

mod test_util;

#[named]
#[test]
fn parse_bin1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a basic binary");
  let filename = PathBuf::from(function_name!());

  {
    let src = "<<1, 2, 3>>";
    let module = ErlModule::from_expr_source(&filename, src)?;
    println!("{} From «{}» parsed: {}", function_name!(), src, module.ast);
    assert!(module.ast.is_binary());
  }
  {
    let src = "<<X, B:3, (atom):V>>";
    let module = ErlModule::from_expr_source(&filename, src)?;
    println!("{} From «{}» parsed: {}", function_name!(), src, module.ast);
    assert!(module.ast.is_binary());
  }
  {
    let src = "<<X/binary-big-unit:33>>";
    let module = ErlModule::from_expr_source(&filename, src)?;
    println!("{} From «{}» parsed: {}", function_name!(), src, module.ast);
    assert!(module.ast.is_binary());
  }
  Ok(())
}

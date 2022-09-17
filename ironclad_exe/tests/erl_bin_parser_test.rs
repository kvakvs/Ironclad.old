extern crate core;
extern crate function_name;
extern crate libironclad_erlang;

use ::function_name::named;
use libironclad_erlang::error::ic_error::IroncladResult;

mod test_util;

#[named]
#[test]
fn parse_bin1() {
  test_util::start(function_name!(), "Parse a basic binary");

  let input1 = "<<1, 2, 3>>";
  let ast1 = test_util::parse_expr(function_name!(), input1);
  println!("{} From «{}» parsed: {:?}", function_name!(), input1, ast1);
  assert!(ast1.is_binary());
}

#[named]
#[test]
fn parse_bin2() {
  test_util::start(function_name!(), "Parse a basic binary");
  let input2 = "<<X, B:3, (atom):V>>";
  let ast2 = test_util::parse_expr(function_name!(), input2);
  println!("{} From «{}» parsed: {}", function_name!(), input2, ast2);
  assert!(ast2.is_binary());
}

#[named]
#[test]
fn parse_bin3() {
  test_util::start(function_name!(), "Parse a basic binary");

  let input3 = "<<X/binary-big-unit:33>>";
  let ast3 = test_util::parse_expr(function_name!(), input3);
  println!("{} From «{}» parsed: {}", function_name!(), input3, ast3);
  assert!(ast3.is_binary());
}

#[named]
#[test]
fn parse_bin_empty() {
  test_util::start(function_name!(), "Parse a basic binary");

  let input3 = "<<>>";
  let ast3 = test_util::parse_expr(function_name!(), input3);
  println!("{} From «{}» parsed: {}", function_name!(), input3, ast3);
  assert!(ast3.is_binary());
}

#[named]
#[test]
fn parse_nested_bin() -> IroncladResult<()> {
  test_util::start(function_name!(), "Parse nested binaries");

  let input1 = "<<<<A, 4>> || A <- <<5, 6>>>>";
  let ast1 = test_util::parse_expr(function_name!(), input1);
  // println!("{} From «{}» parsed: {:?}", function_name!(), input1, ast1);
  assert!(ast1.is_binary_comprehension());
  Ok(())
}

#[named]
#[test]
fn parse_if_with_binaries() -> IroncladResult<()> {
  test_util::start(function_name!(), "Parse some binary and empty binary inside if");

  let input1 = "
  resolve_inst({bs_match_string=I,[F,Ms,{u,Bits},{u,Off}]},_,Strings,_) ->
      String = if
  		 Len > 0 ->
  		     <<_:Off/binary,Bin:Len/binary,_/binary>> = Strings,
  		     Bin
  	     end.";
  let _m1 = test_util::parse_module(function_name!(), input1);
  Ok(())
}

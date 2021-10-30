extern crate compiler;
extern crate function_name;

mod test_util;

use ::function_name::named;
use compiler::erl_error::ErlResult;
use compiler::erlang::syntax_tree::erl_parser::{Rule};
use std::ops::Deref;
use compiler::project::module::Module;
use compiler::typing::synth::TypeBuilder;

#[named]
#[test]
fn typing_simple() -> ErlResult<()> {
  test_util::start(function_name!(), "Typing.Simple");
  let code = "hello";
  let mut module = Module::default();

  module.parse_erl_str(Rule::function_def, code)?;

  let _ast = module.core_ast.clone();

  println!("Parsed: {}", module.ast);

  println!("Synthesized type: {:?}", TypeBuilder::synthesize_from_core(&module.core_ast).deref());

  Ok(())
}

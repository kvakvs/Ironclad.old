extern crate function_name;
extern crate libironclad_erlang;

mod test_util;

use ::function_name::named;
use libironclad::project::module::ErlModule;
use libironclad_erlang::erl_syntax::erl_ast::node_impl::ErlAstType;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use libironclad_erlang::erl_syntax::parsers::parse_attr::ErlAttrParser;
use libironclad_erlang::erl_syntax::parsers::parse_type::ErlTypeParser;
use libironclad_erlang::typing::erl_type::ErlType;
use libironclad_error::ic_error::IcResult;
use libironclad_util::mfarity::MFArity;
use nom::Finish;
use std::path::PathBuf;

#[named]
#[test]
fn union_type_parse() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a multiline union type");
  let input = "-type src() :: beam_reg() |
	       {'literal',term()} |
	       {'atom',atom()} |
	       {'integer',integer()} |
	       'nil' |
	       {'float',float()}.";
  let (tail1, result1) =
    panicking_parser_error_reporter(input, ErlAttrParser::type_definition_attr(input).finish());
  assert!(tail1.is_empty(), "Not all input consumed, tail: «{}»", tail1);
  println!("Parsed: {}", result1);
  Ok(())
}

#[named]
#[test]
fn fn_generic_attr_parse1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a generic attribute without args");
  let input = "- fgsfds.\n";
  let (tail1, result1) =
    panicking_parser_error_reporter(input, ErlAttrParser::parse_generic_attr(input).finish());
  assert!(
    tail1.trim().is_empty(),
    "Not all input consumed from attr1_src, tail: «{}»",
    tail1
  );
  println!("ErlAst for «{}»: {}", input, result1);
  Ok(())
}

#[named]
#[test]
fn fn_generic_attr_parse2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a generic attribute line, consuming all as string");
  let input = "- bbbggg (ababagalamaga()) .  ";
  let (tail2, result2) =
    panicking_parser_error_reporter(input, ErlAttrParser::parse_generic_attr(input).finish());
  assert!(
    tail2.trim().is_empty(),
    "Not all input consumed from attr2_src, tail: {}",
    tail2
  );
  println!("ErlAst for attr2: {}", result2);
  Ok(())
}

#[named]
#[test]
fn fn_typespec_parse_1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse typespec syntax for a 1-clause fn");

  let filename = PathBuf::from(function_name!());

  let input = format!("-spec {}(A :: integer()) -> any().", function_name!());
  let module = ErlModule::from_fun_spec_source(&filename, &input)?;

  if let ErlAstType::FnSpec { funarity, spec, .. } = &module.ast.content {
    assert_eq!(
      funarity,
      &MFArity::new_local(function_name!(), 1),
      "Expected fnspec for '{}'/1, got spec for {}",
      function_name!(),
      funarity
    );
    let fntype = spec.as_fn_type();
    assert_eq!(fntype.clauses().len(), 1, "Expected 1 clause in typespec, got {}", module.ast);
  } else {
    panic!("Expected AST FnSpec node, but got {}", module.ast)
  }
  Ok(())
}

#[named]
#[test]
fn fn_typespec_parse_2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse typespec syntax for a 2-clause fn");

  let filename = PathBuf::from(function_name!());
  let input =
    format!("-spec {}(A :: integer()) -> any(); (B :: atom()) -> tuple().", function_name!());
  let module = ErlModule::from_fun_spec_source(&filename, &input)?;

  if let ErlAstType::FnSpec { funarity, spec, .. } = &module.ast.content {
    assert_eq!(
      funarity,
      &MFArity::new_local(function_name!(), 1),
      "Expected fnspec for 'myfun'/2, got spec for {}",
      funarity
    );
    let fntype = spec.as_fn_type();
    assert_eq!(fntype.clauses().len(), 2, "Expected 2 clauses in typespec, got {}", module.ast);
  } else {
    panic!("Expected AST FnSpec node, but got {}", module.ast)
  }

  Ok(())
}

#[named]
#[test]
fn fn_typespec_parse_when() -> IcResult<()> {
  test_util::start(function_name!(), "Parse when-part of a function type spec");

  let filename = PathBuf::from(function_name!());
  let input = format!("-spec {}(atom()) -> A when A :: tuple(). ", function_name!());
  let module = ErlModule::from_fun_spec_source(&filename, &input)?;

  assert!(module.ast.is_fn_spec());
  // TODO: Check that the spec parsed return type is tuple()
  let fn_spec = module.ast.as_fn_spec();
  let c0 = fn_spec.as_fn_type().clause(0);
  let t0 = c0.ret_type.ty.clone();
  assert!(
    t0.eq(&ErlType::any_tuple()),
    "Return type of the function spec must be tuple, but got {}",
    t0
  );

  Ok(())
}

#[named]
#[test]
fn type_parse_union() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a type union");

  let filename = PathBuf::from(function_name!());
  let src = "atom() | 42 | integer()";
  let parsed = ErlModule::from_type_source(&filename, &src)?;

  assert!(parsed.ast.is_type());
  assert!(parsed.ast.as_type().is_union());

  Ok(())
}

#[named]
#[test]
fn fn_typespec_parse_union() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function spec with type union in it");

  let filename = PathBuf::from(function_name!());
  let input = format!(
    " -spec {}(atom() | 42 | integer()) -> {{ok, any()}} | {{error, any()}} .",
    function_name!()
  );
  let module = ErlModule::from_fun_spec_source(&filename, &input)?;

  assert!(module.ast.is_fn_spec());
  // TODO: Check that the spec parsed contains the unions as written

  Ok(())
}

#[named]
#[test]
#[should_panic(expected = "All function clauses must have same arity in a typespec")]
fn fn_typespec_parse_1_2() {
  test_util::start(
    function_name!(),
    "Parse typespec syntax for 1 and 2 clause fns, must fail because mismatching arity",
  );

  let filename = PathBuf::from(function_name!());
  let input = format!("-spec {}(A) -> any(); (B, C) -> any().", function_name!());
  let module = ErlModule::from_fun_spec_source(&filename, &input);
  assert!(module.is_err(), "Parse error is expected");
}

#[named]
#[test]
fn parse_spec_test() {
  test_util::start(function_name!(), "Parse a spec from beam_a.erl");

  let filename = PathBuf::from(function_name!());
  let input = "-spec module(beam_asm:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.";
  let result = ErlModule::parse_helper(&filename, &input, ErlTypeParser::fn_spec_attr).unwrap();
  assert!(matches!(result.ast.content, ErlAstType::FnSpec { .. }));
}

#[named]
#[test]
fn parse_int_range_test() {
  test_util::start(function_name!(), "Parse an integer range");

  let filename = PathBuf::from(function_name!());
  let input = "-type reg_num() :: 0 .. 1023.";
  let result =
    ErlModule::parse_helper(&filename, &input, ErlAttrParser::type_definition_attr).unwrap();
  println!("Parsed typeattr: {:?}", result.ast);
  // let content = result.ast.children().unwrap();
  // assert!(matches!(content[0].content, ErlAstType::TypeAttr { .. }));
}

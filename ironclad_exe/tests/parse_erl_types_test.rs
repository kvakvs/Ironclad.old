mod test_util;

use function_name::named;
use libironclad_erlang::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use libironclad_erlang::erl_syntax::parsers::parse_type::parse_binary_t::{
  binary_type_head_element, binary_type_tail_element,
};
use libironclad_erlang::erl_syntax::parsers::parser_input::ParserInput;
use libironclad_erlang::project::module::module_impl::ErlModuleImpl;
use libironclad_util::mfarity::MFArity;
use nom::Finish;

#[named]
#[test]
fn parse_fntype_type() {
  test_util::start(function_name!(), "Parse a fun() type with args and return type");
  let input2 = "
  -spec f() -> fun((b_blk()|terminator(), any()) -> any()).
";
  let _ = test_util::parse_module(function_name!(), input2);
}

#[named]
#[test]
fn parse_a_spec_with_typevar() {
  test_util::start(function_name!(), "Parse a fn spec with a type variable");
  let input2 = r#"-spec normalize_test(Op, Args) -> NormalizedTest | 'none' when
      Op :: beam_ssa:op(),
      Args :: [beam_ssa:value()],
      NormalizedTest :: basic_test()."#;
  let _ = test_util::parse_module(function_name!(), input2);
}

#[named]
#[test]
fn union_type_parse() {
  test_util::start(function_name!(), "Parse a multiline union type");
  // let input = "-type src() :: {term()}.";
  let input = "-type src() :: beam_reg() |
         {'literal',term()} |
         {'atom',atom()} |
         {'integer',integer()} |
         'nil' |
         {'float',float()}.";
  // let _module = test_util::parse_module(function_name!(), input);
  let module = ErlModuleImpl::new_default();
  let tokens = test_util::tokenize(input);
  println!("TOKENS {:?}", &tokens);
  let p_input = ParserInput::new_slice(module, &tokens);
  let (_tail, result) = panicking_parser_error_reporter(
    input,
    p_input.clone(),
    libironclad_erlang::erl_syntax::preprocessor::parsers::parse_attr::parse_new_type_attr(p_input)
      .finish(),
    true,
  );
  println!("Parsed: {}", result);
}

#[named]
#[test]
fn fn_generic_attr_parse1() {
  test_util::start(function_name!(), "Parse a generic attribute without args");
  let input = "- fgsfds.\n";
  let module = test_util::parse_module(function_name!(), input);
  let fgsattr = module.root_scope.attributes.get(&"fgsfds".to_string());
  assert!(fgsattr.is_some());
  assert_eq!(fgsattr.unwrap().len(), 1);
}

#[named]
#[test]
fn fn_generic_attr_parse2() {
  test_util::start(function_name!(), "Parse a generic attribute line, consuming all as string");
  let input = "- bbbggg (ababagalamaga) .  ";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  let attr_collection = root_scope.attributes.get(&"bbbggg".to_string()).unwrap();
  assert_eq!(attr_collection.len(), 1);
  assert!(attr_collection
    .get(0)
    .expr
    .unwrap()
    .is_atom_of("ababagalamaga"));
}

#[named]
#[test]
fn fn_typespec_parse_1() {
  test_util::start(function_name!(), "Parse typespec syntax for a 1-clause fn");

  let input = format!("-spec {}(A :: integer()) -> any().", function_name!());
  let module = test_util::parse_module(function_name!(), &input);
  let root_scope = module.root_scope.clone();
  let fn_spec = root_scope
    .fn_specs
    .get(&MFArity::new_local(function_name!(), 1));
  assert!(
    fn_spec.is_some(),
    "Function spec for {}/1 not found in the root scope",
    function_name!()
  );

  // if let AstNodeType::FnSpec { funarity, spec, .. } = &nodes[0].content {
  //   assert_eq!(
  //     funarity,
  //     &MFArity::new_local(function_name!(), 1),
  //     "Expected fnspec for '{}'/1, got spec for {}",
  //     function_name!(),
  //     funarity
  //   );
  //   let fntype = spec.as_fn_type();
  //   assert_eq!(fntype.clauses().len(), 1, "Expected 1 clause in typespec, got {}", nodes[0]);
  // } else {
  //   panic!("Expected AST FnSpec node, but got {:?}", nodes)
  // }
}

#[named]
#[test]
fn fn_typespec_parse_2() {
  test_util::start(function_name!(), "Parse typespec syntax for a 2-clause fn");

  let input =
    format!("-spec {}(A :: integer()) -> any(); (B :: atom()) -> tuple().", function_name!());
  let module = test_util::parse_module(function_name!(), &input);
  let root_scope = module.root_scope.clone();
  let _spec = root_scope
    .fn_specs
    .get(&MFArity::new_local(function_name!(), 1))
    .unwrap();

  // if let AstNodeType::FnSpec { funarity, spec, .. } = &nodes[0].content {
  //   assert_eq!(
  //     funarity,
  //     &MFArity::new_local(function_name!(), 1),
  //     "Expected fnspec for 'myfun'/2, got spec for {}",
  //     funarity
  //   );
  //   let fntype = spec.as_fn_type();
  //   assert_eq!(fntype.clauses().len(), 2, "Expected 2 clauses in typespec, got {:?}", nodes);
  // } else {
  //   panic!("Expected AST FnSpec node, but got {:?}", nodes)
  // }
}

#[named]
#[test]
fn fn_typespec_parse_when() {
  test_util::start(function_name!(), "Parse when-part of a function type spec");
  let input = format!("-spec {}(atom()) -> A when A :: tuple(). ", function_name!());
  let module = test_util::parse_module(function_name!(), &input);

  // assert!(nodes[0].is_fn_spec());
  // TODO: Check that the spec parsed return type is tuple()
  // let fn_spec = nodes[0].as_fn_spec();
  // let c0 = fn_spec.as_fn_type().clause(0);
  // let t0 = c0.ret_type.ty.clone();
  // assert!(
  //   t0.eq(&ErlType::any_tuple()),
  //   "Return type of the function spec must be tuple, but got {}",
  //   t0
  // );
  let root_scope = module.root_scope.clone();
  let spec = root_scope
    .fn_specs
    .get(&MFArity::new_local(&function_name!(), 1))
    .unwrap();
  println!("Spec found: {}", spec);
}

#[named]
#[test]
fn type_parse_union() {
  test_util::start(function_name!(), "Parse a type union");

  let src = "atom() | 42 | integer()";
  let parsed = test_util::parse_type(function_name!(), &src);
  assert!(parsed.is_union());
}

#[named]
#[test]
fn fn_typespec_parse_union() {
  test_util::start(function_name!(), "Parse a function spec with type union in it");

  let input = format!(
    " -spec {}(atom() | 42 | integer()) -> {{ok, any()}} | {{error, any()}} .",
    function_name!()
  );
  let module = test_util::parse_module(function_name!(), &input);
  let root_scope = module.root_scope.clone();
  let _spec = root_scope
    .fn_specs
    .get(&MFArity::new_local(&function_name!(), 1))
    .unwrap();
  // assert!(nodes[0].is_fn_spec());
  // TODO: Check that the spec parsed contains the unions as written
}

#[named]
#[test]
#[should_panic(expected = "All function clauses must have same arity in a typespec")]
fn fn_typespec_parse_1_2() {
  test_util::start(
    function_name!(),
    "Parse typespec syntax for 1 and 2 clause fns, must fail because mismatching arity",
  );

  let input = format!("-spec {}(A) -> any(); (B, C) -> any().", function_name!());
  test_util::parse_module_unwrap(function_name!(), &input);
  // assert!(nodes.is_err(), "Parse error is expected");
}

#[named]
#[test]
fn parse_spec_test() {
  test_util::start(function_name!(), "Parse a spec from beam_a.erl");

  let input = "-spec module(beam_asm:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.";
  let _module = test_util::parse_module(function_name!(), input);
  // assert!(matches!(nodes[0].content, AstNodeType::FnSpec { .. }));
}

#[named]
#[test]
fn parse_spec_ellipsis_test() {
  test_util::start(function_name!(), "Parse a spec with an ellipsis");

  let input = "-spec atom_table(bdict()) -> {non_neg_integer(), [[non_neg_integer(),...]]}.";
  let module = test_util::parse_module(function_name!(), input);
  assert!(module
    .root_scope
    .fn_specs
    .contains(&MFArity::new_local("atom_table", 1)));
}

#[named]
#[test]
fn parse_spec_binary_head() {
  test_util::start(function_name!(), "Parse a raw part of a binary spec");

  // Ensure that _ token is parsed as `Underscore` otherwise the test will fail
  let module = ErlModuleImpl::new_default();
  let input0 = "_:192";
  let tok = test_util::tokenize(input0);
  println!("TOKENS {:?}", &tok);
  let pinput = ParserInput::new_slice(module, &tok);
  let (_t1, hd) = panicking_parser_error_reporter(
    input0,
    pinput.clone(),
    binary_type_head_element(pinput.clone()).finish(),
    true,
  );
  println!("parsed: {:?}", hd);
}

#[named]
#[test]
fn parse_spec_binary_tail() {
  test_util::start(function_name!(), "Parse a raw part of a binary spec");

  // Ensure that _ token is parsed as `Underscore` otherwise the test will fail
  let module = ErlModuleImpl::new_default();
  let input0 = "_:_*192";
  let tok = test_util::tokenize(input0);
  println!("TOKENS {:?}", &tok);
  let pinput = ParserInput::new_slice(module, &tok);
  let (_t1, hd) = panicking_parser_error_reporter(
    input0,
    pinput.clone(),
    binary_type_tail_element(pinput.clone()).finish(),
    true,
  );
  println!("parsed: {:?}", hd);
}

#[named]
#[test]
fn parse_spec_binary() {
  test_util::start(function_name!(), "Parse a spec with an ellipsis");

  let input = "
  -type type0() :: <<_:192>>.
  -spec lambda_table1(bdict()) -> {non_neg_integer(), [<<_:192>>]}.
  -spec lambda_table2(bdict()) -> {non_neg_integer(), [<<_:_*192>>]}.
  -spec lambda_table3(bdict()) -> {non_neg_integer(), [<<_:192, _:_*8>>]}.
  ";
  let module = test_util::parse_module(function_name!(), input);
  assert!(module
    .root_scope
    .fn_specs
    .contains(&MFArity::new_local("lambda_table1", 1)));
  assert!(module
    .root_scope
    .fn_specs
    .contains(&MFArity::new_local("lambda_table2", 1)));
  assert!(module
    .root_scope
    .fn_specs
    .contains(&MFArity::new_local("lambda_table3", 1)));
}

#[named]
#[test]
fn parse_int_range_test() {
  test_util::start(function_name!(), "Parse an integer range");
  let input = "-type reg_num() :: 0 .. 1023.";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  let _type = root_scope
    .user_types
    .get(&MFArity::new_local("reg_num", 0))
    .unwrap();
}

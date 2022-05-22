extern crate compiler;
extern crate function_name;

mod test_util;

use std::ops::Deref;
use std::path::PathBuf;
use ::function_name::named;
use nom::Finish;
use compiler::erl_error::{ErlError, ErlResult};
use compiler::erlang::syntax_tree::erl_ast::ErlAst;
use compiler::project::module::ErlModule;
use compiler::erlang::syntax_tree::nom_parse::parse_attr::ErlAttrParser;
use compiler::mfarity::MFArity;
use compiler::typing::erl_type::ErlType;

#[named]
#[test]
fn union_type_parse() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a multiline union type");
  // No leading `-`, and no trailing `.`, because `ErlAttrParser::parse` strips them
  let input = "type src() :: beam_reg() |
	       {'literal',term()} |
	       {'atom',atom()} |
	       {'integer',integer()} |
	       'nil' |
	       {'float',float()}";
  match ErlAttrParser::parse_type_attr(input).finish() {
    Ok((tail1, result1)) => {
      assert!(tail1.is_empty(), "Not all input consumed, tail: «{}»", tail1);
      println!("Parsed: {}", result1);
    }
    Err(err) => return Err(ErlError::from_nom_error(input, err))
  }
  Ok(())
}

#[named]
#[test]
fn fn_generic_attr_parse1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a generic attribute without args");

  // Dash `-` and terminating `.` are matched outside by the caller.
  let src = " fgsfds\n";

  match ErlAttrParser::parse_generic_attr(src).finish() {
    Ok((tail1, result1)) => {
      assert!(tail1.trim().is_empty(), "Not all input consumed from attr1_src, tail: «{}»", tail1);
      println!("ErlAst for «{}»: {}", src, result1);
    }
    Err(err) => return Err(ErlError::from_nom_error(src, err))
  }
  Ok(())
}

#[named]
#[test]
fn fn_generic_attr_parse2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a generic attribute line, consuming all as string");

  // Dash `-` and terminating `.` are matched outside by the caller.
  let input = " bbbggg (ababagalamaga()) ";

  match ErlAttrParser::parse_generic_attr(input).finish() {
    Ok((tail2, result2)) => {
      assert!(tail2.trim().is_empty(), "Not all input consumed from attr2_src, tail: {}", tail2);
      println!("ErlAst for attr2: {}", result2);
    }
    Err(err) => return Err(ErlError::from_nom_error(input, err))
  }
  Ok(())
}


#[named]
#[test]
fn fn_typespec_parse_1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse typespec syntax for a 1-clause fn");

  let filename = PathBuf::from(function_name!());

  // Leading - and trailing . are consumed by the parser calling parse_fn_spec, so we don't include them here
  let spec1_src = format!("spec {}(A :: integer()) -> any()", function_name!());
  let spec1_m = ErlModule::from_fun_spec_source(&filename, &spec1_src)?;

  if let ErlAst::FnSpec { funarity, spec, .. } = spec1_m.ast.deref() {
    assert_eq!(funarity, &MFArity::new_local(function_name!(), 1),
               "Expected fnspec for '{}'/1, got spec for {}", function_name!(), funarity);
    let fntype = spec.as_fn_type();
    assert_eq!(fntype.clauses().len(), 1,
               "Expected 1 clause in typespec, got {}", spec1_m.ast);
  } else {
    panic!("Expected AST FnSpec node, but got {}", spec1_m.ast)
  }
  Ok(())
}

#[named]
#[test]
fn fn_typespec_parse_2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse typespec syntax for a 2-clause fn");

  let filename = PathBuf::from(function_name!());
  let spec2_src = format!("-spec {}(A :: integer()) -> any(); (B :: atom()) -> tuple().", function_name!());
  let spec2_m = ErlModule::from_fun_spec_source(&filename, &spec2_src)?;

  if let ErlAst::FnSpec { funarity, spec, .. } = spec2_m.ast.deref() {
    assert_eq!(funarity, &MFArity::new_local(function_name!(), 1),
               "Expected fnspec for 'myfun'/2, got spec for {}", funarity);
    let fntype = spec.as_fn_type();
    assert_eq!(fntype.clauses().len(), 2,
               "Expected 2 clauses in typespec, got {}", spec2_m.ast);
  } else {
    panic!("Expected AST FnSpec node, but got {}", spec2_m.ast)
  }

  Ok(())
}

#[named]
#[test]
fn fn_typespec_parse_when() -> IcResult<()> {
  test_util::start(function_name!(), "Parse when-part of a function type spec");

  let filename = PathBuf::from(function_name!());
  // Leading - and trailing . are consumed by the parser calling parse_fn_spec, so we don't include them here
  let spec_src = format!("spec {}(atom()) -> A when A :: tuple()", function_name!());
  let parsed = ErlModule::from_fun_spec_source(&filename, &spec_src)?;

  assert!(parsed.ast.is_fn_spec());
  // TODO: Check that the spec parsed return type is tuple()
  let fn_spec = parsed.ast.as_fn_spec();
  let c0 = fn_spec.as_fn_type().clause(0);
  let t0 = c0.ret_type.ty.clone();
  assert!(t0.eq(&ErlType::any_tuple()),
          "Return type of the function spec must be tuple, but got {}", t0);

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
  // Leading - and trailing . are consumed by the parser calling parse_fn_spec, so we don't include them here
  let spec_src = format!(" spec {}(atom() | 42 | integer()) -> {{ok, any()}} | {{error, any()}}",
                         function_name!());
  let parsed = ErlModule::from_fun_spec_source(&filename, &spec_src)?;

  assert!(parsed.ast.is_fn_spec());
  // TODO: Check that the spec parsed contains the unions as written

  Ok(())
}

#[named]
#[test]
#[should_panic(expected = "All function clauses must have same arity in a typespec")]
fn fn_typespec_parse_1_2() {
  test_util::start(function_name!(), "Parse typespec syntax for 1 and 2 clause fns, must fail because mismatching arity");

  let filename = PathBuf::from(function_name!());
  let spec2_src = format!("-spec {}(A) -> any(); (B, C) -> any().", function_name!());
  let result = ErlModule::from_fun_spec_source(&filename, &spec2_src);
  assert!(result.is_err(), "Parse error is expected");
}
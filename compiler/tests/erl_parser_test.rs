extern crate compiler;
extern crate function_name;
extern crate core;

use std::ops::Deref;
use std::path::PathBuf;

use ::function_name::named;
use nom::Finish;

use compiler::erl_error::{ErlError, ErlResult};
use compiler::erlang::syntax_tree::erl_ast::ErlAst;
use compiler::erlang::syntax_tree::nom_parse::ErlParser;
use compiler::erlang::syntax_tree::nom_parse::parse_attr::ErlAttrParser;
use compiler::literal::Literal;
use compiler::project::module::Module;

mod test_util;

/// Try parse empty module
#[named]
#[test]
fn parse_empty_module() -> ErlResult<()> {
  test_util::start(function_name!(), "parse an empty module with start attribute only");
  let filename = PathBuf::from(function_name!());
  let code = format!("-module({}).\n", function_name!());
  let parsed = Module::from_module_source(&filename, &code)?;
  // let parsed = Module::parse_helper(&filename, &code, ErlParser::parse_module_attr)?;
  println!("Parsed empty module: «{}»\nAST: {}", code, &parsed.ast);
  Ok(())
}

/// Try parse `-export([])` attr
#[named]
#[test]
fn parse_export_attr() -> ErlResult<()> {
  test_util::start(function_name!(), "parse an export attr");

  let (_tail, pfna) = ErlAttrParser::parse_funarity("name/123").unwrap();
  assert_eq!(pfna.name, "name");
  assert_eq!(pfna.arity, 123usize);

  let filename = PathBuf::from(function_name!());
  let code = format!("-module({}).
-export([module/2, format_error/1]).
", function_name!());
  let parsed = Module::from_module_source(&filename, &code)?;
  println!("Parsed module with export attr: «{}»\nAST: {}", code, &parsed.ast);
  Ok(())
}

/// Try parse empty module forms collection (from empty input)
#[named]
#[test]
fn parse_empty_module_forms_collection() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a whitespace only string as module forms collection");
  let code = "    \n   \r\n  ";
  let parse_result = ErlParser::parse_module_forms_collection(code);
  match parse_result.finish() {
    Ok((_tail, forms)) => {
      println!("Parsed empty module forms collection: «{}»\nResult: {:?}", code, forms)
    }
    Err(err) => return Err(ErlError::from_nom_error(code, err)),
  }
  Ok(())
}

/// Try parse module forms collection with 2 functions in it
#[named]
#[test]
fn parse_2_module_forms_collection() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a string with 2 function defs in it as module forms collection");
  let code = "fn1(A, B) -> A + B.\n  fn2(A) ->\n fn1(A, 4).";
  let parse_result = ErlParser::parse_module_forms_collection(code);
  match parse_result.finish() {
    Ok((_tail, forms)) => {
      println!("{} parsed: tail=«{}»\nResult={:?}", function_name!(), code, forms)
    }
    Err(err) => return Err(ErlError::from_nom_error(code, err)),
  }
  Ok(())
}

/// Try parse string
#[named]
#[test]
fn parse_string_test() -> ErlResult<()> {
  test_util::start(function_name!(), "parse a string literal");
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "\"abc\"").unwrap();

  if let ErlAst::Lit { value: lit, .. } = module.ast.deref() {
    if let Literal::String(value) = lit.deref() {
      assert_eq!(value, "abc");
      return Ok(());
    }
  }
  panic!("{} Expected: Literal(String) result, got {}", function_name!(), module.ast)
}

/// Try parse a 2+2 expression
#[named]
#[test]
fn parse_expr_2_plus_2() -> ErlResult<()> {
  let filename = PathBuf::from(function_name!());
  let expr_2 = Module::from_expr_source(&filename, " 2")?;
  println!("Parse \"2\": {}", expr_2.ast);
  assert!(matches!(expr_2.ast.deref(), ErlAst::Lit {..}));

  let expr_2_2 = Module::from_expr_source(&filename, " 2         + 2       ")?;
  println!("Parse \"2+2\": {}", expr_2_2.ast);
  assert!(matches!(expr_2_2.ast.deref(), ErlAst::BinaryOp { .. }));

  Ok(())
}

/// Try parse a flat + expression
#[named]
#[test]
fn parse_expr_flat() -> ErlResult<()> {
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "A + 123 + 333 + 6 + atom + Test")?;
  println!("Parse \"A+123+333+6+atom+Test\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));
  Ok(())
}

/// Try parse a list builder expression
#[named]
#[test]
fn parse_expr_list_builder() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a list builder");
  let filename = PathBuf::from(function_name!());
  let input = "[1, 2, {3, 4} | 5]";
  let module = Module::from_expr_source(&filename, input)?;
  println!("Parsed from «{}»: {}", input, module.ast);
  Ok(())
}

/// Try parse a more complex expression
#[named]
#[test]
fn parse_expr_longer() -> ErlResult<()> {
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "123 + 1 / (2 * hello)")?;
  println!("Parse \"123+1/(2*hello)\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));
  Ok(())
}

/// Try parse an expression with parentheses and division
#[named]
#[test]
fn parse_expr_2() -> ErlResult<()> {
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "(A +1)/ 2")?;
  println!("Parse \"(A+1)/2\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));
  Ok(())
}

/// Try parse a comma expression with some simpler nested exprs
#[named]
#[test]
#[ignore]
fn parse_expr_comma() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a comma separated list of expressions");
  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "A, B, 123 * C")?;
  println!("Parse \"A,B,123*C\": {}", module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));

  Ok(())
}

/// Try parse a list and a tuple
#[named]
#[test]
fn parse_expr_containers() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a list and a tuple");
  let filename = PathBuf::from(function_name!());
  let src = "[1,2  ,3  ] + {a, b ,C}";
  let module = Module::from_expr_source(&filename, src)?;
  println!("Parse «{}»: {}", src, module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));

  Ok(())
}

/// Try parse a hard-equals expression
#[named]
#[test]
fn parse_expr_hard_eq() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a hard-equals expr");
  let filename = PathBuf::from(function_name!());
  let src = "A =:= B2";
  let module = Module::from_expr_source(&filename, src)?;
  println!("Parse «{}»: {}", src, module.ast);
  assert!(matches!(module.ast.deref(), ErlAst::BinaryOp { .. }));

  Ok(())
}

/// Try parse some function defs
#[named]
#[test]
fn parse_fn1() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a function returning some simple value");
  let filename = PathBuf::from(function_name!());
  let module = Module::from_fun_source(&filename, "f(A) -> atom123.")?;
  println!("Parse \"f(A) -> atom123.\": {}", module.ast);

  if let ErlAst::FnDef { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::FunctionDef, got {}", function_name!(), module.ast);
  }

  let func_count = if let Ok(scope_r) = module.scope.read() {
    scope_r.function_defs.len()
  } else {
    panic!()
  };
  assert_eq!(func_count, 1, "Module must have 1 function in its env");
  // assert_eq!(module1.function_clauses.len(), 1, "Module must have 1 function clause in its env");
  Ok(())
}

/// Try parse some function defs
#[named]
#[test]
fn parse_fn_with_list_comprehension() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a function from OTP lib/compiler with list comprehension");
  let filename = PathBuf::from(function_name!());
  let source = "module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.";
  let module = Module::from_fun_source(&filename, source)?;

  println!("Parsed result: {}", module.ast);
  Ok(())
}

#[named]
#[test]
fn parse_try_catch_exceptionpattern() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse an exception pattern for try/catch");

  {
    let (exc_tail, exc) = ErlParser::parse_exception_pattern("Class:Error").unwrap();
    println!("Parsed ExceptionPattern: {:?}", &exc);

    assert!(exc_tail.is_empty(), "Could not parse exception pattern");
    assert!(exc.class.is_var());
    assert!(exc.error.is_var());
    assert!(exc.stack.is_none());
  }
  {
    let (exc_tail, exc) = ErlParser::parse_exception_pattern("Class:Error:Stack").unwrap();
    println!("Parsed ExceptionPattern: {:?}", &exc);

    assert!(exc_tail.is_empty(), "Could not parse exception pattern");
    assert!(exc.class.is_var());
    assert!(exc.error.is_var());
    assert!(exc.stack.is_some());
  }
  Ok(())
}

#[named]
#[test]
fn parse_try_catch_clause() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a try-catch catch-clause");

  let (tail, clause) = ErlParser::parse_catch_clause("A:B:C when true -> ok").unwrap();
  assert!(tail.is_empty(), "Could not parse exception pattern");
  assert!(clause.exc_pattern.class.is_var());
  assert!(clause.when_guard.is_some());
  assert!(clause.body.is_atom());
  println!("Parsed Catch clause: {:?}", &clause);
  Ok(())
}

#[named]
#[test]
fn parse_fn_try_catch() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a function with try/catch");

  let filename = PathBuf::from(function_name!());
  // let source = "function(X) -> try X/0 end.";
  let source = "function({function,Name,Arity,CLabel,Is0}) ->
    try atom1, {function,Name,Arity,CLabel,Is}
    catch Class:Error:Stack -> erlang:raise(Class, Error, Stack), ok
    end.";
  let module = Module::from_fun_source(&filename, source)?;

  println!("Parsed result: {}", module.ast);
  Ok(())
}

/// Try parse a function apply expr. This can be any expression immediately followed by
/// a parenthesized comma expression.
#[named]
#[test]
fn parse_apply_1() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a simple apply() expr");

  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "a_function()")?;
  println!("{}: parsed {}", function_name!(), module.ast);

  if let ErlAst::Apply { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::App, got {}", function_name!(), module.ast);
  }

  Ok(())
}

#[named]
#[test]
fn parse_big_fun() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a multi-clause big function");

  let filename = PathBuf::from(function_name!());
  let src = "rename_instr({bs_put_binary=I,F,Sz,U,Fl,Src}) ->
    {bs_put,F,{I,U,Fl},[Sz,Src]};
rename_instr({bs_put_float=I,F,Sz,U,Fl,Src}) ->
    {bs_put,F,{I,U,Fl},[Sz,Src]};
rename_instr({bs_put_integer=I,F,Sz,U,Fl,Src}) ->
    {bs_put,F,{I,U,Fl},[Sz,Src]};
rename_instr({bs_put_utf8=I,F,Fl,Src}) ->
    {bs_put,F,{I,Fl},[Src]};
rename_instr({bs_put_utf16=I,F,Fl,Src}) ->
    {bs_put,F,{I,Fl},[Src]};
rename_instr({bs_put_utf32=I,F,Fl,Src}) ->
    {bs_put,F,{I,Fl},[Src]};
rename_instr({bs_put_string,_,{string,String}}) ->
    %% Only happens when compiling from .S files. In old
    %% .S files, String is a list. In .S in OTP 22 and later,
    %% String is a binary.
    {bs_put,{f,0},{bs_put_binary,8,{field_flags,[unsigned,big]}},
     [{atom,all},{literal,iolist_to_binary([String])}]};
rename_instr({bs_add=I,F,[Src1,Src2,U],Dst}) when is_integer(U) ->
    {bif,I,F,[Src1,Src2,{integer,U}],Dst};
rename_instr({bs_utf8_size=I,F,Src,Dst}) ->
    {bif,I,F,[Src],Dst};
rename_instr({bs_utf16_size=I,F,Src,Dst}) ->
    {bif,I,F,[Src],Dst};
rename_instr({bs_init2=I,F,Sz,Extra,Live,Flags,Dst}) ->
    {bs_init,F,{I,Extra,Flags},Live,[Sz],Dst};
rename_instr({bs_init_bits=I,F,Sz,Extra,Live,Flags,Dst}) ->
    {bs_init,F,{I,Extra,Flags},Live,[Sz],Dst};
rename_instr({bs_append=I,F,Sz,Extra,Live,U,Src,Flags,Dst}) ->
    {bs_init,F,{I,Extra,U,Flags},Live,[Sz,Src],Dst};
rename_instr({bs_private_append=I,F,Sz,U,Src,Flags,Dst}) ->
    {bs_init,F,{I,U,Flags},none,[Sz,Src],Dst};
rename_instr(bs_init_writable=I) ->
    {bs_init,{f,0},I,1,[{x,0}],{x,0}};
rename_instr({put_map_assoc,Fail,S,D,R,L}) ->
    {put_map,Fail,assoc,S,D,R,L};
rename_instr({put_map_exact,Fail,S,D,R,L}) ->
    {put_map,Fail,exact,S,D,R,L};
rename_instr({test,has_map_fields,Fail,Src,{list,List}}) ->
    {test,has_map_fields,Fail,[Src|List]};
rename_instr({test,is_nil,Fail,[Src]}) ->
    {test,is_eq_exact,Fail,[Src,nil]};
rename_instr({select_val=I,Reg,Fail,{list,List}}) ->
    {select,I,Reg,Fail,List};
rename_instr({select_tuple_arity=I,Reg,Fail,{list,List}}) ->
    {select,I,Reg,Fail,List};
rename_instr(send) ->
    {call_ext,2,send};
rename_instr(I) -> I.";
  let mod1 = Module::from_fun_source(&filename, src)?;
  println!("{}: parsed {}", function_name!(), mod1.ast);

  Ok(())
}

#[named]
#[test]
fn parse_fun_with_if() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a function with if statement");
  let filename = PathBuf::from(function_name!());
  let src = "rename_instrs([{get_list,S,D1,D2}|Is]) ->
    if D1 =:= S -> [{get_tl,S,D2},{get_hd,S,D1}|rename_instrs(Is)];
        true -> [{get_hd,S,D1},{get_tl,S,D2}|rename_instrs(Is)]
    end.";
  let mod1 = Module::from_fun_source(&filename, src)?;
  println!("{}: parsed {}", function_name!(), mod1.ast);

  Ok(())
}

#[named]
#[test]
fn parse_fun_with_case() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a function with case statement");
  let filename = PathBuf::from(function_name!());
  let src = " f(x)  ->   case proplists:get_bool(no_shared_fun_wrappers, Opts) of
        false -> Swap = beam_opcodes:opcode(swap, 2), beam_dict:opcode(Swap, Dict);
        true -> Dict end";
  let mod1 = Module::from_fun_source(&filename, src)?;
  println!("{}: parsed {}", function_name!(), mod1.ast);

  Ok(())
}

#[named]
#[test]
fn parse_fun_with_lambda() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a function with a lambda");
  let filename = PathBuf::from(function_name!());
  let src = "coalesce_consecutive_labels([{label,L}=Lbl,{label,Alias}|Is], Replace, Acc) ->
    coalesce_consecutive_labels([Lbl|Is], [{Alias,L}|Replace], Acc);
coalesce_consecutive_labels([I|Is], Replace, Acc) ->
    coalesce_consecutive_labels(Is, Replace, [I|Acc]);
coalesce_consecutive_labels([], Replace, Acc) ->
    D = maps:from_list(Replace),
    beam_utils:replace_labels(Acc, [], D, fun(L) -> L end).";
  let mod1 = Module::from_fun_source(&filename, src)?;
  println!("{}: parsed {}", function_name!(), mod1.ast);

  Ok(())
}

#[named]
#[test]
fn parse_apply_with_module_and_without() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse an function call with or without module name");
  let filename = PathBuf::from(function_name!());

  {
    let src = "function_name()";
    let mod1 = Module::from_expr_source(&filename, src)?;
    println!("{}: from «{}» parsed {}", function_name!(), src, mod1.ast);
    assert!(mod1.ast.is_application());
  }
  {
    let src = "mod_name:function_name()";
    let mod1 = Module::from_expr_source(&filename, src)?;
    println!("{}: from «{}» parsed {}", function_name!(), src, mod1.ast);
    assert!(mod1.ast.is_application());
  }
  Ok(())
}

#[should_panic]
#[named]
#[test]
fn parse_apply_panic() {
  test_util::start(function_name!(), "Parse an function call without parentheses, should panic");
  let filename = PathBuf::from(function_name!());
  {
    let src = "mod_name:function_name";
    let mod1 = Module::from_expr_source(&filename, src).unwrap();
    // Parsing above should panic

    println!("{}: from «{}» parsed {}", function_name!(), src, mod1.ast);
  }
}

#[named]
#[test]
fn parse_apply_2() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse an apply() expression with a fancy left side");

  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "(123 + atom)()")?;
  println!("{}: parsed {}", function_name!(), module.ast);

  if let ErlAst::Apply { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::App, got {}", function_name!(), module.ast);
  }

  Ok(())
}

#[named]
#[test]
fn parse_apply_3() -> ErlResult<()> {
  test_util::start(function_name!(), "Parse a very fancy nested apply() expression");

  let filename = PathBuf::from(function_name!());
  let module = Module::from_expr_source(&filename, "(F() + g())(test(), 123())")?;
  println!("{} parse_application 3 parsed {}", function_name!(), module.ast);

  if let ErlAst::Apply { .. } = module.ast.deref() {
    // ok
  } else {
    panic!("{} Expected: ErlAst::App, got {}", function_name!(), module.ast);
  }
  Ok(())
}

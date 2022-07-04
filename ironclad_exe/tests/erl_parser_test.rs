extern crate core;
extern crate function_name;
extern crate libironclad_erlang;

use std::ops::Deref;

use ::function_name::named;
use libironclad_erlang::erl_syntax::erl_ast::ast_iter::IterableAstNodeT;
use libironclad_erlang::erl_syntax::erl_ast::node_impl::AstNodeType::{
  Apply, BinaryOp, ListComprehension, Lit,
};
use libironclad_erlang::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use libironclad_erlang::erl_syntax::parsers::parse_expr::parse_expr_list::parse_list_comprehension;
use libironclad_erlang::erl_syntax::parsers::parser_input::ParserInput;
use libironclad_erlang::erl_syntax::preprocessor::parsers::parse_record::parse_record_def;
use libironclad_erlang::error::ic_error::IcResult;
use libironclad_erlang::literal::Literal;
use libironclad_erlang::project::module::module_impl::ErlModuleImpl;
use libironclad_util::mfarity::MFArity;
use nom::Finish;

mod test_util;

/// Try parse empty module
#[named]
#[test]
fn parse_empty_module() -> IcResult<()> {
  test_util::start(function_name!(), "parse an empty module with start attribute only");
  let nodes = test_util::parse_module_unwrap(function_name!(), "");
  assert_eq!(nodes.len(), 0);
  Ok(())
}

/// Try parse `-export([])` attr
#[named]
#[test]
fn parse_export_attr1() -> IcResult<()> {
  test_util::start(function_name!(), "parse an export attr");

  let input = "-export([name/123]).";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  assert_eq!(root_scope.exports.len(), 1);
  assert!(root_scope
    .exports
    .contains(&MFArity::new_local("name", 123)));
  Ok(())
}

#[named]
#[test]
fn parse_export_attr2() -> IcResult<()> {
  test_util::start(function_name!(), "parse an export attr");
  let input = "-export([module/2, format_error/1]).";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  assert_eq!(root_scope.exports.len(), 2);

  assert!(root_scope
    .exports
    .contains(&MFArity::new_local("module", 2)));
  assert!(root_scope
    .exports
    .contains(&MFArity::new_local("format_error", 1)));
  Ok(())
}

/// Try parse `-import(atom, [mfa,...]).` attr
#[named]
#[test]
fn parse_import_attr() -> IcResult<()> {
  test_util::start(function_name!(), "parse an import attr");
  let input = "-import(lists, [map/2,member/2,keymember/3,duplicate/2,splitwith/2]).\n\n";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  assert_eq!(root_scope.imports.len(), 5);
  assert!(root_scope
    .imports
    .contains(&MFArity::new("lists", "map", 2)));
  assert!(root_scope
    .imports
    .contains(&MFArity::new("lists", "member", 2)));
  assert!(root_scope
    .imports
    .contains(&MFArity::new("lists", "keymember", 3)));
  assert!(root_scope
    .imports
    .contains(&MFArity::new("lists", "duplicate", 2)));
  assert!(root_scope
    .imports
    .contains(&MFArity::new("lists", "splitwith", 2)));
  Ok(())
}

/// Try parse module forms collection with 2 functions in it
#[named]
#[test]
fn parse_2_module_forms_collection() -> IcResult<()> {
  test_util::start(
    function_name!(),
    "Parse a string with 2 function defs in it as module forms collection",
  );
  let input = "fn1(A, B) -> A + B.\n  fn2(A) ->\n fn1(A, 4).";
  let _module = test_util::parse_module(function_name!(), input);
  Ok(())
}

/// Try parse string
#[named]
#[test]
fn parse_string_test() -> IcResult<()> {
  test_util::start(function_name!(), "parse a string literal");
  let expr = test_util::parse_expr(function_name!(), "\"abc\"");
  if let Lit { value: lit, .. } = &expr.content {
    if let Literal::String(value) = lit.deref() {
      assert_eq!(value.as_str(), "abc");
      return Ok(());
    }
  }
  panic!("{} Expected: Literal(String) result, got {}", function_name!(), expr)
}

/// Try parse quoted atom
#[named]
#[test]
fn parse_atom_test() {
  test_util::start(function_name!(), "parse some atoms");
  let input = "{'a-a', b_b, c, '$d'}";
  let expr = test_util::parse_expr(function_name!(), input);

  let elements = expr.as_tuple();
  assert_eq!(elements.len(), 4, "Expected 4-tuple, received {}", &expr);
  assert_eq!(elements[0].as_atom(), "a-a");
  assert_eq!(elements[1].as_atom(), "b_b");
  assert_eq!(elements[2].as_atom(), "c");
  assert_eq!(elements[3].as_atom(), "$d");
}

/// Try parse a 2+2 expression
#[named]
#[test]
fn parse_expr_2_plus_2() -> IcResult<()> {
  let expr_2 = test_util::parse_expr(function_name!(), " 2");
  println!("Parse \"2\": {}", expr_2);
  assert!(matches!(&expr_2.content, Lit { .. }));

  let expr_2_2 = test_util::parse_expr(function_name!(), " 2         + 2       ");
  println!("Parse \"2+2\": {}", expr_2_2);
  assert!(matches!(&expr_2_2.content, BinaryOp { .. }));

  Ok(())
}

/// Try parse a flat + expression
#[named]
#[test]
fn parse_expr_flat() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a long expr with +");
  let expr = test_util::parse_expr(function_name!(), "A + 123 + 333 + 6 + atom + Test");
  println!("Parse \"A+123+333+6+atom+Test\": {}", expr);
  assert!(matches!(&expr.content, BinaryOp { .. }));
  Ok(())
}

/// Try parse a list builder expression
#[named]
#[test]
fn parse_expr_list_builder() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a list builder");
  let input = "[1, 2, {3, 4} | 5]";
  let expr = test_util::parse_expr(function_name!(), input);
  println!("Parsed from «{}»: {}", input, expr);
  Ok(())
}

/// Try parse a more complex expression
#[named]
#[test]
fn parse_expr_longer() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a math expr");
  let expr = test_util::parse_expr(function_name!(), "123 + 1 / (2 * hello)");
  println!("Parse \"123+1/(2*hello)\": {}", expr);
  assert!(matches!(&expr.content, BinaryOp { .. }));
  Ok(())
}

/// Try parse an expression with parentheses and division
#[named]
#[test]
fn parse_expr_based_numbers() -> IcResult<()> {
  test_util::start(function_name!(), "Parse 2-10-16-36 based decimals");
  let input = "{2#10, 10#10, 16#fF, 36#Zz}";
  let expr = test_util::parse_expr(function_name!(), input);
  let tuple = expr.as_tuple();
  assert_eq!(tuple[0].as_small(), 2);
  assert_eq!(tuple[1].as_small(), 10);
  assert_eq!(tuple[2].as_small(), 16 * 16 - 1);
  assert_eq!(tuple[3].as_small(), 36 * 36 - 1);
  Ok(())
}

/// Try parse record field accessors
#[named]
#[test]
fn parse_expr_record_field_access() {
  test_util::start(function_name!(), "Parse record field accessors");
  let input1 = "Var#record.field";
  let input2 = "(Var)#record.field";
  let input3 = "(functioncall())#record.field";

  let expr1 = test_util::parse_expr(function_name!(), input1);
  assert!(expr1.is_record_field(), "Failed to parse Var#record.field");

  let expr2 = test_util::parse_expr(function_name!(), input2);
  assert!(expr2.is_record_field(), "Failed to parse (Var)#record.field");

  let expr3 = test_util::parse_expr(function_name!(), input3);
  assert!(expr3.is_record_field(), "Failed to parse (functioncall())#record.field");
}

/// Try parse record builder
#[named]
#[test]
fn parse_expr_record_builder_case() {
  test_util::start(function_name!(), "Parse record builder in a case");
  let input1 = "
    case T#t_tuple.exact of
      false ->
          T#t_tuple{size=max(Index, Size),elements=Es};
      true when Index =< Size ->
          T#t_tuple{elements=Es};
      true ->
          none
    end";

  let expr1 = test_util::parse_expr(function_name!(), input1);
  assert!(expr1.is_case_expr(), "Failed to parse a record builder");
}

/// Try parse an expression with parentheses and division
#[named]
#[test]
fn parse_expr_2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a math expr with some spaces");
  let expr = test_util::parse_expr(function_name!(), "(A +1)/ 2");
  assert!(matches!(&expr.content, BinaryOp { .. }));
  Ok(())
}

/// Try parse an expression with list comprehension
#[named]
#[test]
fn parse_expr_lc1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse an expr with list comprehension");
  let input = "[F || F <- Fs0 ] \n";
  let tokens = test_util::tokenize(input);
  let module = ErlModuleImpl::new_default();
  let p_input = ParserInput::new_slice(module, &tokens);
  let (_tail, expr) = panicking_parser_error_reporter(
    input,
    p_input.clone(),
    parse_list_comprehension(p_input.clone()).finish(),
    true,
  );
  assert!(matches!(&expr.content, ListComprehension { .. }));
  Ok(())
}

/// Try parse an expression with list comprehension
#[named]
#[test]
fn parse_expr_lc2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse an expr with list comprehension");
  let expr = test_util::parse_expr(function_name!(), " [function(F) || F <- Fs0]");
  assert!(matches!(&expr.content, ListComprehension { .. }));
  Ok(())
}

/// Try parse a comma expression with some simpler nested exprs
#[named]
#[test]
#[ignore]
fn parse_expr_comma() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a comma separated list of expressions");
  let expr = test_util::parse_expr(function_name!(), "A, B, 123 * C");
  println!("Parse \"A,B,123*C\": {}", expr);
  assert!(matches!(&expr.content, BinaryOp { .. }));

  Ok(())
}

/// Try parse a list and a tuple
#[named]
#[test]
fn parse_expr_containers() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a list and a tuple");
  let src = "[1,2  ,3  ] + {a, b ,C}";
  let expr = test_util::parse_expr(function_name!(), src);
  println!("Parse «{}»: {}", src, expr);
  assert!(matches!(&expr.content, BinaryOp { .. }));

  Ok(())
}

/// Try parse a hard-equals expression
#[named]
#[test]
fn parse_expr_hard_eq() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a hard-equals expr");
  let src = "A =:= B2";
  let expr = test_util::parse_expr(function_name!(), src);
  println!("Parse «{}»: {}", src, expr);
  assert!(matches!(&expr.content, BinaryOp { .. }));
  Ok(())
}

/// Try parse some function defs
#[named]
#[test]
fn parse_fn1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function returning some simple value");
  let module = test_util::parse_module(function_name!(), "f(A) -> atom123.");
  let ast = module.ast.borrow().clone();
  let nodes = ast.children().unwrap_or_default();
  println!("Parse \"f(A) -> atom123.\": {}", nodes[0]);

  let fndef = nodes[0].as_fn_def();
  assert_eq!(fndef.clauses.len(), 1);
  assert_eq!(fndef.clauses[0].name, Some("f".to_string()));

  let root_scope = module.root_scope.clone();
  let fnf = root_scope.fn_defs.get(&MFArity::new_local("f", 1));
  assert!(fnf.is_some(), "Function f/1 not found");

  Ok(())
}

/// Try parse some function defs
#[named]
#[test]
fn parse_fn_with_list_comprehension() -> IcResult<()> {
  test_util::start(function_name!(), "From OTP's lib/compiler: parse list comprehension");
  let source = "module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.";
  let module = test_util::parse_module(function_name!(), source);
  let root_scope = module.root_scope.clone();
  assert!(root_scope
    .fn_defs
    .contains(&MFArity::new_local("module", 2)));
  Ok(())
}

#[named]
#[test]
fn parse_try_catch_exceptionpattern() -> IcResult<()> {
  test_util::start(function_name!(), "Parse an exception pattern for try/catch");

  {
    let input = "myfun() -> try ok catch Class:Error -> ok end.";
    let module = test_util::parse_module(function_name!(), input);
    println!("Parsed ExceptionPattern: {:?}", module.ast.borrow());

    // // TODO: Use panicking error reporter
    // assert!(exc_tail.is_empty(), "Could not parse exception pattern");
    // assert!(exc.class.is_var());
    // assert!(exc.error.is_var());
    // assert!(exc.stack.is_none());
  }

  {
    let input = "myfun() -> try ok catch Class:Error:Stack -> ok end.";
    let module = test_util::parse_module(function_name!(), input);
    println!("Parsed ExceptionPattern: {:?}", module.ast.borrow());

    // // TODO: Use panicking error reporter
    // assert!(exc_tail.is_empty(), "Could not parse exception pattern");
    // assert!(exc.class.is_var());
    // assert!(exc.error.is_var());
    // assert!(exc.stack.is_some());
  }
  Ok(())
}

#[named]
#[test]
fn parse_try_catch_clause() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a try-catch catch-clause");

  let input = "myfun() -> try ok catch Class:Error:Stack when true -> ok end.";
  let module = test_util::parse_module(function_name!(), input);
  println!("Parsed Catch clause: {:?}", module.ast.borrow());
  // // TODO: Use panicking error reporter
  // assert!(tail.is_empty(), "Could not parse exception pattern");
  // assert!(clause.exc_pattern.class.is_var());
  // assert!(clause.when_guard.is_some());
  // assert!(clause.body.is_atom());
  Ok(())
}

#[named]
#[test]
fn parse_fn_try_catch() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function with try/catch");

  let source = "function({function,Name,Arity,CLabel,Is0}) ->
    try atom1, {function,Name,Arity,CLabel,Is}
    catch Class:Error:Stack -> erlang:raise(Class, Error, Stack), ok
    end.";
  let module = test_util::parse_module(function_name!(), source);
  println!("Parsed result: {}", module.ast.borrow());
  Ok(())
}

/// Try parse a function apply expr. This can be any expression immediately followed by
/// a parenthesized comma expression.
#[named]
#[test]
fn parse_apply_1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a simple apply() expr");
  let expr = test_util::parse_expr(function_name!(), "a_function()");
  println!("{}: parsed {}", function_name!(), expr);

  if let Apply { .. } = &expr.content {
    // ok
  } else {
    panic!("{} Expected: ErlAst::App, got {}", function_name!(), expr);
  }

  Ok(())
}

#[named]
#[test]
fn parse_big_fun() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a multi-clause big function");
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
  let module = test_util::parse_module(function_name!(), src);
  println!("{}: parsed {}", function_name!(), module.ast.borrow());
  Ok(())
}

#[named]
#[test]
fn parse_fun_with_if() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function with if statement");
  let src = "rename_instrs([{get_list,S,D1,D2}|Is]) ->
    if D1 =:= S -> [{get_tl,S,D2},{get_hd,S,D1}|rename_instrs(Is)];
        true -> [{get_hd,S,D1},{get_tl,S,D2}|rename_instrs(Is)]
    end.";
  let _module = test_util::parse_module(function_name!(), src);
  Ok(())
}

#[named]
#[test]
fn parse_fun_with_case() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function with case statement");
  let src = " f(x)  ->   case proplists:get_bool(no_shared_fun_wrappers, Opts) of
        false -> Swap = beam_opcodes:opcode(swap, 2), beam_dict:opcode(Swap, Dict);
        true -> Dict end.";
  let _module = test_util::parse_module(function_name!(), src);
  Ok(())
}

#[named]
#[test]
fn parse_fun_with_lambda() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function with a lambda");
  let src = "coalesce_consecutive_labels([{label,L}=Lbl,{label,Alias}|Is], Replace, Acc) ->
    coalesce_consecutive_labels([Lbl|Is], [{Alias,L}|Replace], Acc);
coalesce_consecutive_labels([I|Is], Replace, Acc) ->
    coalesce_consecutive_labels(Is, Replace, [I|Acc]);
coalesce_consecutive_labels([], Replace, Acc) ->
    D = maps:from_list(Replace),
    beam_utils:replace_labels(Acc, [], D, fun(L) -> L end).";
  let _module = test_util::parse_module(function_name!(), src);
  Ok(())
}

#[named]
#[test]
fn parse_fun_with_binary_match() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function with a binary match in args");
  let input = "finalize_fun_table_1(<<\"FunT\",Keep:8/binary,Table0/binary>>, MD5) ->
    <<Uniq:27,_:101/bits>> = MD5,
    Table = finalize_fun_table_2(Table0, Uniq, <<>>),
    <<\"FunT\",Keep/binary,Table/binary>>;
finalize_fun_table_1(Chunk, _) -> Chunk.";
  let _module = test_util::parse_module(function_name!(), input);
  Ok(())
}

#[named]
#[test]
fn parse_fun_guard() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function with a guard");
  let input = "
  %% Build an IFF form.
  build_form(Id, Chunks0) 
  % when byte_size(Id) =:= 4, is_list(Chunks0) 
  ->
      %Chunks = list_to_binary(Chunks0),
      %Size = byte_size(Chunks),
      0 = Size rem 4\t\t\t\t% Assertion: correct padding?
      %, <<\"FOR1\",(Size+4):32,Id/binary,Chunks/binary>>
      .";
  let _module = test_util::parse_module(function_name!(), input);
  Ok(())
}

#[named]
#[test]
fn parse_rem_expr() -> IcResult<()> {
  test_util::start(
    function_name!(),
    "Parse an expression with a remainder; \
          Ensure final period allows stray newlines before it",
  );
  let input = "f() -> 0 = Size rem 4\n.";
  let _module = test_util::parse_module(function_name!(), input);
  Ok(())
}

#[named]
#[test]
fn parse_fun_guard2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a function with a guard and record fields");
  let input = "-spec opcode(non_neg_integer(), bdict()) -> bdict().
      opcode(Op, Dict) when Dict#asm.highest_opcode >= Op -> Dict;
      opcode(Op, Dict) -> Dict#asm{highest_opcode=Op}.";
  let _module = test_util::parse_module(function_name!(), input);
  Ok(())
}

#[named]
#[test]
fn parse_apply_with_module_and_without1() -> IcResult<()> {
  test_util::start(function_name!(), "Parse an function call with or without module name");
  let src = "function_name()";
  let expr = test_util::parse_expr(function_name!(), src);
  println!("{}: from «{}» parsed {}", function_name!(), src, expr);
  assert!(expr.is_application());
  Ok(())
}

#[named]
#[test]
fn parse_apply_with_module_and_without2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse an function call with or without module name");
  let src = "mod_name:function_name()";
  let expr = test_util::parse_expr(function_name!(), src);
  println!("{}: from «{}» parsed {}", function_name!(), src, expr);
  assert!(expr.is_application());
  Ok(())
}

#[named]
#[test]
fn parse_apply_with_module_and_without3() -> IcResult<()> {
  test_util::start(function_name!(), "Parse an function call with or without module name");
  let src = "proplists:get_bool(no_shared_fun_wrappers, Opts)";
  let expr = test_util::parse_expr(function_name!(), src);
  println!("{}: from «{}» parsed {}", function_name!(), src, expr);
  assert!(expr.is_application());
  Ok(())
}

#[should_panic]
#[named]
#[test]
fn parse_apply_panic() {
  test_util::start(function_name!(), "Parse an function call without parentheses, should panic");

  let input = "mod_name:function_name";
  let expr = test_util::parse_expr(function_name!(), input);
  // Parsing above should panic

  println!("{}: from «{}» parsed {}", function_name!(), input, expr);
}

#[named]
#[test]
fn parse_apply_2() -> IcResult<()> {
  test_util::start(function_name!(), "Parse an apply() expression with a fancy left side");

  let expr = test_util::parse_expr(function_name!(), "(123 + atom)()");
  println!("{}: parsed {}", function_name!(), expr);
  assert!(expr.is_application());

  Ok(())
}

#[named]
#[test]
fn parse_apply_3() -> IcResult<()> {
  test_util::start(function_name!(), "Parse a fancy nested apply() expression");

  let expr = test_util::parse_expr(function_name!(), "(F() + g())(test(), 123())");
  println!("{} parse_application 3 parsed {}", function_name!(), expr);
  assert!(expr.is_application());
  Ok(())
}

/// Try parse a small `-record(name, {fields})` attr from OTP's `lib/erl_compile.hrl`
#[named]
#[test]
fn parse_small_record_test() -> IcResult<()> {
  test_util::start(function_name!(), "parse a record definition");
  let input = "-record(test_small,\t\n{a\t=value,\nb =\"test\"\n}).";
  let _parsed = test_util::parse_module_unwrap(function_name!(), input);
  Ok(())
}

/// Try parse `-record(name, {fields})` attr from OTP's `lib/erl_compile.hrl`
#[named]
#[test]
fn parse_record_raw_parser_invocation() -> IcResult<()> {
  test_util::start(function_name!(), "parse a record definition (raw parser invocation)");
  let input = "-record(options,
	 {includes=[] :: [file:filename()],	% Include paths (list of
	 }).";
  let module = ErlModuleImpl::new_default();
  let tokens = test_util::tokenize(input);
  let p_input = ParserInput::new_slice(module, &tokens);
  let (_tail, _pnode) = panicking_parser_error_reporter(
    input,
    p_input.clone(),
    parse_record_def(p_input.clone()).finish(),
    true,
  );
  Ok(())
}

/// Try parse `-record(name, {fields})` attr from OTP's `lib/erl_compile.hrl`
#[named]
#[test]
fn parse_record_test() -> IcResult<()> {
  test_util::start(function_name!(), "parse a record definition");
  let input = "%%\n
    %% Generic compiler options, passed from the erl_compile module.\n
    -record(options,
	 {includes=[] :: [file:filename()],	% Include paths (list of
						% absolute directory names).
	  outdir=\".\"  :: file:filename(),	% Directory for result (absolute path).
	  output_type=undefined :: atom(),	% Type of output file.
	  defines=[]  :: [atom() | {atom(),_}],	% Preprocessor defines.  Each element is an atom
						% (the name to define), or a {Name, Value} tuple.
	  warning=1   :: non_neg_integer(),	% Warning level (0 - no warnings, 1 - standard level,
						% 2, 3, ... - more warnings).
	  verbose=false :: boolean(),		% Verbose (true/false).
	  optimize=999,				% Optimize options.
	  specific=[] :: [_],			% Compiler specific options.
	  outfile=\"\"  :: file:filename(),	% Name of output file (internal
						% use in erl_compile.erl).
	  cwd	      :: file:filename()	% Current working directory for erlc.
	 }).\n";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  let record_def = root_scope.record_defs.get(&"options".to_string()).unwrap();
  assert_eq!(record_def.tag, "options");
  assert_eq!(record_def.fields.len(), 10);
  Ok(())
}

/// Try parse `-record(name, {fields})` attr from OTP's `lib/erl_compile.hrl` as a part of a module
#[named]
#[test]
fn parse_record_with_module() -> IcResult<()> {
  test_util::start(function_name!(), "parse a record definition as a part of a module");
  let input = "-record(options, {includes }).\n";
  let module = test_util::parse_module(function_name!(), input);
  let root_scope = module.root_scope.clone();
  let _rec_def = root_scope.record_defs.get(&"options".to_string()).unwrap();

  // assert_eq!(nodes.len(), 1); // -module() {} is the root node, and -record() node is inside its '.forms'
  // assert!(matches!(nodes[0].content, AstNodeType::RecordDefinition { .. }));
  Ok(())
}

#[named]
#[test]
fn parse_record_construction() -> IcResult<()> {
  test_util::start(function_name!(), "parse a record construction");
  let input = "\
  fn() ->\n\
    #tr{t=#t_fun{target={_Name, TotalArity}}} = Func.";
  let module = test_util::parse_module(function_name!(), input);
  let _root_scope = module.root_scope.clone();
  Ok(())
}

/// Try parse `-record(name, {fields})` with a map in it
#[named]
#[test]
fn parse_record_with_map() -> IcResult<()> {
  test_util::start(function_name!(), "parse a record definition");
  let input = "-record(t_tuple, {size=0 :: integer(),
    exact=false :: boolean(),
    elements=#{} :: tuple_elements()}).";
  let _nodes = test_util::parse_module_unwrap(function_name!(), input);
  // println!("Parsed: «{}»\nAST: {}", input, &nodes.ast);
  Ok(())
}

/// Try parse a complex case with guards
#[named]
#[test]
fn parse_case_with_guards() {
  test_util::start(function_name!(), "parse a complex case with guards");
  let input = "
-define(tag_u, 1).
encode1(Tag, Bytes) ->
  case iolist_size(Bytes) of
    Num when 2 =< Num, Num =< 8 ->
      [((Num - 2) bsl 5) bor 2#00011000 bor Tag | Bytes];
    Num when 8 < Num ->
      [2#11111000 bor Tag, encode(?tag_u, Num - 9) | Bytes]
  end.
";
  let _m = test_util::parse_module(function_name!(), input);
}

/// Try parse a complex case with unary `not` operator
#[named]
#[test]
fn parse_case_with_unary_op() {
  test_util::start(function_name!(), "parse a complex case with unary op");
  let input = "simplify_get_map_elements(Fail, Src, {list,[Key,Dst]},
                          [{get_map_elements,Fail,Src,{list,List1}}|Acc]) ->
    case are_keys_literals([Key]) andalso are_keys_literals(List1) andalso not is_reg_overwritten(Src, List1) andalso not is_reg_overwritten(Dst, List1) of
        true ->
            case member(Key, List1) of
                true -> error;
                false -> List = [Key,Dst|List1], {ok,[{get_map_elements,Fail,Src,{list,List}}|Acc]}
            end;
        false -> error
    end;
simplify_get_map_elements(_, _, _, _) -> error.";
  let _m = test_util::parse_module(function_name!(), input);
}

#[named]
#[test]
fn parse_func_with_map_arg() {
  test_util::start(function_name!(), "parse a function with empty map argument");
  let input = "
    lut_element_types(Index, Max, #{}) when Index > Max -> [].
    lut_element_types(Index, Max, Es) -> ElementType = beam_types:get_tuple_element(Index, Es),
      [ElementType | lut_element_types(Index + 1, Max, Es)].";
  let _m = test_util::parse_module(function_name!(), input);
}

#[named]
#[test]
fn parse_func_with_funref() {
  test_util::start(function_name!(), "parse a function with a function reference");
  let input = "fix_swap(Fs, Opts) ->
    case proplists:get_bool(no_swap, Opts) of
        false -> Fs;
        true -> fold_functions(fun swap_moves/1, Fs)
    end.";
  let _m = test_util::parse_module(function_name!(), input);
}

#[named]
#[test]
fn parse_rec_map_builder1() {
  test_util::start(function_name!(), "parse a record+map builder with base value");
  let input = "
    x() -> {NextIndex,Dict#asm{atoms=Atoms#{Atom=>NextIndex}}}.";
  let _m = test_util::parse_module(function_name!(), input);
}

#[named]
#[test]
fn parse_func_with_maps_and_recs() {
  test_util::start(function_name!(), "parse a function with maps and records access");
  let input = "
    -spec atom(atom(), bdict()) -> {pos_integer(), bdict()}.
    atom(Atom, #asm{atoms=Atoms}=Dict) when is_atom(Atom) ->
      case Atoms of #{ Atom := Index} -> {Index,Dict};
          _ -> NextIndex = maps:size(Atoms) + 1, {NextIndex,Dict#asm{atoms=Atoms#{Atom=>NextIndex}}}
      end.";
  let _m = test_util::parse_module(function_name!(), input);
}

#[named]
#[test]
fn parse_func_with_list_decomposition1() {
  test_util::start(function_name!(), "parse a function with list decomposition argument");
  let input = "
    function_renumber([{function,Name,Arity,_Entry,Asm0}|Fs], St0, Acc) ->
      {Asm,St} = renumber_labels(Asm0, [], St0),
      function_renumber(Fs, St, [{function,Name,Arity,St#st.entry,Asm}|Acc]);
    function_renumber([], St, Acc) -> {Acc,St}.";
  let _m = test_util::parse_module(function_name!(), input);
}

#[named]
#[test]
fn parse_func_with_list_decomposition2() {
  test_util::start(function_name!(), "parse a function with some list decomps");
  let input = "f() -> [1|St0#st.lmap]";
  let _m = test_util::parse_module(function_name!(), input);
}

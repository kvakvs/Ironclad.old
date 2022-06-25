//! Parsing macro invocations
//
// use crate::erl_syntax::erl_ast::AstNode;
// use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
// use crate::erl_syntax::parsers::misc::tok;
// use crate::erl_syntax::parsers::parse_expr::parse_expr;
// use crate::erl_syntax::preprocessor::parsers::preprocessor::macro_ident;
// use crate::erl_syntax::preprocessor::pp_define::PreprocessorDefine;
// use crate::erl_syntax::token_stream::token_type::TokenType;
// use nom::branch::alt;
// use nom::character::complete::char;
// use nom::combinator::{cut, map, verify};
// use nom::error::{context, ParseError};
// use nom::multi::separated_list0;
// use nom::sequence::{delimited, pair};
// use nom::Finish;
//
// /// Contains extra info for error reporting
// struct MacroLookupResult {
//   /// If macro is found, this is the definition
//   pub pdef: Option<PreprocessorDefine>,
//   /// If `pdef` was not defined, the `name` will contain the failed lookup name
//   pub name: String,
//   /// If `pdef` was not defined, the `arity` will contain the failed lookup arity
//   pub arity: usize,
// }
//
// impl MacroLookupResult {
//   pub(crate) fn new(
//     name: String,
//     arity: usize,
//     lookup_result: Option<PreprocessorDefine>,
//   ) -> MacroLookupResult {
//     match lookup_result {
//       None => MacroLookupResult { pdef: None, name, arity },
//       Some(pdef) => MacroLookupResult { pdef: Some(pdef), name, arity },
//     }
//   }
// }
//
// /// Parse a `? <IDENT>` for a macro without arguments
// /// Returns `Some(PreprocessorDefine)` to parse, or `None` (macro not defined).
// fn macro_invocation_0(input: ParserInput) -> ParserResult<MacroLookupResult> {
//   // This parser will only work if name/0 macro is defined in input's scope
//   let result = verify(context("macro identifier for macro invocation", cut(macro_ident)), |n1| {
//     input.parser_scope.is_defined_with_arity(n1, 0)
//   })(input.clone());
//
//   if let Ok((input2, n2)) = result {
//     let lookup_result = input.parser_scope.get_value(&n2, 0);
//     let out = MacroLookupResult::new(n2, 0, lookup_result);
//     Ok((input2, out))
//   } else {
//     Err(result.unwrap_err())
//   }
// }
//
// /// Parse a parenthesized list of exprs `( EXPR1, ... )`
// fn macro_args(input: ParserInput) -> ParserResult<Vec<AstNode>> {
//   delimited(
//     tok_par_open,
//     separated_list0(tok_comma, parse_expr),
//     tok_par_close,
//   )(input)
// }
//
// /// Parse a `? <IDENT> ( ARGS, ... )` for a macro with arguments.
// /// Returns `Some(PreprocessorDefine)` to parse, or `None` (macro not defined).
// fn macro_invocation_with_args(input: ParserInput) -> ParserResult<MacroLookupResult> {
//   // This parser will only work if name/0 macro is defined in input's scope
//   map(
//     verify(
//       pair(
//         context("macro identifier for macro invocation", cut(macro_ident)),
//         context("macro invocation arguments", macro_args),
//       ),
//       |(n1, args1)| input.parser_scope.is_defined_with_arity(n1, args1.len()),
//     ),
//     |(n2, args2)| {
//       let lr = input.parser_scope.get_value(&n2, args2.len());
//       MacroLookupResult::new(n2, args2.len(), lr)
//     },
//   )(input.clone())
// }
//
// /// Expect a macro invocation where the macro content will be parsed with the given parser function.
// /// Usually macro insertion points can tell precisely what type of AST they expect.
// /// And to simplify things we can forbid macroing incomplete/partial syntax structures.
// fn macro_invocation<'a, InnerFn: 'a, Out>(
//   input: ParserInput<'a>,
//   macro_content_parser_fn: InnerFn,
// ) -> ParserResult<AstNode>
// where
//   InnerFn: Fn(ParserInput<'a>) -> ParserResult<AstNode>,
// {
//   map(alt((macro_invocation_0, macro_invocation_with_args)), |mlr| -> AstNode {
//     match mlr.pdef {
//       // TODO: Return name/arity from macro_invocation parsers or check inside them
//       None => {
//         panic!(
//           "Macro definition {}/{} is not found in the current preprocessor scope",
//           mlr.name, mlr.arity
//         )
//       }
//       Some(pdef) => {
//         let (_tail, result) = macro_content_parser_fn(input.clone_nested(&pdef.text))
//           .finish()
//           .unwrap();
//         result
//       }
//     }
//   })(input.clone())
// }
//
// /// Expect a macro invocation with 0 or more args, which will expand to an expression
// pub(crate) fn macro_invocation_as_ast_node(input0: ParserInput) -> ParserResult<AstNode> {
//   let maybe_question_mark: nom::IResult<ParserInput, _> = char('?')(input0.clone());
//
//   if let Ok((input1, _)) = maybe_question_mark {
//     context("macro invocation expecting to get an expression", |inp| {
//       macro_invocation::<_, AstNode>(inp, parse_expr)
//     })(input1)
//   } else {
//     Err(nom::Err::Error(nom::error::VerboseError::from_error_kind(
//       input0,
//       nom::error::ErrorKind::Fail,
//     )))
//   }
// }

//! Parsing macro invocations

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::parsers::defs::{ParserInput, ParserResult};
use crate::erl_syntax::parsers::misc::{comma_tag, par_close_tag, par_open_tag, ws_before};
use crate::erl_syntax::parsers::parse_expr::parse_expr;
use crate::erl_syntax::preprocessor::parsers::preprocessor::macro_ident;
use crate::erl_syntax::preprocessor::pp_define::PreprocessorDefine;
use nom::branch::alt;
use nom::character::complete::char;
use nom::combinator::{cut, map, verify};
use nom::error::context;
use nom::multi::separated_list0;
use nom::sequence::{delimited, pair, preceded};
use nom::Finish;
use std::sync::Arc;

/// Contains extra info for error reporting
struct MacroLookupResult {
  /// If macro is found, this is the definition
  pub pdef: Option<Arc<PreprocessorDefine>>,
  /// If `pdef` was not defined, the `name` will contain the failed lookup name
  pub name: String,
  /// If `pdef` was not defined, the `arity` will contain the failed lookup arity
  pub arity: usize,
}

impl MacroLookupResult {
  pub fn new(
    name: String,
    arity: usize,
    lookup_result: Option<Arc<PreprocessorDefine>>,
  ) -> MacroLookupResult {
    match lookup_result {
      None => MacroLookupResult { pdef: None, name, arity },
      Some(pdef) => MacroLookupResult { pdef: Some(pdef), name, arity },
    }
  }
}

/// Parse a `? <IDENT>` for a macro without arguments
/// Returns `Some(PreprocessorDefine)` to parse, or `None` (macro not defined).
fn macro_invocation_0(input: ParserInput) -> ParserResult<MacroLookupResult> {
  // This parser will only work if name/0 macro is defined in input's scope
  map(
    preceded(
      ws_before(char('?')),
      verify(context("macro identifier for macro invocation", cut(macro_ident)), |n1| {
        input.preprocessor_scope.is_defined_with_arity(n1, 0)
      }),
    ),
    |n2| {
      let lr = input.preprocessor_scope.get_value(&n2, 0);
      MacroLookupResult::new(n2.clone(), 0, lr)
    },
  )(input.clone())
}

/// Parse a parenthesized list of exprs `( EXPR1, ... )`
fn macro_args(input: ParserInput) -> ParserResult<Vec<AstNode>> {
  delimited(par_open_tag, separated_list0(comma_tag, parse_expr), par_close_tag)(input)
}

/// Parse a `? <IDENT> ( ARGS, ... )` for a macro with arguments.
/// Returns `Some(PreprocessorDefine)` to parse, or `None` (macro not defined).
fn macro_invocation_n(input: ParserInput) -> ParserResult<MacroLookupResult> {
  // This parser will only work if name/0 macro is defined in input's scope
  map(
    preceded(
      ws_before(char('?')),
      verify(
        pair(
          context("macro identifier for macro invocation", cut(macro_ident)),
          context("macro invocation arguments", macro_args),
        ),
        |(n1, args1)| {
          input
            .preprocessor_scope
            .is_defined_with_arity(n1, args1.len())
        },
      ),
    ),
    |(n2, args2)| {
      let lr = input.preprocessor_scope.get_value(&n2, args2.len());
      MacroLookupResult::new(n2, args2.len(), lr)
    },
  )(input.clone())
}

/// Expect a macro invocation where the macro content will be parsed with the given parser function.
/// Usually macro insertion points can tell precisely what type of AST they expect.
/// And to simplify things we can forbid macroing incomplete/partial syntax structures.
pub fn macro_invocation<'a, InnerFn: 'a, Out>(
  input: ParserInput<'a>,
  inner_fn: InnerFn,
) -> ParserResult<AstNode>
where
  InnerFn: Fn(ParserInput<'a>) -> ParserResult<AstNode>,
{
  map(alt((macro_invocation_0, macro_invocation_n)), |mlr| -> AstNode {
    match mlr.pdef {
      // TODO: Return name/arity from macro_invocation parsers or check inside them
      None => {
        println!("Macro {}/{} was not defined in scope", mlr.name, mlr.arity);
        panic!(
          "Macro definition {}/{} is not found in the current preprocessor scope",
          mlr.name, mlr.arity
        )
      }
      Some(pdef) => {
        let (_tail, result) = inner_fn(input.clone_nested(&pdef.text)).finish().unwrap();
        println!("Macro {}/{} expanded to {}", pdef.name, pdef.args.len(), result);
        result
      }
    }
  })(input.clone())
}

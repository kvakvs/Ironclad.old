//! Groups type definitions shared by all preprocessor parse modules
use crate::erl_syntax::parsers::defs::ParserResult;
use crate::erl_syntax::parsers::misc::{
  dash_atom, period_eol, tok_atom, tok_atom_of, tok_comma, tok_par_close, tok_par_open, tok_string,
};
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::def_undef::{define_directive, undef_directive};
use crate::erl_syntax::preprocessor::parsers::if_ifdef::{
  elif_directive, else_directive, endif_directive, if_directive, ifdef_directive, ifndef_directive,
  tok_macro_ident,
};
use crate::erl_syntax::preprocessor::parsers::parse_attr::parse_any_module_attr;
use crate::erl_syntax::preprocessor::pp_node::pp_impl::PreprocessorNodeImpl;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::source_loc::SourceLoc;
use nom::branch::alt;
use nom::combinator::{cut, map};
use nom::error::context;
use nom::multi::separated_list0;
use nom::sequence::delimited;

/// Parse a `Macroident1, Macroident2, ...` into a list
pub(crate) fn comma_sep_macro_idents(input: ParserInput) -> ParserResult<Vec<String>> {
  separated_list0(tok_comma, tok_macro_ident)(input)
}

/// Parse a `-include(STRING)`
fn include_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  match delimited(
    tok_atom_of("include"),
    delimited(tok_par_open, tok_string, tok_par_close),
    period_eol,
  )(input.clone())
  {
    Ok((input2, path)) => {
      // let included_file = input
      //   .scope
      //   .load_include(SourceLoc::new(&input), &PathBuf::from(path), input.file_name())
      //   .unwrap();
      // let input3 = input2.clone_with_input(included_file);
      // tokenize_source(input3)
      let node =
        PreprocessorNodeImpl::new_include(SourceLoc::new(&input), path.as_str().to_string());
      Ok((input2, node))
    }
    Err(result) => Err(result),
  }
}

/// Parse a `-include_lib(STRING)`
fn include_lib_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      tok_atom_of("include_lib"),
      delimited(tok_par_open, tok_string, tok_par_close),
      period_eol,
    ),
    |t| PreprocessorNodeImpl::new_include_lib(SourceLoc::new(&input), t.as_str().to_string()),
  )(input.clone())
}

/// Parse a `-error(STRING)`
fn error_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      tok_atom_of("error"),
      delimited(tok_par_open, tok_string, tok_par_close),
      period_eol,
    ),
    |t| PreprocessorNodeImpl::new_error(SourceLoc::new(&input), t.as_str().to_string()),
  )(input.clone())
}

/// Parse a `-warning(STRING)`
fn warning_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  map(
    delimited(
      tok_atom_of("warning"),
      delimited(tok_par_open, tok_string, tok_par_close),
      period_eol,
    ),
    |t| PreprocessorNodeImpl::new_warning(SourceLoc::new(&input), t.as_str().to_string()),
  )(input.clone())
}

/// Parses a `-module(atom).` attribute.
/// Dash `-` and terminating `.` are matched outside by the caller.
/// Will create error if the attribute does not parse (essentially a required attribute).
pub(crate) fn module_start_attr(input: ParserInput) -> ParserResult<PreprocessorNode> {
  context(
    "expected -module() attribute",
    delimited(
      |i1| dash_atom(i1, "module"),
      context(
        "the module name in a -module() attribute",
        map(cut(delimited(tok_par_open, tok_atom, tok_par_close)), |modname: String| {
          PreprocessorNodeImpl::new_module_start(SourceLoc::new(&input), modname)
        }),
      ),
      period_eol,
    ),
  )(input.clone())
}

/// Parse one of supported preprocessor directives
pub(crate) fn parse_preproc_directive(input: ParserInput) -> ParserResult<PreprocessorNode> {
  alt((
    alt((
      context("'-module()' opening attribute", module_start_attr),
      // -define is special, it needs closing ).\n to consume the content
      context("'-define()' directive", define_directive),
      context("'-undef()' directive", undef_directive),
      // temporary nodes used by parse_if_block
      context("'-endif()' directive", endif_directive),
      context("'-elif()' directive", elif_directive),
      context("'-else()' directive", else_directive),
      context("'-ifdef()' directive", ifdef_directive),
      context("'-ifndef()' directive", ifndef_directive),
      context("'-if()' directive", if_directive),
    )),
    context("'-warning()' directive", warning_directive),
    context("'-error()' directive", error_directive),
    context("'-include_lib()' directive", include_lib_directive),
    context("'-include()' directive", include_directive),
    parse_any_module_attr,
  ))(input)
}

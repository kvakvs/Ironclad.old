//! Quick scan through a source file, split it using preprocessor directives as a divider

use std::ops::Deref;
use std::sync::Arc;
use nom::{combinator, sequence, multi, character::complete::{char, alphanumeric1},
          bytes::complete::{tag}, branch, error::{context}, character};
use crate::erlang::syntax_tree::nom_parse::ErlParser;
use crate::erlang::syntax_tree::nom_parse::misc::MiscParser;
use crate::erlang::syntax_tree::nom_parse::parse_str::StringParser;
use crate::preprocessor::syntax_tree::pp_ast::PpAst;

/// Gathers multiple errors and contexts together
pub type PpParserError<'a> = nom::error::VerboseError<&'a str>;

/// Generic return value from a Nom parser which takes &str and returns `Out`
pub type ParserResult<'a, Out> = nom::IResult<&'a str, Out, PpParserError<'a>>;

/// Return value from a Nom parser which takes &str and returns `Arc<PpAst>`
pub type PpAstParserResult<'a> = ParserResult<'a, Arc<PpAst>>;

/// Return value from a Nom parser which takes &str and returns `Vec<Arc<PpAst>>`
pub type VecPpAstParserResult<'a> = ParserResult<'a, Vec<Arc<PpAst>>>;

/// Return value from a Nom parser which takes &str and returns `String`
pub type StringParserResult<'a> = ParserResult<'a, String>;

/// Return value from a Nom parser which takes &str and returns `&str`
pub type StrSliceParserResult<'a> = ParserResult<'a, &'a str>;

/// Return value from a Nom parser which takes &str and returns `()`
pub type VoidParserResult<'a> = ParserResult<'a, ()>;

/// Groups code for parsing preprocessor directives
pub struct PreprocessorParser {}

impl PreprocessorParser {
  /// Parse a `Var1, Var2, ...` into a list
  fn parse_comma_sep_varnames(input: &str) -> ParserResult<Vec<String>> {
    multi::separated_list0(
      MiscParser::ws_before(char(',')),
      MiscParser::parse_varname,
    )(input)
  }

  fn terminator(input: &str) -> ParserResult<&str> {
    combinator::recognize(
      sequence::tuple((
        MiscParser::ws_before(char(')')),
        MiscParser::ws_before(char('.')),
        MiscParser::newline_or_eof,
      ))
    )(input)
  }

  /// Parse a `-define(NAME)` or `-define(NAME, VALUE)` or `-define(NAME(ARGS,...), VALUE)`
  pub fn parse_define(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("define")),
        sequence::tuple((
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(MiscParser::parse_varname),
          combinator::opt(sequence::delimited(
            MiscParser::ws_before(char('(')),
            Self::parse_comma_sep_varnames,
            MiscParser::ws_before(char(')')),
          )),
          combinator::opt(sequence::delimited(
            MiscParser::ws_before(char(',')),
            multi::many_till(
              nom::character::complete::anychar,
              Self::terminator),
            Self::terminator,
          )), // )delimited )opt
          MiscParser::ws_before(char(')')),
        )),
      ),
      |(_open, name, args, body, _close)| {
        PpAst::new_define(
          name,
          args,
          body.map(|(chars, _term)| chars.into_iter().collect::<String>()))
      },
    )(input)
  }

  /// Parse a `-if(EXPR)` and return a temporary node
  fn parse_if_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("if")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(ErlParser::parse_expr),
          MiscParser::ws_before(char(')')),
        ),
      ),
      |expr| PpAst::new_if_temporary(expr),
    )(input)
  }

  /// Parse a `-if(EXPR).` `<LINES>` then optional `-else. <LINES> -endif.`
  fn parse_if_block(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::tuple((
        MiscParser::ws_before(Self::parse_if_temporary),
        // Consume lines and directives until an `-else` or `-endif`
        multi::many0(
          combinator::verify(
            Self::parse_fragment,
            |frag: &Arc<PpAst>| !frag.is_else() && !frag.is_elseif(),
          )),
        // Optional -else. <LINES> block
        combinator::opt(
          sequence::preceded(
            Self::parse_else_temporary,
            multi::many0(Self::parse_fragment),
          )
        ),
        Self::consume_endif,
      )),
      |(pp_if_expr, branch_true, branch_false, _endif)| {
        if let PpAst::_TemporaryIf(if_expr) = pp_if_expr.deref() {
          PpAst::new_if(if_expr.clone(), Some(branch_true), branch_false)
        } else {
          unreachable!("This code path should not execute")
        }
      },
    )(input)
  }

  /// Parse an identifier, starting with lowercase and also can be containing numbers and underscoress
  fn parse_macro_ident(input: &str) -> StringParserResult {
    combinator::map(
      combinator::recognize(
        sequence::pair(
          combinator::verify(character::complete::anychar,
                             |c: &char| c.is_alphabetic() || *c == '_'),
          multi::many1(branch::alt((alphanumeric1, tag("_")))),
        )
      ),
      |result: &str| result.to_string(),
    )(input)
  }

  /// Parse a `-undef(IDENT)`
  fn parse_undef(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("undef")),
        sequence::tuple((
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(Self::parse_macro_ident),
          MiscParser::ws_before(char(')')),
        )),
      ),
      |(_open, ident, _close)| PpAst::new_undef(ident),
    )(input)
  }

  /// Parse a `-include(STRING)`
  fn parse_include(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("include")),
        sequence::tuple((
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(StringParser::parse_string),
          MiscParser::ws_before(char(')')),
        )),
      ),
      |(_open, s, _close)| PpAst::new_include(s),
    )(input)
  }

  /// Parse a `-include_lib(STRING)`
  fn parse_include_lib(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("include_lib")),
        sequence::tuple((
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(StringParser::parse_string),
          MiscParser::ws_before(char(')')),
        )),
      ),
      |(_open, s, _close)| PpAst::new_include_lib(s),
    )(input)
  }

  /// Parse a `-elif(EXPR)` into a temporary AST node
  fn parse_elif_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("elif")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(ErlParser::parse_expr),
          MiscParser::ws_before(char(')')),
        ),
      ), |expr| PpAst::new_elif_temporary(expr),
    )(input)
  }

  /// Parse a `-ifdef(MACRO_NAME)`
  fn parse_ifdef_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("ifdef")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(Self::parse_macro_ident),
          MiscParser::ws_before(char(')')),
        ),
      ),
      |s| PpAst::new_ifdef_temporary(s),
    )(input)
  }

  /// Parse a `-ifndef(MACRO_NAME)`
  fn parse_ifndef_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("ifndef")),
        sequence::delimited(
          MiscParser::ws_before(char('(')),
          MiscParser::ws_before(Self::parse_macro_ident),
          MiscParser::ws_before(char(')')),
        ),
      ),
      |s| PpAst::new_ifndef_temporary(s),
    )(input)
  }

  /// Parse a `-else.`, return a temporary `Else` node, which will not go into final `PpAst`
  fn parse_else_temporary(input: &str) -> PpAstParserResult {
    combinator::map(
      sequence::preceded(
        MiscParser::ws_before(tag("else")),
        combinator::opt(
          sequence::pair(
            MiscParser::ws_before(char('(')),
            MiscParser::ws_before(char(')')),
          )
        ),
      ),
      |_opt| PpAst::_TemporaryElse.into(),
    )(input)
  }

  /// Parse a `-endif.` and return it as a `&str` slice
  fn consume_endif(input: &str) -> StrSliceParserResult {
    combinator::recognize(
      sequence::preceded(
        MiscParser::ws_before(tag("endif")),
        combinator::opt(
          sequence::pair(
            MiscParser::ws_before(char('(')),
            MiscParser::ws_before(char(')')),
          )
        ),
      ),
    )(input)
  }

  fn parse_preproc_directive(input: &str) -> PpAstParserResult {
    sequence::delimited(
      // Preceded by a dash -
      MiscParser::ws_before(char('-')),
      MiscParser::ws_before_mut(branch::alt((
        branch::alt((
          // -define is special, it needs closing ).\n to consume the content
          context("'-define' directive", Self::parse_define),
          context("'-undef' directive", Self::parse_undef),

          // temporary nodes used by parse_if_block
          context("'-elif' directive", Self::parse_elif_temporary),
          context("'-else' directive", Self::parse_else_temporary),
          context("'-ifdef' directive", Self::parse_ifdef_temporary),
          context("'-ifndef' directive", Self::parse_ifndef_temporary),
          context("'-if' directive", Self::parse_if_block), // if must go after longer words ifdef and ifndef

          // Self::parse_error,
          // Self::parse_warning,
          context("'-include_lib' directive", Self::parse_include_lib),
          context("'-include' directive", Self::parse_include),
        )),
      ))),
      // Terminated by .\n
      sequence::pair(
        MiscParser::ws_before(char('.')),
        MiscParser::newline_or_eof),
    )(input)
  }

  /// Parse full lines till a line which looks like a preprocessor directive is found
  fn consume_one_line_of_text(input: &str) -> PpAstParserResult {
    combinator::map(
      combinator::verify(
        MiscParser::ws(nom::bytes::complete::take_till(|c| c == '\n' || c == '\r')),
        |text: &str| !text.is_empty(), //&& !text.starts_with('-'),
      ),
      PpAst::new_text,
    )(input)
  }

  /// Parses either a preprocessor directive or block, or consumes one line of text
  fn parse_fragment(input: &str) -> PpAstParserResult {
    branch::alt((
      Self::parse_preproc_directive,
      Self::consume_one_line_of_text,
      // A final comment in file is not visible to consume_text
      combinator::map(MiscParser::parse_line_comment, |_| PpAst::new_text("")),
    ))(input)
  }

  /// Split input into AST nodes for preprocessor directives and any irrelevant text in between
  pub fn parse_fragments_collection(input: &str) -> VecPpAstParserResult {
    // Followed by 1 or more directive or another text fragment
    multi::many1(
      Self::parse_fragment
    )(input)
  }

  /// Parses file contents into mix of preprocessor directives and text fragments.
  /// Comments are eliminated.
  pub fn parse_module(input: &str) -> PpAstParserResult {
    let (input, fragments) = Self::parse_fragments_collection(input)?;
    Ok((input, PpAst::new_file(fragments)))
  }
}

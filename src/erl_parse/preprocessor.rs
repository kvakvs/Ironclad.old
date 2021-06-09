use crate::project::ErlProject;
use crate::erl_parse::pp_ast::PpAstNode;
use crate::erl_parse::helpers::ws;
use nom::combinator::{recognize, map_res, map};
use nom::sequence::{pair, tuple, delimited};
use nom::branch::alt;
use nom::character::complete::{alpha1, alphanumeric1, newline, anychar};
use nom::bytes::complete::{tag, take_till1};
use nom::multi::{many0, many_till};
use crate::erl_parse::Span;

/// Does rough preparse of ERL files, only being interested in -include, -ifdef, ... etc
/// preprocessor directives, and in macro applications.
///
/// -define(Name(...), ...).
/// -if(Bool), -ifdef(Macro), -ifndef(Macro), -undef(Macro), -else, -elif(Bool), -endif
/// -error(Term), -warning(Term) (OTP 19+)
/// ?MODULE, ?MODULE_STRING, ?FILE, ?LINE, ?MACHINE='BEAM', ?FUNCTION_NAME, ?FUNCTION_ARITY,
/// ?OTP_RELEASE (OTP 21+)
/// ??MACRO to stringify the tokens in the macro argument
// pub fn preprocess_source(project: &mut ErlProject, src: String) -> String {
//     dbg!("Preprocess notimpl");
//     return src;
// }

fn parse_macro_ident(input: &str) -> nom::IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn parse_attr_ident(input: &str) -> nom::IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

/// Parse a module attribute with 1+ parameters
fn parse_line(input: &str) -> nom::IResult<&str, PpAstNode> {
    map(
        many_till(anychar, newline),
        |(text, _)| PpAstNode::Text,
    )(input)
}

fn parse_attr_1(input: &str) -> nom::IResult<&str, PpAstNode> {
    map(
        tuple((
            // Consume -attr_name(
            ws(tag("-")),
            parse_attr_ident,
            ws(tag("(")),

            // Consume first parameter
            parse_macro_ident,

            // Consume comma separated attr parameters till the end of the attribute
            many_till(
                tuple((
                    ws(tag(",")),
                    parse_macro_ident,
                )),
                tag(")."),
            ),
        )),
        |(_, attr_ident, _, ident, args)| -> PpAstNode {
            // PpAstNode::DefineMacro(String::from(ident), Span::from(value))
            PpAstNode::Else
        },
    )(input)
}

pub fn parse_module(input: &str) -> nom::IResult<&str, Vec<PpAstNode>> {
    many0(
        alt((
            parse_attr_1,
            parse_line, ))
    )(input)
}

use crate::erl_module::ErlModule;
use crate::erl_parse::helpers::ws;
use crate::erl_parse::atom::parse_atom;
use crate::erl_parse::ast::ASTNode;

mod helpers;
mod preprocessor;
mod atom;
pub mod ast;

// fn parse_module_attribute(i: &str) -> nom::IResult<String, AST> {
//     nom::sequence::tuple((
//         ws(nom::bytes::complete::tag("-module")),
//         parse_atom,
//         ws(nom::bytes::complete::tag(")")),
//     ))(i)
// }

pub fn parse_module(i: &str) -> nom::IResult<&str, Vec<ASTNode>> {
    // let forms = nom::branch::alt((
    //     // parse_module_attribute,
    //     // parse_function,
    // ))(i);

    // ErlModule::from_forms(forms)
    Ok(("", vec![]))
}

// pub fn parse_test() {
//     let input = "-module(fgsfds).\n\
//     myfun(Args) -> ok.\n\
//     ";
//     println!("{:?}", parse_module(input));
// }

// //! Erlang preprocess AST tree
// use crate::preprocessor::syntax_tree::pp_ast::{PpAst};
// use crate::project::source_file::SourceFile;
// use pest::iterators::{Pair};
// use pest::Parser;
// use std::sync::Arc;
// use crate::erl_error::ErlResult;
// use crate::preprocessor::nom_parser::PreprocessorParser;
//
// impl PpAst {
//   /// Does rough preparse of ERL files, only being interested in -include, -ifdef, macros, ... etc
//   ///
//   /// -define(Name(...), ...).
//   /// -if(Bool), -ifdef(Macro), -ifndef(Macro), -undef(Macro), -else, -elif(Bool), -endif
//   /// -error(Term), -warning(Term) (OTP 19+)
//   /// ?MODULE, ?MODULE_STRING, ?FILE, ?LINE, ?MACHINE='BEAM', ?FUNCTION_NAME, ?FUNCTION_ARITY,
//   /// ?OTP_RELEASE (OTP 21+)
//   /// ??MACRO to stringify the tokens in the macro argument
//   ///
//   /// Return: Parsed preprocess forms list (directives, and text fragments and comments)
//   /// Lifetime note: Parse input string must live at least as long as parse tree is alive
//   pub fn from_source_file(source_file: &Arc<SourceFile>) -> ErlResult<Arc<PpAst>> {
//     // let successful_parse = PpParser::parse(Rule::file, &source_file.text)?.next().unwrap();
//     // let pp_tree = Arc::new(PpAst::Empty);
//     // pp_tree.pp_parse_tokens_to_ast(successful_parse)
//     let (tail, ast) = PreprocessorParser::parse_module(&source_file.text)?;
//     if tail.trim().is_empty() {
//       Ok(ast)
//     } else {
//       panic!("Preprocessor parse failed, not all input was consumed.\nRemaining: «{}»", tail)
//     }
//   }
//
//   fn string_from(pair: Pair<Rule>) -> ErlResult<String> {
//     Ok(String::from(pair.into_inner().as_str()))
//   }
//
//   fn string_from_quoted_string(pair: Pair<Rule>) -> ErlResult<String> {
//     Self::string_from(pair.into_inner().next().unwrap())
//   }
//
//   /// Convert Pest syntax token tree produced by the Pest PEG parser into Preprocessor AST tree
//   pub fn pp_parse_tokens_to_ast(&self, pair: Pair<Rule>) -> ErlResult<Arc<PpAst>> {
//     let result: Arc<PpAst> = match pair.as_rule() {
//       Rule::file => {
//         // Parse all nested file elements, comments and text fragments
//         let ast_nodes = pair.into_inner()
//             .map(|p| self.pp_parse_tokens_to_ast(p))
//             .map(Result::unwrap)
//             .collect::<Vec<Arc<PpAst>>>();
//         PpAst::File(ast_nodes).into()
//       }
//
//       Rule::text => {
//         PpAst::Text(String::from(pair.as_str())).into()
//       }
//
//       Rule::pp_include => {
//         PpAst::Include(Self::string_from_quoted_string(pair)?).into()
//       }
//
//       Rule::pp_include_lib => {
//         PpAst::IncludeLib(Self::string_from_quoted_string(pair)?).into()
//       }
//
//       Rule::pp_ifdef => {
//         PpAst::Ifdef(Self::string_from(pair)?).into()
//       }
//       Rule::pp_ifndef => {
//         PpAst::Ifndef(Self::string_from(pair)?).into()
//       }
//
//       // -if and -elif can have boolean expressions in them
//       Rule::pp_if => {
//         PpAst::If(Self::string_from(pair)?).into()
//       }
//       Rule::pp_elif => {
//         PpAst::Elif(Self::string_from(pair)?).into()
//       }
//
//       Rule::pp_else => PpAst::Else.into(),
//       Rule::pp_endif => PpAst::Endif.into(),
//
//       Rule::pp_error => {
//         PpAst::Error(String::from(pair.into_inner().as_str())).into()
//       }
//       Rule::pp_warning => {
//         PpAst::Warning(String::from(pair.into_inner().as_str())).into()
//       }
//
//       Rule::pp_define => {
//         let mut inner = pair.into_inner();
//         let name = String::from(inner.next().unwrap().as_str());
//         let body = String::from(inner.next().unwrap().as_str());
//         PpAst::Define(name, body).into()
//       }
//
//       Rule::pp_define_fun => {
//         let mut inner = pair.into_inner();
//         let name = String::from(inner.next().unwrap().as_str());
//         let args = inner.next().unwrap().into_inner()
//             .into_iter()
//             .map(|n| String::from(n.as_str()))
//             .collect();
//         let body = String::from(inner.next().unwrap().as_str());
//         PpAst::DefineFun { name, args, body }.into()
//       }
//
//       Rule::COMMENT => {
//         PpAst::Comment(String::from(pair.as_str())).into()
//       }
//
//       other => unreachable!("PpAst value: {:?}", other),
//     };
//     Ok(result)
//   }
// }

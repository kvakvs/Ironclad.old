//! Preprocessing support for `ErlModule`

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::parse_pp::parse_preproc_directive;
use crate::erl_syntax::preprocessor::pp_define::PreprocessorDefineImpl;
use crate::erl_syntax::preprocessor::pp_node::pp_type::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::token::{format_tok_stream, Token};
use crate::erl_syntax::token_stream::token_line_iter::TokenLinesIter;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::error::ic_error::IcResult;
use crate::project::module::mod_impl::{ErlModule, ErlModuleImpl};
use crate::record_def::RecordDefinition;
use crate::typing::erl_type::ErlType;
use ::function_name::named;
use libironclad_util::mfarity::MFArity;
use nom::Finish;

struct PreprocessState<'a> {
  pub result: Vec<Token>,
  pub itr: TokenLinesIter<'a>,
  pub too_many_errors: bool,
  pub module: &'a ErlModule,
}

/// Given a next input line (till newline), check whether it is a preprocessor directive, and
/// whether it does not end with `).\n` or `.\n` - in this case we try to add one more line to it
/// till it is complete or till end of input is reached.
fn try_consume_entire_directive<'a>(
  line: &'a [Token],
  state: &mut PreprocessState<'a>,
) -> &'a [Token] {
  // The line is a beginning of an attribute or a preprocessor definition or condition
  // These can only span one or more full lines, so we can work with lines iterator

  // Expand the line slice till we find the terminator symbol `period + end of line`
  let mut result = line;
  while !Token::ends_with(line, &[TokenType::Period, TokenType::EOL]) && !state.itr.eof() {
    if let Some(expanded) = state.itr.expand_till_next_line() {
      result = expanded;
    } else {
      break; // end of input
    }
  }
  result
}

fn on_define(state: &mut PreprocessState, name: &str, args: &[String], body: &[Token]) {
  {
    let key = MFArity::new_local(name, args.len());
    let ppdef = PreprocessorDefineImpl::new(name.to_string(), args, body);
    state.module.root_scope.defines.add(key, ppdef)
  }
}

fn on_export(state: &mut PreprocessState, fun_arities: &[MFArity]) {
  fun_arities
    .iter()
    .for_each(|fun_arity| state.module.root_scope.exports.add(fun_arity.clone()))
}

fn on_export_type(state: &mut PreprocessState, type_arities: &[MFArity]) {
  type_arities.iter().for_each(|type_arity| {
    state
      .module
      .root_scope
      .exported_types
      .add(type_arity.clone())
  })
}

fn on_import(state: &mut PreprocessState, module_name: &str, fun_arities: &[MFArity]) {
  fun_arities.iter().for_each(|fun_arity| {
    state
      .module
      .root_scope
      .imports
      .add(fun_arity.clone_with_module(module_name))
  })
}

fn on_new_type(state: &mut PreprocessState, name: &str, vars: &[String], ty: ErlType) {
  let key = MFArity::new_local(name, vars.len());
  state.module.root_scope.user_types.add(key, ty)
}

fn on_new_record(state: &mut PreprocessState, tag: &str, fields: &[RecordField]) {
  let r_def = RecordDefinition { tag: tag.to_string(), fields: fields.into() }.into();
  state
    .module
    .root_scope
    .record_defs
    .add(tag.to_string(), r_def)
}

fn on_fn_spec(state: &mut PreprocessState, funarity: &MFArity, spec: &ErlType) {
  state
    .module
    .root_scope
    .fn_specs
    .add(funarity.clone(), spec.clone())
}

fn on_attr(state: &mut PreprocessState, tag: &str, term: &Option<AstNode>) {
  state.module.root_scope.add_attr(tag, term.clone())
}

fn preprocess_handle_ppnode(ppnode: PreprocessorNode, state: &mut PreprocessState) {
  match &ppnode.content {
    PreprocessorNodeType::ModuleName { name } => {
      ErlModuleImpl::set_name(state.module, name.as_str())
    }
    PreprocessorNodeType::Include(_) => unimplemented!(),
    PreprocessorNodeType::IncludeLib(_) => unimplemented!(),
    PreprocessorNodeType::Define { name, args, body } => {
      on_define(state, name.as_str(), args, body)
    }
    PreprocessorNodeType::Undef(_) => unimplemented!(),
    PreprocessorNodeType::Error(_) => unimplemented!(),
    PreprocessorNodeType::Warning(_) => unimplemented!(),
    PreprocessorNodeType::IncludedFile { .. } => unimplemented!(),
    PreprocessorNodeType::Attr { tag, term } => on_attr(state, tag.as_str(), term),
    PreprocessorNodeType::Export { fun_arities } => on_export(state, fun_arities),
    PreprocessorNodeType::ExportType { type_arities } => on_export_type(state, type_arities),
    PreprocessorNodeType::Import { module: module_name, fun_arities } => {
      on_import(state, module_name.as_str(), fun_arities)
    }
    PreprocessorNodeType::NewType { name, vars, ty } => {
      on_new_type(state, name.as_str(), vars, ty.clone())
    }
    PreprocessorNodeType::NewRecord { tag, fields } => on_new_record(state, tag, fields),
    PreprocessorNodeType::FnSpec { funarity, spec } => on_fn_spec(state, funarity, spec),

    PreprocessorNodeType::If { .. } => unimplemented!(),
    PreprocessorNodeType::ElseIf { .. } => unimplemented!(),
    PreprocessorNodeType::Ifdef { .. } => unimplemented!(),
    PreprocessorNodeType::Ifndef { .. } => unimplemented!(),
    PreprocessorNodeType::Else => {
      unimplemented!()
      // state.too_many_errors = state.too_many_errors
      //   || state.module.add_error(ErlError::preprocessor_error(
      //     SourceLoc::unimplemented(file!(), function_name!()),
      //     "Unexpected preprocessor -else.".to_string(),
      //   ))
    }
    PreprocessorNodeType::Endif => {
      unimplemented!()
      // state.too_many_errors = state.too_many_errors
      //   || state.module.add_error(ErlError::preprocessor_error(
      //     SourceLoc::unimplemented(file!(), function_name!()),
      //     "Unexpected preprocessor -endif.".to_string(),
      //   ))
    }
  }
}

#[inline]
fn line_contains_preprocessor_directive(line: &[Token]) -> bool {
  // Begins with a -
  // Followed by an atom, or an "else" keyword, because -else() tokenizes as a keyword
  line.len() > 2
    && line[0].is_tok(TokenType::Minus)
    && (line[1].is_atom() || line[1].is_keyword(Keyword::Else))
}

impl ErlModuleImpl {
  /// Filter through the tokens array and produce a new token array with preprocessor directives
  /// eliminated, files included and macros substituted.
  pub fn preprocess(
    original_input: &str,
    module: &ErlModule,
    tokens: &[Token],
  ) -> IcResult<Vec<Token>> {
    let mut state = PreprocessState {
      result: Vec::with_capacity(tokens.len()),
      itr: TokenLinesIter::new(tokens),
      too_many_errors: false,
      module,
    };

    while let Some(mut line) = state.itr.next() {
      if state.too_many_errors {
        break;
      }

      if line_contains_preprocessor_directive(&line) {
        line = try_consume_entire_directive(line, &mut state);
        println!("Next line: {}", format_tok_stream(line, 50));

        //---------------
        // Parse the accumulated one or more lines as a preprocessor directive
        // or a module attribute
        //---------------
        let parser_input = ParserInput::new_slice(line);
        let (tail, ppnode) = panicking_parser_error_reporter(
          original_input,
          parser_input.clone(),
          parse_preproc_directive(parser_input).finish(),
        );
        let trim_tail = tail.tokens.iter().filter(|t| !t.is_eol()).count();
        assert_eq!(trim_tail, 0,
                "Not all input consumed while parsing a preprocessor directive or a module attribute:\n{}",
                format_tok_stream(tail.tokens, 100));

        preprocess_handle_ppnode(ppnode, &mut state);
      } else {
        // copy the line contents
        state.result.extend(line.iter().cloned())
      }
    }

    println!("Preprocessor: remaining/resulting tokens:");
    state.result.iter().for_each(|t| print!("{}", t));
    println!();

    Ok(state.result)
  }
}

//! Preprocessing support for `ErlModule`

use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::parsers::misc::panicking_parser_error_reporter;
use crate::erl_syntax::parsers::parser_input::ParserInput;
use crate::erl_syntax::preprocessor::parsers::parse_pp::parse_preproc_directive;
use crate::erl_syntax::preprocessor::pp_define::PreprocessorDefineImpl;
use crate::erl_syntax::preprocessor::pp_node::pp_type::PreprocessorNodeType;
use crate::erl_syntax::token_stream::token::{format_tok_stream, Token};
use crate::erl_syntax::token_stream::token_line_iter::TokenLinesIter;
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::error::ic_error::IcResult;
use crate::project::module::mod_impl::{ErlModule, ErlModuleImpl};
use crate::record_def::RecordDefinition;
use crate::source_loc::SourceLoc;
use ::function_name::named;
use libironclad_util::mfarity::MFArity;
use nom::Finish;

struct PreprocessState<'a> {
  pub result: Vec<Token>,
  pub itr: TokenLinesIter<'a>,
  pub too_many_errors: bool,
}

impl ErlModuleImpl {
  /// Given a next input line (till newline), check whether it is a preprocessor directive, and
  /// whether it does not end with `).\n` or `.\n` - in this case we try to add one more line to it
  /// till it is complete or till end of input is reached.
  fn preprocess_expand_line<'a>(line: &'a [Token], state: &mut PreprocessState<'a>) -> &'a [Token] {
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

  /// Filter through the tokens array and produce a new token array with preprocessor directives
  /// eliminated, files included and macros substituted.
  #[named]
  pub fn preprocess(
    original_input: &str,
    module: &ErlModule,
    tokens: &[Token],
  ) -> IcResult<Vec<Token>> {
    let mut state = PreprocessState {
      result: Vec::with_capacity(tokens.len()),
      itr: TokenLinesIter::new(tokens),
      too_many_errors: false,
    };

    while let Some(mut line) = state.itr.next() {
      if state.too_many_errors {
        break;
      }

      if line.len() > 2 && line[0].is_tok(TokenType::Minus) && line[1].is_atom() {
        line = Self::preprocess_expand_line(line, &mut state);
        println!("Next line: {}", format_tok_stream(line, 50));

        let parser_input = ParserInput::new_slice(line);

        let (tail, ppnode) = panicking_parser_error_reporter(
          original_input,
          parser_input.clone(),
          parse_preproc_directive(parser_input).finish(),
        );
        let trim_tail = tail.tokens.iter().filter(|t| !t.is_eol()).count();
        assert_eq!(trim_tail, 0,
                "Not all input consumed while parsing a preprocessor directive or a module attribute:\n{}",
                format_tok_stream(&tail.tokens, 100));
        // println!("Parsed -<ident> line into {:?}", &ppnode);

        match &ppnode.content {
          PreprocessorNodeType::ModuleName { name } => {
            ErlModuleImpl::set_name(module, name.as_str())
          }
          PreprocessorNodeType::Include(_) => {}
          PreprocessorNodeType::IncludeLib(_) => {}
          PreprocessorNodeType::Define { name, args, body } => {
            let key = MFArity::new_local(name.as_str(), args.len());
            let ppdef = PreprocessorDefineImpl::new(name.clone(), &args, &body);
            module.root_scope.defines.add(key, ppdef)
          }
          PreprocessorNodeType::Undef(_) => {}
          PreprocessorNodeType::Error(_) => {}
          PreprocessorNodeType::Warning(_) => {}
          PreprocessorNodeType::IncludedFile { .. } => {}
          PreprocessorNodeType::Attr { tag, term } => module.root_scope.add_attr(tag, term.clone()),
          PreprocessorNodeType::Export { fun_arities } => fun_arities
            .iter()
            .for_each(|fun_arity| module.root_scope.exports.add(fun_arity.clone())),
          PreprocessorNodeType::ExportType { type_arities } => type_arities
            .iter()
            .for_each(|type_arity| module.root_scope.exported_types.add(type_arity.clone())),
          PreprocessorNodeType::Import { module: module_name, fun_arities } => {
            fun_arities.iter().for_each(|fun_arity| {
              module
                .root_scope
                .imports
                .add(fun_arity.clone_with_module(module_name.as_str()))
            })
          }
          PreprocessorNodeType::NewType { name, vars, ty } => {
            let key = MFArity::new_local(name.as_str(), vars.len());
            module.root_scope.user_types.add(key, ty.clone())
          }
          PreprocessorNodeType::NewRecord { tag, fields } => {
            let r_def = RecordDefinition { tag: tag.clone(), fields: fields.clone() }.into();
            module.root_scope.record_defs.add(tag.clone(), r_def)
          }
          PreprocessorNodeType::FnSpec { funarity, spec } => module
            .root_scope
            .fn_specs
            .add(funarity.clone(), spec.clone()),

          PreprocessorNodeType::If(_) => {}
          PreprocessorNodeType::ElseIf(_) => {}
          PreprocessorNodeType::Ifdef(_) => {}
          PreprocessorNodeType::Ifndef(_) => {}
          PreprocessorNodeType::IfdefBlock { .. } => {}
          PreprocessorNodeType::IfBlock { .. } => {}
          PreprocessorNodeType::Else => {
            state.too_many_errors = state.too_many_errors
              || module.add_error(ErlError::preprocessor_error(
                SourceLoc::unimplemented(file!(), function_name!()),
                "Unexpected preprocessor -else.".to_string(),
              ))
          }
          PreprocessorNodeType::Endif => {
            state.too_many_errors = state.too_many_errors
              || module.add_error(ErlError::preprocessor_error(
                SourceLoc::unimplemented(file!(), function_name!()),
                "Unexpected preprocessor -endif.".to_string(),
              ))
          }
        }

        // let pptok = Token::new(line[0].offset, TokenType::Preprocessor(ppnode));
        // result.push(pptok);
      } else {
        // copy the line contents
        state.result.extend(line.iter().cloned())
      }
    }

    // TODO: Interpret -define/undef -if/ifdef/ifndef/else
    // TODO: Interpret -include and -include_lib
    // TODO: Parse and store other module attributes
    println!("Preprocessor: resulting tokens:");
    state.result.iter().for_each(|t| print!("{}", t));
    println!();

    Ok(state.result)
  }
}

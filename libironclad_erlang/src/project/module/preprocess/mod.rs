//! Preprocessing support for `ErlModule`

use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::literal_bool::LiteralBool;
use crate::erl_syntax::node::erl_record::RecordField;
use crate::erl_syntax::preprocessor::pp_define::PreprocessorDefineImpl;
use crate::erl_syntax::preprocessor::pp_node::pp_type::PreprocessorNodeType;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::token::{format_tok_stream, Token};
use crate::erl_syntax::token_stream::token_type::TokenType;
use crate::error::ic_error::IcResult;
use crate::project::module::mod_impl::{ErlModule, ErlModuleImpl};
use crate::project::module::preprocess::pp_tok_stream::TokenStream;
use crate::record_def::RecordDefinition;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::ErlType;
use ::function_name::named;
use libironclad_util::mfarity::MFArity;
use pp_state::PreprocessState;

pub mod pp_section;
pub mod pp_state;
pub mod pp_tok_stream;

/// Given a next input line (till newline), check whether it is a preprocessor directive, and
/// whether it does not end with `).\n` or `.\n` - in this case we try to add one more line to it
/// till it forms a complete-looking preprocessor or attribute, or till the end of input is reached.
fn expand_till_directive_end<'a>(
  line: &'a [Token],
  state: &mut PreprocessState<'a>,
) -> &'a [Token] {
  // The line is a beginning of an attribute or a preprocessor definition or condition
  // These can only span one or more full lines, so we can work with lines iterator

  // Expand the line slice till we find the terminator symbol `period + end of line`
  let mut result = line;
  while !ends_with_dot_eol(result) && !state.itr.eof() {
    if let Some(expanded) = state.itr.expand_till_next_line() {
      result = expanded;
    } else {
      break; // end of input
    }
  }
  result
}

#[inline]
fn ends_with_dot_eol(line: &[Token]) -> bool {
  Token::ends_with(line, &[TokenType::Period, TokenType::EOL])
}

fn on_undef(state: &mut PreprocessState, name: &str) {
  state
    .module
    .root_scope
    .defines
    .delete_if(|key, _value| key.name == name);
}

fn on_define(state: &mut PreprocessState, name: &str, args: &[String], body: &[Token]) {
  let key = MFArity::new_local(name, args.len());
  let ppdef = PreprocessorDefineImpl::new(name.to_string(), args, body);
  state.module.root_scope.defines.add(key, ppdef);
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

#[named]
fn on_if(state: &mut PreprocessState, ppnode: &PreprocessorNode, cond: &AstNode) {
  match cond.walk_boolean_litexpr() {
    LiteralBool::False => state.begin_section(ppnode.clone(), false),
    LiteralBool::True => state.begin_section(ppnode.clone(), true),
    LiteralBool::NotABoolean => {
      let msg =
        "-if() or elif() condition does not evaluate to a compile-time boolean.".to_string();
      state.module.add_error(ErlError::preprocessor_error(
        SourceLoc::unimplemented(file!(), function_name!()),
        msg,
      ));
    }
  }
}

fn on_if_def(state: &mut PreprocessState, ppnode: &PreprocessorNode, macro_name: &str) {
  let is_def = state.module.root_scope.is_defined(macro_name);
  state.begin_section(ppnode.clone(), is_def);
}

fn on_if_not_def(state: &mut PreprocessState, ppnode: &PreprocessorNode, macro_name: &str) {
  let is_not_def = !state.module.root_scope.is_defined(macro_name);
  state.begin_section(ppnode.clone(), is_not_def);
}

#[named]
fn on_else(state: &mut PreprocessState) {
  if let Some(section) = state.section.last_mut() {
    if section.else_encountered {
      // Can only encounter -else once, otherwise an error is raised
      let msg = "-else() encountered after another -else().".to_string();
      state.module.add_error(ErlError::preprocessor_error(
        SourceLoc::unimplemented(file!(), function_name!()),
        msg,
      ));
    } else {
      section.else_encountered = true;
      section.condition = !section.condition;
    }
  } else {
    let msg =
      "-else() encountered without a matching -if(), ifdef(), -ifndef() or -elif().".to_string();
    state.module.add_error(ErlError::preprocessor_error(
      SourceLoc::unimplemented(file!(), function_name!()),
      msg,
    ));
  }
}

#[named]
fn on_endif(state: &mut PreprocessState) {
  if let Some(_section) = state.section.pop() {
    // all good
  } else {
    let msg =
      "-endif() encountered without a matching -if, ifdef, -ifndef, -elif or -else.".to_string();
    state.module.add_error(ErlError::preprocessor_error(
      SourceLoc::unimplemented(file!(), function_name!()),
      msg,
    ));
  }
}

/// Pop last section; Invert the condition in it and push back
#[named]
fn on_else_if(state: &mut PreprocessState, cond: &AstNode) {
  if let Some(section) = state.section.pop() {
    // Open a new -IF section
    on_if(state, &section.ppnode, cond);
  } else {
    let msg = "-elif() encountered without a matching -if, ifdef, -ifndef or -elif.".to_string();
    state.module.add_error(ErlError::preprocessor_error(
      SourceLoc::unimplemented(file!(), function_name!()),
      msg,
    ));
  }
}

fn preprocess_handle_ppnode(ppnode: PreprocessorNode, state: &mut PreprocessState) {
  let active = state.is_section_condition_true();

  match &ppnode.content {
    //------------------
    // Set module name (can be done only once)
    //------------------
    PreprocessorNodeType::ModuleName { name } if active => {
      ErlModuleImpl::set_name(state.module, name.as_str())
    }

    //------------------
    // Inclusion
    //------------------
    PreprocessorNodeType::Include(_) if active => unimplemented!(),
    PreprocessorNodeType::IncludeLib(_) if active => unimplemented!(),
    PreprocessorNodeType::IncludedFile { .. } if active => unimplemented!(),

    //------------------
    // Failure on demand
    //------------------
    PreprocessorNodeType::Error(_) if active => unimplemented!(),
    PreprocessorNodeType::Warning(_) if active => unimplemented!(),

    //------------------
    // Populate module scope with stuff
    //------------------
    PreprocessorNodeType::Attr { tag, term } if active => on_attr(state, tag.as_str(), term),
    PreprocessorNodeType::Export { fun_arities } if active => on_export(state, fun_arities),
    PreprocessorNodeType::ExportType { type_arities } if active => {
      on_export_type(state, type_arities)
    }
    PreprocessorNodeType::Import { module: module_name, fun_arities } if active => {
      on_import(state, module_name.as_str(), fun_arities)
    }
    PreprocessorNodeType::NewType { name, vars, ty } if active => {
      on_new_type(state, name.as_str(), vars, ty.clone())
    }
    PreprocessorNodeType::NewRecord { tag, fields } if active => on_new_record(state, tag, fields),
    PreprocessorNodeType::FnSpec { funarity, spec } if active => on_fn_spec(state, funarity, spec),

    //------------------
    // Macro define and undefine
    //------------------
    PreprocessorNodeType::Define { name, args, body } if active => {
      on_define(state, name.as_str(), args, body)
    }
    PreprocessorNodeType::Undef(name) if active => on_undef(state, name),

    //------------------
    // Conditionals
    //------------------
    PreprocessorNodeType::If { cond } if active => on_if(state, &ppnode, cond),
    PreprocessorNodeType::Ifdef { macro_name } if active => on_if_def(state, &ppnode, macro_name),
    PreprocessorNodeType::Ifndef { macro_name } if active => {
      on_if_not_def(state, &ppnode, macro_name)
    }

    // Elseif, Else and Endif do not check the `active` condition, and are always processed
    PreprocessorNodeType::ElseIf { cond } => on_else_if(state, cond),
    PreprocessorNodeType::Else => on_else(state),
    PreprocessorNodeType::Endif => on_endif(state),
    _ => {
      // println!("Section is not active for: {}", ppnode);
    }
  }
}

#[inline]
fn line_begins_with_preprocessor_or_attr(line: &[Token]) -> bool {
  // Begins with a -
  // Followed by an atom, or an "else" keyword, because -else() tokenizes as a keyword
  line.len() > 2
    && line[0].is_tok(TokenType::Minus)
    && (line[1].is_atom() || line[1].is_keyword(Keyword::Else) || line[1].is_keyword(Keyword::If))
}

fn has_any_macro_invocations(line: &[Token]) -> bool {
  line.iter().any(|t| t.is_macro_invocation())
}

/// Given an input line of tokens, replace macro invocations with their actual body content.
/// Also substitute the macro variables.
fn substitute_macro_invocations<'a>(
  _original_input: &str,
  tokens: &'a [Token],
  _state: &mut PreprocessState<'a>,
) -> TokenStream<'a> {
  if !has_any_macro_invocations(tokens) {
    // no changes, no macro invocations
    return TokenStream::new_borrowed(tokens);
  }

  let substituted = Vec::with_capacity(tokens.len());

  for t in tokens.iter() {
    if let TokenType::MacroInvocation(_macro_name) = &t.content {
      // let (tail, ppnode) = panicking_parser_error_reporter(
      //   original_input,
      //   parser_input.clone(),
      //   parse_macro_invocation_args(parser_input).finish(),
      // );
      // let trim_tail = tail.tokens.iter().filter(|t| !t.is_eol()).count();
      // assert_eq!(
      //   trim_tail,
      //   0,
      //   "Not all input consumed while parsing a preprocessor directive or a module attribute:\n{}",
      //   format_tok_stream(tail.tokens, 100)
      // );
      //
      // let macro_body = state.macros.get(macro_name).unwrap();
      // let macro_body = substitute_macro_invocations(macro_body, state);
      // let macro_body = substitute_macro_variables(macro_body, macro_args, state);
      // substituted.extend_from_slice(macro_body);
    } else {
      // substituted.push(t.clone());
    }
  }

  TokenStream::new_owned(substituted)
}

/// Final checks for whether preprocessing was successful:
/// * Unmatched #if/#endif
#[named]
fn final_state_check(state: &mut PreprocessState) {
  // Check for if/ifdef/else without a matching endif
  if let Some(last_sec) = state.section.last() {
    let msg =
      format!("A preprocessor section does not have a matching -endif: {}", last_sec.ppnode);
    state.module.add_error(ErlError::preprocessor_error(
      SourceLoc::unimplemented(file!(), function_name!()),
      msg,
    ));
  }
}

impl ErlModuleImpl {
  /// Filter through the tokens array and produce a new token array with preprocessor directives
  /// eliminated, files included and macros substituted.
  #[named]
  pub fn preprocess_interpret(
    original_input: &str,
    module: &ErlModule,
    tokens: &[Token],
  ) -> IcResult<Vec<Token>> {
    let mut state = PreprocessState::new(module, tokens);

    while let Some(line) = state.itr.next() {
      if state.too_many_errors {
        break;
      }

      if line_begins_with_preprocessor_or_attr(&line) {
        let line2 = expand_till_directive_end(line, &mut state);
        let line3 = substitute_macro_invocations(original_input, line2, &mut state);
        let (tail, ppnode) = line3.parse_as_preprocessor(original_input);

        // Any non-EOL token in the tail = the input was not consumed
        let tail_non_eol = tail.tokens.iter().any(|t| !t.is_eol());
        if tail_non_eol {
          let msg = format!(
            "Not all input consumed while parsing a preprocessor directive or a module attribute:\n{}",
            format_tok_stream(tail.tokens, 100));
          module.add_error(ErlError::preprocessor_error(
            SourceLoc::unimplemented(file!(), function_name!()),
            msg,
          ));
        }
        preprocess_handle_ppnode(ppnode, &mut state);
      } else {
        if state.is_section_condition_true() {
          // copy the line contents
          let line2 = substitute_macro_invocations(original_input, line, &mut state);
          state.result.extend(line2.as_slice().iter().cloned())
        }
      }
    }

    final_state_check(&mut state);

    if !module.has_errors() {
      println!(
        "Preprocessor: output tokens:\n{}",
        format_tok_stream(&state.result, state.result.len())
      );
    }
    // state.result.iter().for_each(|t| print!("{}", t));
    // println!();

    Ok(state.result)
  }
}

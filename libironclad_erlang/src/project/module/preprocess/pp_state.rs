//! State for preprocessor interpreter.

use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::token::Token;
use crate::erl_syntax::token_stream::token_line_iter::TokenLinesIter;
use crate::project::module::mod_impl::ErlModule;
use crate::project::module::preprocess::pp_section::PreprocessorSection;
use crate::project::ErlProject;

/// Stores the state of preprocessor directives interpretation.
pub(crate) struct PreprocessState<'a> {
  /// Output vector of Tokens, returned as result of interpretation
  pub(crate) result: Vec<Token>,
  /// Flag to stop the preprocessing when too many errors are encountered, so we don't flood the terminal
  pub(crate) too_many_errors: bool,
  /// Module ref with scope
  pub(crate) project: ErlProject,
  /// Module ref with scope
  pub(crate) module: ErlModule,
  /// Stack of encountered -if/ifdef/ifndef and matching else pairs. Endif pops last stack item.
  pub(crate) section: Vec<PreprocessorSection>,
  /// Input iterator of Tokens
  pub(crate) itr: TokenLinesIter<'a>,
}

impl<'a> PreprocessState<'a> {
  /// Create a new state to begin preprocessing
  pub fn new(
    project: &ErlProject,
    module: &ErlModule,
    input_tokens: (*const Token, usize),
  ) -> Self {
    PreprocessState {
      project: project.clone(),
      module: module.clone(),
      result: Vec::with_capacity(input_tokens.1),
      itr: TokenLinesIter::new(input_tokens),
      too_many_errors: false,
      section: Vec::default(),
    }
  }

  /// Insert tokens vector contents at the current preprocessing state. The vector might be
  /// reallocated, so also update the `itr` iterator. This is used to include files.
  pub(crate) fn paste_tokens(&self, paste_into: &mut Vec<Token>, mut tokens: Vec<Token>) {
    tokens.push(Token::new_eol());

    let paste_pos = self.itr.slice_start + self.itr.slice_len;
    // println!(
    //   "Pasting at {} <HERE> {}",
    //   format_tok_stream(&paste_into[paste_pos - 20..paste_pos], 20),
    //   format_tok_stream(&paste_into[paste_pos..paste_pos + 20], 20)
    // );
    paste_into.splice(paste_pos..paste_pos, tokens.iter().cloned());
  }

  /// Pushes a new section to the stack, when a condition is encountered.
  pub fn begin_section(&mut self, ppnode: PreprocessorNode, condition: bool) {
    self
      .section
      .push(PreprocessorSection::new(ppnode, condition));
  }

  /// Return true if last section `condition` is true, allowing us to paste tokens into the output,
  /// and to interpret further directives in this section.
  pub fn is_section_condition_true(&self) -> bool {
    if let Some(section) = self.section.last() {
      section.condition
    } else {
      // No if/ifdef/elif/else section, paste is allowed
      true
    }
  }
}

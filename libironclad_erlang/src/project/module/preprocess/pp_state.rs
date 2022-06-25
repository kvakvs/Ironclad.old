//! State for preprocessor interpreter.

use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::token::Token;
use crate::erl_syntax::token_stream::token_line_iter::TokenLinesIter;
use crate::project::module::mod_impl::ErlModule;
use crate::project::module::preprocess::pp_section::PreprocessorSection;
use crate::source_loc::SourceLoc;
use ::function_name::named;

/// Stores the state of preprocessor directives interpretation.
pub(crate) struct PreprocessState<'a> {
  /// Output vector of Tokens, returned as result of interpretation
  pub(crate) result: Vec<Token>,
  /// Input vector of Tokens (comes from the tokenizer)
  pub(crate) itr: TokenLinesIter<'a>,
  /// Flag to stop the preprocessing when too many errors are encountered, so we don't flood the terminal
  pub(crate) too_many_errors: bool,
  /// Module ref with scope
  pub(crate) module: &'a ErlModule,
  /// Stack of encountered -if/ifdef/ifndef and matching else pairs. Endif pops last stack item.
  pub(crate) section: Vec<PreprocessorSection>,
}

impl<'a> PreprocessState<'a> {
  /// Create a new state to begin preprocessing
  pub fn new(module: &'a ErlModule, tokens: &'a [Token]) -> Self {
    PreprocessState {
      result: Vec::with_capacity(tokens.len()),
      itr: TokenLinesIter::new(tokens),
      too_many_errors: false,
      module,
      section: Vec::default(),
    }
  }

  /// Pushes a new section to the stack, when a condition is encountered.
  pub fn begin_section(&mut self, ppnode: PreprocessorNode, condition: bool) {
    self
      .section
      .push(PreprocessorSection::new(ppnode, condition));
  }

  /// Modify last section to opposite `condition`, produce errors if the condition is already true
  /// or if double else is encountered.
  #[named]
  pub fn else_section(&mut self) {
    if let Some(section) = self.section.last_mut() {
      if section.else_encountered {
        // Can only encounter -else once, otherwise an error is raised
        let msg = "-else() encountered after another -else().".to_string();
        self.module.add_error(ErlError::preprocessor_error(
          SourceLoc::unimplemented(file!(), function_name!()),
          msg,
        ));
      } else {
        section.else_encountered = true;
      }
    } else {
      let msg =
        "-else() encountered without a matching -if(), ifdef(), -ifndef() or -elif().".to_string();
      self.module.add_error(ErlError::preprocessor_error(
        SourceLoc::unimplemented(file!(), function_name!()),
        msg,
      ));
    }
  }
}

use std::path::{PathBuf};
use crate::erl_parse::{Span};
use crate::erl_error::{ErlResult, ErlError};
use crate::project::source_file::SourceFile;

/// -include directive, parsed to contain one quoted string
pub struct PpInclude {
  pub file_name: PathBuf,
}

impl PpInclude {
  pub fn parse_from(_source_file: &SourceFile, _input_opt: &Option<Span>) -> ErlResult<Self> {
    //   if input_opt.is_none() {
    //     return ErlError::pp_parse(&source_file.file_name,
    //                               "-include() directive takes 1 string argument");
    //   }
    //
    //   let input = input_opt.as_ref().unwrap().text(source_file);
    //
    //   let (tail, string) = helpers::parse_string(&input)?;
    //   if !tail.is_empty() {
    //     return helpers::incomplete_parse_error(&source_file.file_name, &input, tail);
    //   }
    //   Ok(Self {
    //     file_name: PathBuf::from(string)
    //   })
    ErlError::not_impl("PpInclude:parse_from")
  }
}

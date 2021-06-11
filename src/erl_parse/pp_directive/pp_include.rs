use std::path::{PathBuf, Path};
use crate::erl_parse::helpers;
use crate::erl_error::ErlResult;

/// -include directive, parsed to contain one quoted string
pub struct PpInclude {
  pub file_name: PathBuf,
}

impl PpInclude {
  pub fn parse_from(file_name: &Path, input_opt: &Option<String>) -> ErlResult<Self> {
    if input_opt.is_none() {
      return Ok(Self { file_name: Default::default() });
    }

    let input = input_opt.as_ref().unwrap();

    let (tail, string) = helpers::parse_string(&input)?;
    if !tail.is_empty() {
      return helpers::incomplete_parse_error(file_name, &input, tail);
    }
    Ok(Self {
      file_name: PathBuf::from(string)
    })
  }
}

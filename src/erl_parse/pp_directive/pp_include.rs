use std::path::{PathBuf, Path};
use crate::erl_parse::helpers;
use crate::erl_error::{ErlResult, ErlError, };

/// -include directive, parsed to contain one quoted string
pub struct PpInclude {
  pub file_name: PathBuf,
}

impl PpInclude {
  pub fn parse_from(file_name: &Path, input_opt: &Option<&str>) -> ErlResult<Self> {
    if input_opt.is_none() {
      return ErlError::pp_parse(file_name, "-include() directive takes 1 string argument");
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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::erl_error::ErlError;

  fn parse_include_(path: &str, input: Option<String>) -> ErlResult<PpInclude> {
    let str_path = PathBuf::from(path);
    PpInclude::parse_from(&str_path, &input)
  }

  #[test]
  /// Try parse INNER content of -include(<this content>).
  /// Expected: Produces a [PpInclude] struct with correct field values.
  fn parse_include() {
    // Empty directive -include(). will produce an empty path which should be an error.
    // Check that error is ErlError::PpParse, fail if OK or other error than ErlError::PpParse
    let dir1 = parse_include_("<string>", None);
    match dir1 {
      Ok(_) => assert!(false, "Empty input is invalid for -include(...)"),
      Err(ErlError::PpParse(_, _)) => {},
      other => { other.unwrap(); }
    }

    let dir2 = parse_include_("<string>", Some(String::from("\"hello.hrl\"")))
        .unwrap();
    assert_eq!(format!("{}", dir2.file_name.display()), "hello.hrl");
  }
}
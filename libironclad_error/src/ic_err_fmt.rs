//! Printing for `IroncladError`s

use crate::ic_error::IroncladError;
use crate::ic_error_category::IcErrorCategory;
use crate::ic_error_trait::IcErrorT;

impl std::fmt::Debug for IroncladError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self)
  }
}

impl std::fmt::Display for IroncladError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "{}:", self.get_category().to_string())?;

    match self.get_category() {
      // IcErrorType::Interrupted(s) => write!(f, "Processing interrupted: {}", s),
      IcErrorCategory::Multiple(errs) => {
        for err in errs.iter() {
          writeln!(f, "{}", err)?;
        }
        Ok(())
      }
      IcErrorCategory::FileNotFound { file, while_verb } => {
        writeln!(f, "file: {} while {}", file.to_string_lossy(), while_verb)
      }
      IcErrorCategory::Io(ioerr) => writeln!(f, "{}", ioerr),
      IcErrorCategory::Glob(gerr) => write!(f, "{}", gerr),
      IcErrorCategory::GlobPattern(gperr) => write!(f, "{}", gperr),
      IcErrorCategory::Config(cfgerr) => write!(f, "{}", cfgerr),
      IcErrorCategory::Preprocessor
      | IcErrorCategory::PreprocessorParse
      | IcErrorCategory::ParserInternal
      | IcErrorCategory::Internal
      | IcErrorCategory::TypeError
      | IcErrorCategory::ErlangParse => {
        write!(f, "{} (at {})", self.get_message(), self.get_location())
      }
      _ => unimplemented!("Format is not impl for {:?}", self.get_category()),
    }
  }
}

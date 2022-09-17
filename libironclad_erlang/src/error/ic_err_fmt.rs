//! Printing for `IroncladError`s

use crate::error::ic_error::IroncladError;
use crate::error::ic_error_kind::IcErrorKind;
use crate::error::ic_error_trait::IcErrorTrait;

impl std::fmt::Debug for IroncladError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "IroncladError[{}]", self)
  }
}

impl std::fmt::Display for IroncladError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    writeln!(f, "{}:", self.get_severity())?;

    match &self.kind {
      // IcErrorType::Interrupted(s) => write!(f, "Processing interrupted: {}", s),
      IcErrorKind::Multiple(errs) => {
        for err in errs.iter() {
          writeln!(f, "{}", err)?;
        }
        Ok(())
      }
      IcErrorKind::FileNotFound { file, while_verb } => {
        writeln!(f, "file: {} while {}", file.to_string_lossy(), while_verb)
      }
      IcErrorKind::StdIoError(ioerr) => writeln!(f, "{}", ioerr),
      IcErrorKind::Glob(gerr) => gerr.fmt(f),
      IcErrorKind::GlobPattern(gperr) => gperr.fmt(f),
      IcErrorKind::Config(cfgerr) => cfgerr.fmt(f),
      IcErrorKind::Internal | IcErrorKind::TypeErr { .. } => {
        write!(f, "{} (at {})", self.get_message(), self.get_location())
      }
      _ => unimplemented!("Format is not impl for {:?}", self.kind),
    }
  }
}

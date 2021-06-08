use thiserror::Error;
use std::io::Error;

#[derive(Error, Debug)]
pub enum ErlError {
  #[error("File IO error: {0:?}")]
  IoError(std::io::Error),

  // Project errors produced when glob() scanning input files and directories
  #[error("Glob directory scan error: {0:?}")]
  GlobError(glob::GlobError),
  #[error("Glob pattern error: {0:?}")]
  GlobPatternError(glob::PatternError),

  // Project loading error produced when loading TOML
  #[error("Configuration file syntax error: {0:?}")]
  ConfigError(toml::de::Error),

  #[error("Compiler internal data locking error")]
  LockingPoisonError,
}

pub type ErlResult<T> = Result<T, ErlError>;

impl From<std::io::Error> for ErlError {
  fn from(value: std::io::Error) -> Self {
    ErlError::IoError(value)
  }
}

impl From<toml::de::Error> for ErlError {
  fn from(value: toml::de::Error) -> Self {
    ErlError::ConfigError(value)
  }
}

impl From<glob::GlobError> for ErlError {
  fn from(value: glob::GlobError) -> Self {
    ErlError::GlobError(value)
  }
}

impl From<glob::PatternError> for ErlError {
  fn from(value: glob::PatternError) -> Self {
    ErlError::GlobPatternError(value)
  }
}
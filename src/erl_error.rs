use std::path::{Path, PathBuf};

use thiserror::Error;

use crate::syntaxtree::pp::pp_parser;
use crate::syntaxtree::erl::erl_parser;
use crate::typing::error::TypeError;
use std::num::ParseIntError;

#[derive(Debug)]
pub enum ErrorLocation {
  None,
  SourceFile(PathBuf),
}

impl ErrorLocation {
  pub fn from_source_file(p: &Path) -> ErrorLocation {
    Self::SourceFile(p.to_path_buf())
  }
}

#[derive(Error, Debug)]
pub enum ErlError {
  #[error("Multiple errors: {0:?}")]
  Multiple(Vec<ErlError>),

  #[error("Not implemented: {explanation}")]
  NotImpl { explanation: String },

  #[error("File IO error: {0:?}")]
  Io(std::io::Error),

  // Project errors produced when glob() scanning input files and directories
  #[error("Directory scan error: {0:?}")]
  Glob(glob::GlobError),

  #[error("Glob pattern error: {0:?}")]
  GlobPattern(glob::PatternError),

  // Project loading error produced when loading TOML
  #[error("Configuration file syntax error: {0:?}")]
  Config(toml::de::Error),

  // Lock poisoning happens when a Write-lock caught a panic
  // #[error("Compiler internal data locking error")]
  // LockingPoisonError,

  #[error("Preprocessor parse error: {msg} (at {loc:?})")]
  PreprocessorParse { loc: ErrorLocation, msg: String },

  #[error("Preprocessor syntax parse error: {parse_err:?}")]
  PreprocessorSyntax { parse_err: pest::error::Error<pp_parser::Rule> },

  #[error("Erlang parse error: {msg} (at {loc:?})")]
  ErlangParse { loc: ErrorLocation, msg: String },

  #[error("Erlang syntax parse error: {parse_err:?}")]
  ErlangSyntax { parse_err: pest::error::Error<erl_parser::Rule> },

  #[error("Type error: {0:?}")]
  TypeError(TypeError),
}

impl ErlError {
  pub(crate) fn not_impl<T>(what: &str) -> ErlResult<T> {
    Err(ErlError::NotImpl { explanation: what.to_string() })
  }

  pub fn pp_parse<T>(file_name: &Path, message: &str) -> ErlResult<T> {
    Err(ErlError::PreprocessorParse {
      loc: ErrorLocation::from_source_file(file_name),
      msg: String::from(message),
    })
  }
}

pub type ErlResult<T> = Result<T, ErlError>;

impl From<std::io::Error> for ErlError {
  fn from(value: std::io::Error) -> Self {
    ErlError::Io(value)
  }
}

impl From<toml::de::Error> for ErlError {
  fn from(value: toml::de::Error) -> Self {
    ErlError::Config(value)
  }
}

impl From<glob::GlobError> for ErlError {
  fn from(value: glob::GlobError) -> Self {
    ErlError::Glob(value)
  }
}

impl From<glob::PatternError> for ErlError {
  fn from(value: glob::PatternError) -> Self {
    ErlError::GlobPattern(value)
  }
}

impl From<pest::error::Error<pp_parser::Rule>> for ErlError {
  fn from(value: pest::error::Error<pp_parser::Rule>) -> Self {
    ErlError::PreprocessorSyntax { parse_err: value }
  }
}

impl From<pest::error::Error<erl_parser::Rule>> for ErlError {
  fn from(value: pest::error::Error<erl_parser::Rule>) -> Self {
    ErlError::ErlangSyntax { parse_err: value }
  }
}

impl From<TypeError> for ErlError {
  fn from(value: TypeError) -> Self {
    ErlError::TypeError(value)
  }
}

impl From<ParseIntError> for ErlError {
  fn from(pie: ParseIntError) -> Self {
    ErlError::ErlangParse {
      loc: ErrorLocation::None,
      msg: format!("Cannot parse integer: {}", pie.to_string()),
    }
  }
}
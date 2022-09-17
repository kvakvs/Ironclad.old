//! Projcet configuration: inputs, etc
use crate::error::ic_error::{IroncladError, IroncladResult};
use crate::project::conf::serializable_compiler_opts::SerializableCompilerOpts;
use crate::project::conf::serializable_input_opts::SerializableInputOpts;
use core::fmt;
use core::fmt::Debug;
use core::option::Option;
use core::result::Result;
use serde_derive::Deserialize;
use std::convert::Into;
use std::fs;

pub mod serializable_compiler_opts;
pub mod serializable_input_opts;

/// Defines configuration file as it is loaded by TOML and serde
#[derive(Deserialize)]
pub struct ProjectConf {
  /// Input search paths, output paths, flags, ... etc
  pub compiler_options: Option<SerializableCompilerOpts>,

  /// Input files and directories (wildcards are allowed)
  pub inputs: Option<SerializableInputOpts>,
}

impl ProjectConf {
  /// Creates project struct from a TOML filename
  pub fn from_project_file(filename: &str) -> IroncladResult<Self> {
    println!("Reading: {}", filename);
    let config_str = fs::read_to_string(filename).map_err(IroncladError::from)?;

    // Parse, and convert toml error into ErlError
    toml::from_str(&config_str).map_err(|e| IroncladError::from(e).into())
  }

  /// Creates project struct from a TOML config as a string
  pub fn from_string(input: &str) -> Result<Self, IroncladError> {
    // Parse, and convert toml error into ErlError
    toml::from_str(input).map_err(|e| e.into())
  }
}

impl Debug for ProjectConf {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "ProjectConf({:?}, {:?})", self.inputs, self.compiler_options)
  }
}

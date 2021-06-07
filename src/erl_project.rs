use serde_derive::Deserialize;
use std::fmt::Debug;
use std::{fmt, fs};
use crate::compiler_opts::CompilerOpts;
use crate::source_tree::SourceTree;
use crate::erl_error::ErlError;
use nom::Parser;

// Erlang Program consists of
// - Module list
// - Compiler options (input search paths, output paths, flags)
#[derive(Deserialize)]
pub struct ErlProject {
    compiler_opts: CompilerOpts,
    inputs: SourceTree,
}

impl ErlProject {
    pub fn from_project_file(file: &str) -> Result<Self, ErlError> {
        let config_str = fs::read_to_string(file)?;

        // Parse, and convert toml error into ErlError
        toml::from_str(&config_str).map_err(|e| e.into())
    }
}


impl Debug for ErlProject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ErlProject({:?}, {:?})", self.inputs, self.compiler_opts)
    }
}

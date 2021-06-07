use std::{fmt, fs};
use std::fmt::Debug;

use nom::Parser;
use serde_derive::Deserialize;

use crate::erl_error::ErlError;
use std::sync::Arc;
use crate::project::compiler_opts::CompilerOpts;
use crate::project::input_opts::InputOpts;
use crate::project::conf::ErlProjectConf;

pub(crate) mod conf;
pub(crate) mod compiler_opts;
pub(crate) mod input_opts;

/// Same as ErlProjectConf but no Option<> fields
#[derive(Debug)]
pub struct ErlProject {
    /// Input search paths, output paths, flags, ... etc. Shared with all modules which use default
    /// compile options
    compiler_opts: Arc<CompilerOpts>,

    /// Input files and directories (wildcards are allowed)
    inputs: InputOpts,
}

impl From<ErlProjectConf> for ErlProject {
    fn from(conf: ErlProjectConf) -> Self {
        Self {
            compiler_opts: Arc::new(CompilerOpts::from(conf.compiler_opts)),
            inputs: InputOpts::from(conf.inputs),
        }
    }
}
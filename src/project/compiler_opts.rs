use crate::project::conf::compiler_opts::CompilerOptsConf;

#[derive(Debug)]
pub struct CompilerOpts {
    /// If not specified, defaults to empty
    include_paths: Vec<String>,
}

impl Default for CompilerOpts {
    fn default() -> Self {
        Self { include_paths: vec![], }
    }
}

impl From<CompilerOptsConf> for CompilerOpts {
    fn from(opts: CompilerOptsConf) -> Self {
        let self_default = Self::default();
        Self { include_paths: opts.include_paths.unwrap_or(self_default.include_paths) }
    }
}

impl From<Option<CompilerOptsConf>> for CompilerOpts {
    fn from(maybe_opts: Option<CompilerOptsConf>) -> Self {
        match maybe_opts {
            None => Self::default(),
            Some(conf_val) => Self::from(conf_val)
        }
    }
}

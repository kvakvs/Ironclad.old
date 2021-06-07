use thiserror::Error;
use std::io::Error;

#[derive(Error, Debug)]
pub enum ErlError {
    #[error("File IO error: {0:?}")]
    IoError(std::io::Error),
    #[error("Configuration file syntax error: {0:?}")]
    ConfigError(toml::de::Error),
}

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
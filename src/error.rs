use miette::Diagnostic;
use thiserror::Error;

#[derive(Error, Diagnostic, Debug)]
pub enum Error {
    #[error(transparent)]
    IoError(#[from] std::io::Error),
    #[error(transparent)]
    ZipError(#[from] zip::result::ZipError),
    #[error(transparent)]
    ObjectError(#[from] object::read::Error),
    #[error(transparent)]
    GoblinError(#[from] goblin::error::Error),
    #[error(transparent)]
    GimliError(#[from] gimli::Error),
    #[error("Aerology pack note missing from core dump")]
    PackNoteMissing,
    #[error("Bad section name {0}")]
    BadSectionName(String),
    #[error("Read from an invalid Address {0:x}")]
    InvalidAddress(u32),
}

pub type Result<T> = std::result::Result<T, Error>;

use std::fmt::Debug;

use miette::{Diagnostic, SourceSpan};
use pest::error::Error as PErr;
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
    #[error("member not found {1:?}")]
    #[diagnostic(help("consider replacing with {2} instead"))]
    MemberMissing(#[label("missing")] SourceSpan, String, String),
    #[error("type mismatch")]
    TypeMismatch {
        #[label("expected {expected} found {found}")]
        span: SourceSpan,
        expected: String,
        found: String,
    },
    #[error("Array bounds unknown")]
    UnsizedArray(#[label] SourceSpan),
    #[error("Array index out of bounds")]
    #[diagnostic(help("This array has a size of {1}"))]
    IndexOOB(#[label] SourceSpan, u32),
    #[error("Type has no size")]
    UnsizedType(#[label] SourceSpan),
    #[error("Type missing")]
    TypeMissing(#[label] SourceSpan),
    #[error("Unknwon register")]
    UnknownReg(#[label] SourceSpan),
    #[error("Parse Error")]
    Parse(#[label("{1}")] SourceSpan, String),
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn enumerate<R: Debug>(rules: &[R]) -> String {
    match rules.len() {
        1 => format!("{:?}", &rules[0]),
        l => {
            let separated = rules
                .iter()
                .take(l - 1)
                .map(|r| format!("{:?}", r))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{} or {:?}", separated, &rules[l - 1])
        }
    }
}

impl<R: Debug> From<PErr<R>> for Error {
    fn from(perr: PErr<R>) -> Self {
        let span = match perr.location {
            pest::error::InputLocation::Pos(p) => (p, 1).into(),
            pest::error::InputLocation::Span((start, end)) => (start, end - start).into(),
        };
        let message = match perr.variant {
            pest::error::ErrorVariant::ParsingError {
                positives,
                negatives,
            } => match (negatives.is_empty(), positives.is_empty()) {
                (false, false) => format!(
                    "unexpected {}; expected {}",
                    enumerate(&negatives),
                    enumerate(&positives)
                ),
                (false, true) => format!("unexpected {}", enumerate(&negatives)),
                (true, false) => format!("expected {}", enumerate(&positives)),
                (true, true) => "unknown parsing error".to_owned(),
            },
            pest::error::ErrorVariant::CustomError { message } => message,
        };
        Self::Parse(span, message)
    }
}

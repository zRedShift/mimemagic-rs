use quick_xml::Error as XmlError;
use std::{
    error::Error as StdError,
    fmt::{Display, Formatter, Result as FmtResult},
    io::Error as IoError,
    num::ParseIntError,
    str::ParseBoolError,
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
    Xml(XmlError),
    Io(IoError),
    MissingElem { top: &'static str, sub: &'static str },
    MissingAttr { attr: &'static str, elem: &'static str },
    InvalidType(String),
    InvalidOffset,
    InvalidValue,
    InvalidMatchType,
    Int(ParseIntError),
    Bool(ParseBoolError),
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use Error::*;
        match *self {
            Xml(ref e) => Display::fmt(e, f),
            Io(ref e) => write!(f, "I/O error: {}", e),
            MissingElem { top, sub } => write!(f, "Missing element {} in {}", sub, top),
            MissingAttr { attr, elem } => write!(f, "Missing attribute {} in {}", attr, elem),
            InvalidType(ref s) => write!(f, "Invalid MIME type: {}", s),
            InvalidOffset => write!(f, "Invalid offset"),
            InvalidValue => write!(f, "Invalid value or mask"),
            InvalidMatchType => write!(f, "Invalid match type"),
            Int(ref e) => write!(f, "Int parsing error: {}", e),
            Bool(ref e) => write!(f, "Bool parsing error: {}", e),
        }
    }
}

impl From<IoError> for Error {
    #[inline]
    fn from(error: IoError) -> Self {
        Error::Io(error)
    }
}

impl From<XmlError> for Error {
    #[inline]
    fn from(error: XmlError) -> Self {
        Error::Xml(error)
    }
}

impl From<ParseIntError> for Error {
    #[inline]
    fn from(error: ParseIntError) -> Self {
        Error::Int(error)
    }
}

impl From<ParseBoolError> for Error {
    #[inline]
    fn from(error: ParseBoolError) -> Self {
        Error::Bool(error)
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self {
            Error::Io(e) => Some(e),
            Error::Xml(e) => Some(e),
            Error::Int(e) => Some(e),
            Error::Bool(e) => Some(e),
            _ => None,
        }
    }
}

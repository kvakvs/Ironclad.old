//! Support for binary type, as collection of binary elements

/// First element of a binary type
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryTypeHeadElement(pub usize);

/// Second optional element of a binary type
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryTypeTailElement(pub usize);

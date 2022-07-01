//! Support for binary type, as collection of binary elements

/// An element of Erlang binary type: `<<..., >>`
/// `binary()`             | `<<_:_*8>>`
/// `nonempty_binary()`    | `<<_:8, _:_*8>>`
/// `bitstring()`          | `<<_:_*1>>`
/// `nonempty_bitstring()` | `<<_:1, _:_*1>>`
///
/// The general form of bit strings is `<<_:M, _:_*N>>`, where `M` and `N` must evaluate to
/// positive integers. It denotes a bit string that is `M + (k*N)` bits long (that is, a bit
/// string that starts with `M` bits and continues with `k` segments of `N` bits each,
/// where `k` is also a positive integer). The notations `<<_:_*N>>`, `<<_:M>>`, and `<<>>` are
/// convenient shorthands for the cases that `M` or `N`, or both, are zero.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryTypeHeadElement(pub usize);

/// Second optional element of a binary type
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BinaryTypeTailElement(pub usize);

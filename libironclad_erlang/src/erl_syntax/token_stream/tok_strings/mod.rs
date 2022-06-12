//! Directory will group the tokenizer code for `"string"` and `'atom'` with support for backslash
//! quoting and unicode.
pub mod atom_literal;
pub mod shared;
pub mod str_literal;

/// Replaces `char` where it overlaps with `nom::*::char`
pub type Char = char;

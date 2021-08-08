//! Display helpers for printing stuff

use std::fmt;

/// Print <comma separated list of something>
pub fn display_comma_separated<T>(elems: &[T], f: &mut fmt::Formatter) -> fmt::Result
  where T: std::fmt::Display {
  let mut first = true;
  for entry in elems.iter() {
    if first { first = false; } else { write!(f, ", ")?; }
    write!(f, "{}", entry)?;
  }
  Ok(())
}

/// Print \[ <comma separated something> \]
pub fn display_list<T>(elems: &[T], f: &mut fmt::Formatter) -> fmt::Result
  where T: std::fmt::Display {
  write!(f, "[")?;
  display_comma_separated(elems, f)?;
  write!(f, "]")
}

/// Print { <comma separated something> }
pub fn display_tuple<T>(elems: &[T], f: &mut fmt::Formatter) -> fmt::Result
  where T: std::fmt::Display {
  write!(f, "{{")?;
  display_comma_separated(elems, f)?;
  write!(f, "}}")
}

//! Display helpers for printing stuff

use std::fmt;

/// Groups up the code for pretty printing containers and other stuff
pub struct Pretty {}

impl Pretty {
  /// Print <'sep' separated list of something>
  pub fn display_separated<T>(elems: &[T], sep: &str, f: &mut fmt::Formatter) -> fmt::Result
    where T: std::fmt::Display {
    let mut first = true;
    for entry in elems.iter() {
      if first { first = false; } else { write!(f, "{}", sep)?; }
      write!(f, "{}", entry)?;
    }
    Ok(())
  }

  /// Print <comma, separated, list, of something>
  pub fn display_comma_separated<T>(elems: &[T], f: &mut fmt::Formatter) -> fmt::Result
    where T: std::fmt::Display {
    Pretty::display_separated(elems, ", ", f)
  }

  /// Print <semicolon; separated; list; of something>
  pub fn display_semicolon_separated<T>(elems: &[T], f: &mut fmt::Formatter) -> fmt::Result
    where T: std::fmt::Display {
    Pretty::display_separated(elems, "; ", f)
  }

  /// Print \[ <comma separated something> \]
  pub fn display_square_list<T>(elems: &[T], f: &mut fmt::Formatter) -> fmt::Result
    where T: std::fmt::Display {
    write!(f, "[")?;
    Pretty::display_comma_separated(elems, f)?;
    write!(f, "]")
  }

  /// Print ( <comma separated something> )
  pub fn display_paren_list<T>(elems: &[T], f: &mut fmt::Formatter) -> fmt::Result
    where T: std::fmt::Display {
    write!(f, "(")?;
    Pretty::display_comma_separated(elems, f)?;
    write!(f, ")")
  }

  /// Print { <comma separated something> }
  pub fn display_curly_list<T>(elems: &[T], f: &mut fmt::Formatter) -> fmt::Result
    where T: std::fmt::Display {
    write!(f, "{{")?;
    Pretty::display_comma_separated(elems, f)?;
    write!(f, "}}")
  }

}

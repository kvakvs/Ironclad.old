//! Display helpers for printing stuff

use std::fmt::{Display, Formatter};

/// Groups up the code for pretty printing containers and other stuff
pub struct Pretty {}

impl Pretty {
  /// Print <'sep' separated list of something>
  pub fn display_separated<'a, Iter, T>(
    elems_iter: Iter,
    sep: &str,
    f: &mut Formatter,
  ) -> std::fmt::Result
  where
    Iter: Iterator<Item = &'a T>,
    T: Display + 'a,
  {
    let mut first = true;
    for entry in elems_iter {
      if first {
        first = false;
      } else {
        sep.fmt(f).unwrap();
      }
      entry.fmt(f).unwrap();
    }
    Ok(())
  }

  /// Print <comma, separated, list, of something>
  pub fn display_comma_separated<'a, Iter, T>(
    elems_iter: Iter,
    f: &mut Formatter,
  ) -> std::fmt::Result
  where
    Iter: Iterator<Item = &'a T>,
    T: Display + 'a,
  {
    Pretty::display_separated(elems_iter, ", ", f)
  }

  /// Print <semicolon; separated; list; of something>
  pub fn display_semicolon_separated<'a, Iter, T>(
    elems_iter: Iter,
    f: &mut Formatter,
  ) -> std::fmt::Result
  where
    Iter: Iterator<Item = &'a T>,
    T: Display + 'a,
  {
    Pretty::display_separated(elems_iter, "; ", f)
  }

  /// Print \[ <comma separated something> \]
  pub fn display_square_list<'a, Iter, T>(elems_iter: Iter, f: &mut Formatter) -> std::fmt::Result
  where
    Iter: Iterator<Item = &'a T>,
    T: Display + 'a,
  {
    write!(f, "[").unwrap();
    Pretty::display_comma_separated(elems_iter, f).unwrap();
    write!(f, "]")
  }

  /// Print ( <comma separated something> )
  pub fn display_paren_list<'a, Iter, T>(elems_iter: Iter, f: &mut Formatter) -> std::fmt::Result
  where
    Iter: Iterator<Item = &'a T>,
    T: Display + 'a,
  {
    write!(f, "(").unwrap();
    Pretty::display_comma_separated(elems_iter, f).unwrap();
    write!(f, ")")
  }

  /// Print { <comma separated something> }
  pub fn display_curly_list<'a, Iter, T>(elems_iter: Iter, f: &mut Formatter) -> std::fmt::Result
  where
    Iter: Iterator<Item = &'a T>,
    T: Display + 'a,
  {
    write!(f, "{{").unwrap();
    Pretty::display_comma_separated(elems_iter, f).unwrap();
    write!(f, "}}")
  }

  /// Display a `\" {text} \"` with special characters quoted.
  pub fn doublequot_string(f: &mut Formatter, s: &str) -> std::fmt::Result {
    write!(f, "\"").unwrap();
    for (_i, ch) in s.chars().enumerate() {
      match ch {
        '\"' | '\\' => write!(f, "\\{}", ch)?,
        _ => ch.fmt(f).unwrap(),
      }
    }
    write!(f, "\"")
  }

  /// Display a `\' {text} \'` with special characters quoted.
  pub fn singlequot_string(f: &mut Formatter, s: &str) -> std::fmt::Result {
    write!(f, "\'").unwrap();
    for (_i, ch) in s.chars().enumerate() {
      match ch {
        '\'' | '\\' => write!(f, "\\{}", ch)?,
        _ => ch.fmt(f).unwrap(),
      }
    }
    write!(f, "\'")
  }
}

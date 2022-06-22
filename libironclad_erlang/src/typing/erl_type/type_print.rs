//! Display code for printing types

use crate::typing::erl_type::ErlType;
use libironclad_util::pretty::Pretty;
use std::fmt::Formatter;

impl std::fmt::Display for ErlType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    // write!(f, "{:?}", self)
    match self {
      ErlType::Any => write!(f, "any()"),
      ErlType::None => write!(f, "none()"),
      ErlType::Atom => write!(f, "atom()"),
      ErlType::Boolean => write!(f, "boolean()"),
      ErlType::Number => write!(f, "number()"),
      ErlType::Float => write!(f, "float()"),
      ErlType::Integer => write!(f, "integer()"),
      ErlType::IntegerRange { from, to } => write!(f, "{}..{}", from, to),
      ErlType::AnyTuple => write!(f, "tuple()"),
      ErlType::Tuple { elements } => Pretty::display_curly_list(elements.iter(), f),
      ErlType::Record { tag, fields } => {
        write!(f, "#{}", tag)?;
        Pretty::display_curly_list(fields.iter(), f)
      }
      ErlType::AnyList => write!(f, "list()"),
      ErlType::List { elements, tail } => match tail {
        Some(t) => write!(f, "list({}, {})", elements, t),
        None => write!(f, "list({})", elements),
      },
      ErlType::StronglyTypedList { elements, tail } => match tail {
        Some(t) => {
          write!(f, "strong_list(")?;
          Pretty::display_comma_separated(elements.iter(), f)?;
          write!(f, " | {})", t)
        }
        None => {
          write!(f, "strong_list")?;
          Pretty::display_paren_list(elements.iter(), f)
        }
      },
      ErlType::Nil => write!(f, "[]"),
      ErlType::AnyMap => write!(f, "map()"),
      ErlType::Map { .. } => unimplemented!("Display type for map"),
      ErlType::AnyBinary => write!(f, "binary()"),
      ErlType::Binary { .. } => unimplemented!("Display type for binary"),
      ErlType::AnyFn => write!(f, "function()"),
      ErlType::Fn(fntype) => {
        write!(f, "fun ")?;
        Pretty::display_semicolon_separated(fntype.clauses().iter(), f)
      }
      ErlType::FnRef { fun } => write!(f, "fun {}", fun),
      ErlType::Lambda => write!(f, "function()"),
      ErlType::Pid => write!(f, "pid()"),
      ErlType::Reference => write!(f, "reference()"),
      ErlType::Port => write!(f, "port()"),
      ErlType::RecordRef { tag } => write!(f, "#{}{{}}", tag),
      ErlType::Singleton { val } => write!(f, "{}", val),
      ErlType::Union(u) => Pretty::display_separated(u.types.iter(), "|", f),
      ErlType::UserDefinedType { name, args } => {
        write!(f, "{}", name)?;
        Pretty::display_paren_list(args.iter(), f)
      }
      ErlType::Typevar(tv) => write!(f, "{}", tv),
    }
  }
}

//! Display code for printing types

use crate::typing::erl_type::ErlTypeImpl;
use libironclad_util::pretty::Pretty;
use std::fmt::Formatter;

impl std::fmt::Display for ErlTypeImpl {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    // write!(f, "{:?}", self)
    match self {
      ErlTypeImpl::Any => write!(f, "any()"),
      ErlTypeImpl::None => write!(f, "none()"),
      ErlTypeImpl::Atom => write!(f, "atom()"),
      ErlTypeImpl::Boolean => write!(f, "boolean()"),
      ErlTypeImpl::Number => write!(f, "number()"),
      ErlTypeImpl::Float => write!(f, "float()"),
      ErlTypeImpl::Integer => write!(f, "integer()"),
      ErlTypeImpl::IntegerRange { from, to } => write!(f, "{}..{}", from, to),
      ErlTypeImpl::AnyTuple => write!(f, "tuple()"),
      ErlTypeImpl::Tuple { elements } => Pretty::display_curly_list(elements.iter(), f),
      ErlTypeImpl::Record { tag, fields } => {
        write!(f, "#{}", tag).unwrap();
        Pretty::display_curly_list(fields.iter(), f)
      }
      ErlTypeImpl::AnyList => write!(f, "list()"),
      ErlTypeImpl::List { elements, tail, is_non_empty } => match tail {
        Some(t) => write!(f, "list({}, {})", elements, t),
        None => {
          write!(f, "list({}", elements).unwrap();
          if *is_non_empty {
            write!(f, ", ...").unwrap();
          }
          write!(f, ")")
        }
      },
      ErlTypeImpl::StronglyTypedList { elements, tail } => match tail {
        Some(t) => {
          write!(f, "strong_list(").unwrap();
          Pretty::display_comma_separated(elements.iter(), f).unwrap();
          write!(f, " | {})", t)
        }
        None => {
          write!(f, "strong_list").unwrap();
          Pretty::display_paren_list(elements.iter(), f)
        }
      },
      ErlTypeImpl::Nil => write!(f, "[]"),
      ErlTypeImpl::AnyMap => write!(f, "map()"),
      ErlTypeImpl::Map { members } => {
        write!(f, "#{{").unwrap();
        Pretty::display_comma_separated(members.iter(), f).unwrap();
        write!(f, "}}")
      }
      ErlTypeImpl::AnyBinary => write!(f, "binary()"),
      ErlTypeImpl::Binary { .. } => unimplemented!("Display type for binary"),
      ErlTypeImpl::AnyFn => write!(f, "function()"),
      ErlTypeImpl::Fn(fntype) => {
        write!(f, "fun ").unwrap();
        Pretty::display_semicolon_separated(fntype.clauses().iter(), f)
      }
      ErlTypeImpl::FnRef { fun } => write!(f, "fun {}", fun),
      ErlTypeImpl::Lambda => write!(f, "function()"),
      ErlTypeImpl::Pid => write!(f, "pid()"),
      ErlTypeImpl::Reference => write!(f, "reference()"),
      ErlTypeImpl::Port => write!(f, "port()"),
      ErlTypeImpl::RecordRef { tag } => write!(f, "#{}{{}}", tag),
      ErlTypeImpl::Singleton { val } => val.fmt(f),
      ErlTypeImpl::Union(u) => Pretty::display_separated(u.types.iter(), "|", f),
      ErlTypeImpl::UserDefinedType { name, args } => {
        name.fmt(f).unwrap();
        Pretty::display_paren_list(args.iter(), f)
      }
      ErlTypeImpl::Typevar(tv) => tv.fmt(f),
    }
  }
}

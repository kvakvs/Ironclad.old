//! Display code for printing types

use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::TypeImpl;
use libironclad_util::pretty::Pretty;
use std::fmt::Formatter;

impl std::fmt::Display for TypeImpl {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    if let Some(tv) = &self.typevar {
      write!(f, "{} :: ", tv)?;
    }

    match &self.kind {
      TypeKind::Any => write!(f, "any()"),
      TypeKind::None => write!(f, "none()"),
      TypeKind::Atom => write!(f, "atom()"),
      TypeKind::Boolean => write!(f, "boolean()"),
      TypeKind::Number => write!(f, "number()"),
      TypeKind::Float => write!(f, "float()"),
      TypeKind::Integer => write!(f, "integer()"),
      TypeKind::IntegerRange { from, to } => write!(f, "{}..{}", from, to),
      TypeKind::AnyTuple => write!(f, "tuple()"),
      TypeKind::Tuple { elements } => Pretty::display_curly_list(elements.iter(), f),
      TypeKind::Record { tag, fields } => {
        write!(f, "#{}", tag).unwrap();
        Pretty::display_curly_list(fields.iter(), f)
      }
      TypeKind::AnyList => write!(f, "list()"),
      TypeKind::List { elements, tail, is_non_empty } => match tail {
        Some(t) => write!(f, "list({}, {})", elements, t),
        None => {
          write!(f, "list({}", elements).unwrap();
          if *is_non_empty {
            write!(f, ", ...").unwrap();
          }
          write!(f, ")")
        }
      },
      TypeKind::StronglyTypedList { elements, tail } => match tail {
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
      TypeKind::Nil => write!(f, "[]"),
      TypeKind::AnyMap => write!(f, "map()"),
      TypeKind::Map { members } => {
        write!(f, "#{{").unwrap();
        Pretty::display_comma_separated(members.iter(), f).unwrap();
        write!(f, "}}")
      }
      TypeKind::AnyBinary => write!(f, "binary()"),
      TypeKind::Binary { .. } => unimplemented!("Display type for binary"),
      TypeKind::AnyFn => write!(f, "function()"),
      TypeKind::Fn(fntype) => {
        write!(f, "fun ").unwrap();
        Pretty::display_semicolon_separated(fntype.clauses().iter(), f)
      }
      TypeKind::FnRef { fun } => write!(f, "fun {}", fun),
      TypeKind::Lambda => write!(f, "function()"),
      TypeKind::Pid => write!(f, "pid()"),
      TypeKind::Reference => write!(f, "reference()"),
      TypeKind::Port => write!(f, "port()"),
      TypeKind::RecordRef { tag, pins } => {
        write!(f, "#{}{{", tag).unwrap();
        Pretty::display_comma_separated(pins.iter(), f).unwrap();
        write!(f, "}}")
      }
      TypeKind::Singleton { val } => val.fmt(f),
      TypeKind::Union(u) => Pretty::display_separated(u.types.iter(), "|", f),
      TypeKind::UserDefinedType { name, args } => {
        name.fmt(f).unwrap();
        Pretty::display_paren_list(args.iter(), f)
      }
    }
  }
}

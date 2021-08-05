//! Adds debug printing for Type trees in a somewhat more compact way

use std::fmt::Formatter;

use crate::typing::erl_type::{ErlType};

impl std::fmt::Debug for ErlType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result { write!(f, "{}", self) }
}

impl std::fmt::Display for ErlType {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      ErlType::Union(members) => {
        let mut first = true;
        for m in members.iter() {
          if !first { write!(f, "|")?; } else { first = false; }
          write!(f, "{}", m)?;
        }
        Ok(())
      }
      ErlType::None => write!(f, "none()"),
      ErlType::Any => write!(f, "any()"),
      ErlType::Number => write!(f, "number()"),
      ErlType::AnyInteger => write!(f, "integer()"),
      ErlType::IntegerConst(i) => write!(f, "{}", i),
      ErlType::Float => write!(f, "float()"),
      ErlType::List(ty) => write!(f, "list({})", ty.to_string()),
      ErlType::Tuple(items) => {
        write!(f, "{{")?;
        let mut first = true;
        for i in items.iter() {
          if !first { write!(f, ", ")?; } else { first = false; }
          write!(f, "{}", i)?;
        }
        write!(f, "}}")
      }
      ErlType::Record { tag, fields } => {
        write!(f, "#{}{{", tag)?;
        let mut first = true;
        for field_def in fields.iter() {
          if !first { write!(f, ", ")?; } else { first = false; }
          write!(f, "{}", field_def)?;
        }
        write!(f, "}}")
      }
      ErlType::Map(fields) => {
        write!(f, "#{{")?;
        let mut first = true;
        for elem in fields.iter() {
          if !first { write!(f, ", ")?; } else { first = false; }
          write!(f, "{}", elem)?;
        }
        write!(f, "}}")
      }
      ErlType::AnyAtom => write!(f, "atom()"),
      ErlType::Atom(s) => write!(f, "'{}'", s),

      ErlType::AnyBool => write!(f, "bool()"),
      ErlType::Pid => write!(f, "pid()"),
      ErlType::Reference => write!(f, "reference()"),
      ErlType::Binary => write!(f, "binary()"),
      ErlType::BinaryBits => write!(f, "bits()"),
      ErlType::Literal(lit) => write!(f, "{}", lit),
      ErlType::LocalFunction { name, arity } => write!(f, "fun {}/{}", name, arity),
      ErlType::Function(fun_type) => {
        match &fun_type.name {
          None => write!(f, "fun(")?,
          Some(n) => write!(f, "{}(", n)?,
        }
        let mut first = true;
        for arg in fun_type.arg_types.iter() {
          if !first { write!(f, ", ")?; } else { first = false; }
          write!(f, "{}", arg)?;
        }
        write!(f, ") -> {}", fun_type.ret_type)
      }
      ErlType::TVar(tv) => write!(f, "{}", tv),
      ErlType::String => write!(f, "string()"), // a list of unicode codepoint: list(char())
    }
  }
}

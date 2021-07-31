//! Adds debug printing for Type trees in a somewhat more compact way

use std::fmt;
use crate::typing::erl_type::{ErlType, MapField, RecordField};
use crate::typing::typevar::TypeVar;
use crate::typing::equation::TypeEquation;

impl fmt::Debug for ErlType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ErlType::Union(members) => {
        write!(f, "∪")?;
        f.debug_list().entries(members).finish()
      }
      ErlType::None => write!(f, "∅"),
      ErlType::Any => write!(f, "∀"),
      ErlType::TVar(v) => write!(f, "{}", v.to_string()),
      ErlType::Number => write!(f, "ℝ"),
      ErlType::AnyInteger => write!(f, "ℤ"),
      ErlType::IntegerConst(i) => write!(f, "{}", i),
      ErlType::Float => write!(f, "ℚ"),
      ErlType::List(t) => write!(f, "list({:?})", t),
      ErlType::String => write!(f, "str"),
      ErlType::Tuple(t) => write!(f, "tuple({:?})", t),
      ErlType::Record { tag, fields } => {
        let mut d = f.debug_tuple(&tag);
        fields.iter().for_each(|f| {
          d.field(f);
        });
        d.finish()
      }
      ErlType::Map(fields) => {
        let mut d = f.debug_map();
        fields.iter().for_each(|f| {
          d.entry(&f.key, &f.ty);
        });
        d.finish()
      }
      ErlType::AnyAtom => write!(f, "atom"),
      ErlType::Atom(s) => write!(f, "'{}'", s),
      ErlType::AnyBool => write!(f, "bool"),
      ErlType::Pid => write!(f, "pid"),
      ErlType::Reference => write!(f, "ref"),
      ErlType::BinaryBits => write!(f, "bits"),
      ErlType::Binary => write!(f, "bin"),
      ErlType::Literal(lit) => write!(f, "{:?}", lit),
      ErlType::LocalFunction {name, arity} => write!(f, "fun {}/{}", name, arity),
      ErlType::Function(fun_type) => {
        match &fun_type.name {
          None => write!(f, "fun(")?,
          Some(n) => write!(f, "{}", n)?,
        }

        let mut d = f.debug_tuple("");
        fun_type.arg_types.iter().for_each(|argt| {
          d.field(argt);
        });
        d.finish()?;

        match fun_type.name {
          None => write!(f, " → {:?})", fun_type.ret_type),
          Some(_) => write!(f, " → {:?}", fun_type.ret_type),
        }
      }
    }
  }
}

impl fmt::Debug for RecordField {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}

impl fmt::Debug for MapField {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}

impl fmt::Debug for TypeVar {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{}", self.to_string())
  }
}

impl fmt::Debug for TypeEquation {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{:?} ⊆ {:?}", self.left, self.right)
  }
}
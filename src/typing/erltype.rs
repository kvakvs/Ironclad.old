use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// Defines a name of a type
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TVar(String);

// impl Eq for TVar {
//
// }

/// Defines a type
#[derive(Debug, Clone)]
pub enum Type {
  /// Refers to a type name
  TVar(TVar),
  /// Refers to a const value in the type
  TConst(String),
  /// A lambda-calculus arrow operation `left -> right`, taking one type and outputting one type
  TArr {
    left: Box<Type>,
    right: Box<Type>,
  },
}

impl Type {
  //     normtype (TArr a b) = TArr (normtype a) (normtype b)
  //     normtype (TCon a)   = TCon a
  //     normtype (TVar a)   =
  //       case lookup a ord of
  //         Just x -> TVar x
  //         Nothing -> error "type variable not in signature"
  pub fn normtype(&self, ord: &HashSet<(TVar, String)>) -> Type {
    match self {
      Type::TVar(a) => {
        match ord.into_iter().find(|ord_item| a.0 == ord_item.1) {
          Some(_) => self.clone(),
          None => panic!("TODO: ErlError, type variable is not in signature"),
        }
      }

      Type::TConst(_) => self.clone(),

      Type::TArr { left, right } => {
        Type::TArr {
          left: Box::from(left.normtype(ord)),
          right: Box::from(right.normtype(ord)),
        }
      },
    }
  }

  // fv (TVar a)   = [a]
  // fv (TArr a b) = fv a ++ fv b
  // fv (TCon _)   = []
  fn free_vars(&self) -> Vec<String> {
    match self {
      Type::TVar(a) => vec![a.0],
      Type::TArr { left, right } => {
        let mut left_vars = left.free_vars();
        left_vars.append(&mut right.free_vars());
        left_vars
      }
      Type::TConst(_) => Vec::new(),
    }
  }
}

#[derive(Debug)]
pub enum TypeError {
  UnificationFail(Rc<Type>, Rc<Type>),
  InfiniteType(TVar, Rc<Type>),
  UnboundVariable(String),
  Msg(String), // some unsupported type error reported as string
}

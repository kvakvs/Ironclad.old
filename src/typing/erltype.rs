use std::collections::{HashSet};

/// Defines a name of a type
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TVar(pub String);

/// Defines a type
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
  /// Refers to a type name
  Var(TVar),
  /// Refers to a const value in the type
  Const(String),
  /// A lambda-calculus arrow operation `left -> right`, taking one type and outputting one type
  Arrow {
    in_arg: Box<Type>,
    result: Box<Type>,
  },
}

impl Type {
  pub fn new_var(s: &str) -> Self {
    Type::Var(TVar(s.to_string()))
  }

  //     normtype (TArr a b) = TArr (normtype a) (normtype b)
  //     normtype (TCon a)   = TCon a
  //     normtype (TVar a)   =
  //       case lookup a ord of
  //         Just x -> TVar x
  //         Nothing -> error "type variable not in signature"
  pub fn normtype(&self, ord: &HashSet<(TVar, String)>) -> Type {
    match self {
      Type::Var(a) => {
        match ord.into_iter().find(|ord_item| a.0 == ord_item.1) {
          Some(_) => self.clone(),
          None => panic!("TODO: ErlError, type variable is not in signature"),
        }
      }

      Type::Const(_) => self.clone(),

      Type::Arrow { in_arg: left, result: right } => {
        Type::Arrow {
          in_arg: Box::from(left.normtype(ord)),
          result: Box::from(right.normtype(ord)),
        }
      }
    }
  }

  // fv (TVar a)   = [a]
  // fv (TArr a b) = fv a ++ fv b
  // fv (TCon _)   = []
  fn free_vars(&self) -> Vec<String> {
    match self {
      Type::Var(a) => vec![a.0.clone()],
      Type::Arrow { in_arg: left, result: right } => {
        let mut left_vars = left.free_vars();
        left_vars.append(&mut right.free_vars());
        left_vars
      }
      Type::Const(_) => Vec::new(),
    }
  }
}

#[derive(Debug)]
pub enum TypeError {
  UnificationFail(Box<Type>, Box<Type>),
  InfiniteType(TVar, Box<Type>),
  UnboundVariable(String),
  Msg(String), // some unsupported type error reported as string
}

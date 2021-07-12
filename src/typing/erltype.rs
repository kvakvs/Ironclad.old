use std::collections::{HashSet};
use crate::syntaxtree::erl::erl_op::ErlBinaryOp;

/// Defines a name of a type
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TypeVar(pub String);

lazy_static! {
  static ref BOOL_TYPE: Type = Type::Const("bool".to_string());
  static ref INT_TYPE: Type = Type::Const("integer".to_string());
  static ref FLOAT_TYPE: Type = Type::Const("float".to_string());
  static ref ATOM_TYPE: Type = Type::Const("atom".to_string());
  static ref PID_TYPE: Type = Type::Const("pid".to_string());
  static ref PORT_TYPE: Type = Type::Const("port".to_string());
  static ref REF_TYPE: Type = Type::Const("reference".to_string());
}

/// Defines a type
// TODO: Union types bool() | integer()
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
  /// Refers to a type name
  Var(TypeVar),
  /// Refers to a named existing type
  Const(String),
  /// A lambda-calculus arrow operation `left -> right`, taking one type and outputting one type
  Arrow {
    in_arg: Box<Type>,
    result: Box<Type>,
  },
}

impl Type {
  pub fn new_var(s: &str) -> Self {
    Type::Var(TypeVar(s.to_string()))
  }

  /// Create a new nested arrow type for multiple arguments function
  pub fn new_fun(args: &[Type], ret: &Type) -> Self {
    assert!(!args.is_empty(), "Args to Type::new_fun must not be empty, or it degenerates into a constant");
    if args.len() == 1 {
      Type::Arrow { in_arg: args[0].clone().into_box(), result: ret.clone().into_box() }
    } else {
      Type::Arrow {
        in_arg: args[0].clone().into_box(),
        result: Self::new_fun(&args[1..], ret).into_box(),
      }
    }
  }

  pub fn into_box(self) -> Box<Self> { Box::from(self) }

  //---------------------
  // Preexisting types
  //---------------------

  pub fn bool() -> &'static Self { &BOOL_TYPE }

  pub fn integer() -> &'static Self { &INT_TYPE }

  /// Helper: Builds a vec of two integer() types (useful for functions taking 2 integers)
  pub fn integer_vec2() -> Vec<Type> { vec![INT_TYPE.clone(), INT_TYPE.clone()] }

  pub fn float() -> &'static Self { &FLOAT_TYPE }
  pub fn atom() -> &'static Self { &ATOM_TYPE }
  pub fn pid() -> &'static Self { &PID_TYPE }
  pub fn port() -> &'static Self { &PORT_TYPE }
  pub fn reference() -> &'static Self { &REF_TYPE }

  //     normtype (TArr a b) = TArr (normtype a) (normtype b)
  //     normtype (TCon a)   = TCon a
  //     normtype (TVar a)   =
  //       case lookup a ord of
  //         Just x -> TVar x
  //         Nothing -> error "type variable not in signature"
  pub fn normtype(&self, ord: &HashSet<(TypeVar, String)>) -> Type {
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
  InfiniteType(TypeVar, Box<Type>),
  UnboundVariable(String),
  Msg(String), // some unsupported type error reported as string
  MathOpNumberExpected{arg: usize, op: ErlBinaryOp},
}

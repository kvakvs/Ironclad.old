use std::collections::HashMap;
use std::rc::Rc;

/// Defines a name of a type
#[derive(Debug, Copy, Clone)]
pub struct TVar(String);

/// Defines a type
#[derive(Debug, Copy, Clone)]
pub enum Type {
  /// Refers to a type name
  TVar(TVar),
  /// Refers to a const value in the type
  TConst(String),
  /// A lambda-calculus arrow operation `left -> right`, taking one type and outputting one type
  TArr {
    left: Rc<Type>,
    right: Rc<Type>,
  },
}

#[derive(Debug)]
pub enum TypeError {
  UnificationFail(Rc<Type>, Rc<Type>),
  InfiniteType(TVar, Rc<Type>),
  UnboundVariable(String),
  Msg(String), // some unsupported type error reported as string
}

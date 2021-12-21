//! Defines structs for AST nodes representing binary operators (A + B)
use std::ops::Deref;
use std::sync::{Arc, RwLock};

use crate::core_erlang::syntax_tree::core_ast::CoreAst;
use crate::core_erlang::syntax_tree::core_op::{CoreBinaryOp};
use crate::erl_error::{ErlError, ErlResult};
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;
use crate::typing::type_error::TypeError;

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(Debug)]
pub struct BinaryOperatorExpr {
  /// Left operand
  pub left: Arc<CoreAst>,
  /// Right operand
  pub right: Arc<CoreAst>,
  /// The operator
  pub operator: CoreBinaryOp,
}

impl BinaryOperatorExpr {
  /// Gets the result type of a binary operation
  pub fn synthesize_type(&self, scope: &RwLock<Scope>) -> ErlResult<Arc<ErlType>> {
    let left = self.left.synthesize(scope)?;
    let right = self.right.synthesize(scope)?;

    match self.operator {
      CoreBinaryOp::Add | CoreBinaryOp::Sub | CoreBinaryOp::Mul => {
        // A binary math operation can only produce a numeric type, integer if both args are integer
        if !left.is_supertype_of_number() || !right.is_supertype_of_number() {
          // Either left or right are not compatible with number
          Ok(ErlType::none())
        } else if left.is_supertype_of_number() && right.is_supertype_of_number() {
          Ok(ErlType::number())
        } else if left.is_supertype_of_float() || right.is_supertype_of_float() {
          Ok(ErlType::float())
        } else {
          Ok(ErlType::integer())
        }
      }

      CoreBinaryOp::Div => Ok(ErlType::float()),

      CoreBinaryOp::IntegerDiv | CoreBinaryOp::Modulo => Ok(ErlType::integer()),

      CoreBinaryOp::Less | CoreBinaryOp::Greater | CoreBinaryOp::LessEq | CoreBinaryOp::GreaterEq
      | CoreBinaryOp::Eq | CoreBinaryOp::NotEq | CoreBinaryOp::HardEq | CoreBinaryOp::HardNotEq => {
        Ok(ErlType::boolean())
      }

      CoreBinaryOp::ListAppend => Self::synthesize_list_append_op(scope, &left, &right),
      CoreBinaryOp::ListSubtract => {
        // Type of -- will be left, probably some elements which should be missing, but how do we know?
        Ok(left)
      }
      CoreBinaryOp::Comma => self.right.synthesize(scope),

      other => {
        unimplemented!("Don't know how to synthesize binary operation type for operation {} on {:?}",
                       other, self)
      },
    }
  }

  /// For `any list() ++ any list()` operation
  fn synthesize_list_append_op(scope: &RwLock<Scope>,
                               left: &Arc<ErlType>,
                               right: &Arc<ErlType>) -> ErlResult<Arc<ErlType>> {
    // Type of ++ will be union of left and right
    // Left operand must always be a proper list, right can be any list
    // TODO: AnyList, StronglyTypedList, Nil
    match left.deref() {
      ErlType::AnyList => {
        // Ok(left.clone()), // anylist makes ++ result anylist too
        panic!("Internal: Synthesize anylist++any list loses type precision")
      }

      ErlType::StronglyTypedList { elements: left_elements, tail: left_tail } =>
        Self::synthesize_stronglist_append(scope, left, left_elements, left_tail, right),

      ErlType::List { elements: left_elements, tail: left_tail } =>
        Self::synthesize_list_of_t_append(scope, left, right, left_elements, left_tail),

      other_left => {
        // left is not a list
        let msg = format!("List append operation ++ expected a list in its left argument, got {}", other_left);
        ErlError::type_error(TypeError::ListExpected { msg })
      }
    }
  }

  /// For `list() ++ list(T1, T2...)` operation
  fn synthesize_stronglist_append(_scope: &RwLock<Scope>,
                                  left: &Arc<ErlType>,
                                  left_elements: &[Arc<ErlType>],
                                  _left_tail: &Option<Arc<ErlType>>,
                                  right: &Arc<ErlType>) -> ErlResult<Arc<ErlType>> {
    match right.deref() {
      ErlType::AnyList => panic!("Internal: Synthesize stronglist++anylist loses type precision"),
      ErlType::List { elements: right_elements, tail: right_tail } => {
        let elements: Vec<Arc<ErlType>> = left_elements.iter()
            .map(|l_elem| {
              ErlType::new_union(&vec![l_elem.clone(), right_elements.clone()])
            })
            .collect();
        let result_list = ErlType::StronglyTypedList {
          elements,
          tail: right_tail.clone(),
        };
        Ok(result_list.into())
      }
      ErlType::StronglyTypedList { elements: right_elements, tail: right_tail } => {
        let elements: Vec<Arc<ErlType>> = left_elements.iter().zip(right_elements.iter())
            .map(|(l_elem, r_elem)| {
              ErlType::new_union(&vec![l_elem.clone(), r_elem.clone()])
            })
            .collect();
        let result_list = ErlType::StronglyTypedList {
          elements,
          tail: right_tail.clone(),
        };
        Ok(result_list.into())
      }
      ErlType::Nil => Ok(left.clone()),
      other_right => {
        // right is not a list
        let msg = format!("List append operation ++ expected a list in its right argument, got {}", other_right);
        ErlError::type_error(TypeError::ListExpected { msg })
      }
    }
  }

  /// For `list(T) ++ any list` operation
  fn synthesize_list_of_t_append(_scope: &RwLock<Scope>,
                                 _left: &Arc<ErlType>,
                                 right: &Arc<ErlType>,
                                 left_elements: &Arc<ErlType>,
                                 left_tail: &Option<Arc<ErlType>>) -> ErlResult<Arc<ErlType>> {
    assert!(left_tail.is_none(), "Left operand for ++ must always be a proper list");

    match right.deref() {
      ErlType::AnyList => panic!("Internal: Synthesize list(T)++anylist loses type precision"),
      ErlType::List { elements: right_elements, tail: right_tail } => {
        let union_t = ErlType::new_union(&vec![left_elements.clone(), right_elements.clone()]);

        // Result type for ++ is union of left and right types, and right tail is applied as the
        // tail type for result
        let result_type = ErlType::List {
          elements: union_t,
          tail: right_tail.clone(),
        };
        Ok(result_type.into())
      }
      other_right => {
        // right is not a list
        let msg = format!("List append operation ++ expected a list in its right argument, got {}", other_right);
        ErlError::type_error(TypeError::ListExpected { msg })
      }
    }
  }

  // /// Gets the type for a binary operation, type is widened for numeric ops (return unions of
  // /// types) which later will be constrained by the type equations solver.
  // /// Returns None if the input type is not limited to any type.
  // pub fn get_arg_type(&self) -> Option<Arc<ErlType>> {
  //   match self.operator {
  //     CoreBinaryOp::Add | CoreBinaryOp::Sub | CoreBinaryOp::Mul | CoreBinaryOp::Div => {
  //       Some(ErlType::union_of(vec![TypePrefab::any_integer(), TypePrefab::float()],
  //                              true))
  //     }
  //
  //     CoreBinaryOp::IntegerDiv | CoreBinaryOp::Modulo => Some(TypePrefab::any_integer()),
  //
  //     CoreBinaryOp::Less | CoreBinaryOp::Greater | CoreBinaryOp::LessEq | CoreBinaryOp::GreaterEq
  //     | CoreBinaryOp::Eq | CoreBinaryOp::NotEq | CoreBinaryOp::HardEq | CoreBinaryOp::HardNotEq => {
  //       None
  //     }
  //
  //     CoreBinaryOp::ListAppend | CoreBinaryOp::ListSubtract => Some(TypePrefab::any_list()),
  //     CoreBinaryOp::Comma => Some(TypePrefab::any())
  //   }
  // }
}

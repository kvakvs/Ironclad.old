//! Defines structs for AST nodes representing ironclad_exe operators (A + B) and unary (+A)
use ::function_name::named;
use std::ops::Deref;
use std::sync::{Arc, RwLock};
use libironclad_error::ic_error::IcResult;
use libironclad_error::source_loc::SourceLoc;
use crate::syntax_tree::erl_ast::ErlAst;
use crate::syntax_tree::erl_error::ErlError;
use crate::syntax_tree::erl_op::ErlBinaryOp;
use crate::typing::erl_type::ErlType;
use crate::typing::scope::Scope;
use crate::typing::type_error::TypeError;

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(Debug)]
pub struct ErlBinaryOperatorExpr {
  /// Left operand
  pub left: Arc<ErlAst>,
  /// Right operand
  pub right: Arc<ErlAst>,
  /// The operator
  pub operator: ErlBinaryOp,
}

impl ErlBinaryOperatorExpr {
  /// Create a ironclad_exe operator, caller is to wrap it with ErlAst::BinOp(location, _)
  pub fn new(left: Arc<ErlAst>, op: ErlBinaryOp, right: Arc<ErlAst>) -> Self {
    Self {
      left,
      right,
      operator: op,
    }
  }

  /// From left and multiple right components, build a right-associative tree of expressions.
  /// Try pair last and one before last, then take result and pair with previous one, ... and so on
  pub fn new_right_assoc(loc: &SourceLoc, left: Arc<ErlAst>, tail: &[(ErlBinaryOp, Arc<ErlAst>)]) -> Arc<ErlAst> {
    if tail.is_empty() {
      return left;
    }

    // Take rightmost element in the tail[] array, together with the operator
    // And build the recursive tree from the remaining on the left
    let (op, right) = &tail[tail.len() - 1];
    let build_left_side = Self::new_right_assoc(loc, left, &tail[0..tail.len() - 1]);

    ErlAst::BinaryOp {
      location: loc.clone(),
      expr: Self::new(build_left_side, *op, right.clone()),
    }.into()
  }

  /// From left and multiple right components, build a left-associative tree of expressions.
  /// Try pair first and the first element in tail, then take result and pair with second, ... and so on
  pub fn new_left_assoc(loc: &SourceLoc, left: Arc<ErlAst>, tail: &[(ErlBinaryOp, Arc<ErlAst>)]) -> Arc<ErlAst> {
    if tail.is_empty() {
      return left;
    }

    // Take leftmost element in the tail[] array, together with the operator
    // And build the recursive tree from the remaining on the left
    let (op, first) = &tail[0];
    let build_right_side = Self::new_left_assoc(loc, first.clone(), &tail[1..tail.len()]);

    ErlAst::BinaryOp {
      location: loc.clone(),
      expr: Self::new(left, *op, build_right_side),
    }.into()
  }

  /// Gets the result type of a ironclad_exe operation
  pub fn synthesize_binop_type(&self, scope: &RwLock<Scope>) -> IcResult<Arc<ErlType>> {
    let left = self.left.synthesize(scope)?;
    let right = self.right.synthesize(scope)?;

    match self.operator {
      ErlBinaryOp::Add | ErlBinaryOp::Sub | ErlBinaryOp::Mul => {
        // A ironclad_exe math operation can only produce a numeric type, integer if both args are integer
        if !left.is_supertype_of_number() || !right.is_supertype_of_number() {
          // Either left or right are not compatible with number
          Ok(ErlType::none())
        // } else if left.is_supertype_of_number() && right.is_supertype_of_number() {
        //   Ok(ErlType::number())
        } else if left.is_supertype_of_integer() && right.is_supertype_of_integer() {
          Ok(ErlType::integer())
        } else {
          Ok(ErlType::float())
        }
      }

      ErlBinaryOp::Div => Ok(ErlType::float()),

      ErlBinaryOp::IntegerDiv | ErlBinaryOp::Remainder => Ok(ErlType::integer()),

      ErlBinaryOp::Less | ErlBinaryOp::Greater | ErlBinaryOp::LessEq | ErlBinaryOp::GreaterEq
      | ErlBinaryOp::Eq | ErlBinaryOp::NotEq | ErlBinaryOp::HardEq | ErlBinaryOp::HardNotEq => {
        Ok(ErlType::boolean())
      }

      ErlBinaryOp::ListAppend => Self::synthesize_list_append_op(scope, &left, &right),
      ErlBinaryOp::ListSubtract => {
        // Type of -- will be left, probably some elements which should be missing, but how do we know?
        Ok(left)
      }
      ErlBinaryOp::Comma => self.right.synthesize(scope),

      other => {
        unimplemented!("Don't know how to synthesize ironclad_exe operation type for operation {} on {:?}",
                       other, self)
      }
    }
  }

  /// For `any list() ++ any list()` operation
  #[named]
  fn synthesize_list_append_op(scope: &RwLock<Scope>,
                               left: &Arc<ErlType>,
                               right: &Arc<ErlType>) -> IcResult<Arc<ErlType>> {
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
        ErlError::type_error(
          SourceLoc::unimplemented(file!(), function_name!()),
          TypeError::ListExpected { msg })
      }
    }
  }

  /// For `list() ++ list(T1, T2...)` operation
  #[named]
  fn synthesize_stronglist_append(_scope: &RwLock<Scope>,
                                  left: &Arc<ErlType>,
                                  left_elements: &[Arc<ErlType>],
                                  _left_tail: &Option<Arc<ErlType>>,
                                  right: &Arc<ErlType>) -> IcResult<Arc<ErlType>> {
    match right.deref() {
      ErlType::AnyList => panic!("Internal: Synthesize stronglist++anylist loses type precision"),
      ErlType::List { elements: right_elements, tail: right_tail } => {
        let elements: Vec<Arc<ErlType>> = left_elements.iter()
            .map(|l_elem| {
              ErlType::new_union(&[l_elem.clone(), right_elements.clone()])
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
              ErlType::new_union(&[l_elem.clone(), r_elem.clone()])
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
        ErlError::type_error(
          SourceLoc::unimplemented(file!(), function_name!()),
          TypeError::ListExpected { msg })
      }
    }
  }

  /// For `list(T) ++ any list` operation
  #[named]
  fn synthesize_list_of_t_append(_scope: &RwLock<Scope>,
                                 _left: &Arc<ErlType>,
                                 right: &Arc<ErlType>,
                                 left_elements: &Arc<ErlType>,
                                 left_tail: &Option<Arc<ErlType>>) -> IcResult<Arc<ErlType>> {
    assert!(left_tail.is_none(), "Left operand for ++ must always be a proper list");

    match right.deref() {
      ErlType::AnyList => panic!("Internal: Synthesize list(T)++anylist loses type precision"),
      ErlType::List { elements: right_elements, tail: right_tail } => {
        let union_t = ErlType::new_union(&[left_elements.clone(), right_elements.clone()]);

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
        ErlError::type_error(
          SourceLoc::unimplemented(file!(), function_name!()),
          TypeError::ListExpected { msg })
      }
    }
  }
}

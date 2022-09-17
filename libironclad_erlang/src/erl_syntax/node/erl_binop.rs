//! Defines structs for AST nodes representing binary operators (A + B) and unary (+A)
use crate::erl_syntax::erl_ast::node_impl::{AstNodeImpl, AstNodeType};
use crate::erl_syntax::erl_ast::AstNode;
use crate::erl_syntax::erl_error::ErlError;
use crate::erl_syntax::erl_op::ErlBinaryOp;
use crate::erl_syntax::literal_bool::LiteralBool;
use crate::error::ic_error::IcResult;
use crate::project::module::module_impl::ErlModule;
use crate::project::module::scope::scope_impl::Scope;
use crate::source_loc::SourceLoc;
use crate::typing::erl_type::typekind::TypeKind;
use crate::typing::erl_type::{ErlType, TypeImpl};
use crate::typing::type_error::TypeError;
use std::ops::Deref;

/// Binary operator is a code structure `Expr <operator> Expr`
#[derive(Debug)]
pub struct ErlBinaryOperatorExpr {
  /// Left operand
  pub left: AstNode,
  /// Right operand
  pub right: AstNode,
  /// The operator
  pub operator: ErlBinaryOp,
}

impl ErlBinaryOperatorExpr {
  /// Create a binary operator, caller is to wrap it with ErlAst::BinOp(location, _)
  pub(crate) fn new(left: AstNode, op: ErlBinaryOp, right: AstNode) -> Self {
    Self { left, right, operator: op }
  }

  /// From left and multiple right components, build a right-associative tree of expressions.
  /// Try pair last and one before last, then take result and pair with previous one, ... and so on
  pub(crate) fn new_right_assoc(
    loc: SourceLoc,
    left: AstNode,
    tail: &[(ErlBinaryOp, AstNode)],
  ) -> AstNode {
    if tail.is_empty() {
      return left;
    }

    // Take rightmost element in the tail[] array, together with the operator
    // And build the recursive tree from the remaining on the left
    let (op, right) = &tail[tail.len() - 1];
    let build_left_side = Self::new_right_assoc(loc.clone(), left, &tail[0..tail.len() - 1]);

    let bin_node = AstNodeType::BinaryOp {
      binop_expr: Self::new(build_left_side, *op, right.clone()),
    };
    AstNodeImpl::construct_with_location(loc, bin_node)
  }

  /// From left and multiple right components, build a left-associative tree of expressions.
  /// Try pair first and the first element in tail, then take result and pair with second, ... and so on
  pub(crate) fn new_left_assoc(
    loc: SourceLoc,
    left: AstNode,
    tail: &[(ErlBinaryOp, AstNode)],
  ) -> AstNode {
    if tail.is_empty() {
      return left;
    }

    // Take leftmost element in the tail[] array, together with the operator
    // And build the recursive tree from the remaining on the left
    let (op, first) = &tail[0];
    let build_right_side = Self::new_left_assoc(loc.clone(), first.clone(), &tail[1..tail.len()]);

    let bin_node = AstNodeType::BinaryOp { binop_expr: Self::new(left, *op, build_right_side) };
    AstNodeImpl::construct_with_location(loc, bin_node)
  }

  /// Gets the result type of a binary operation
  #[allow(dead_code)]
  pub(crate) fn synthesize_binop_type(
    &self,
    location: SourceLoc,
    module: &ErlModule,
    scope: &Scope,
  ) -> IcResult<ErlType> {
    let left = self.left.synthesize(module, scope)?;
    let right = self.right.synthesize(module, scope)?;

    match self.operator {
      ErlBinaryOp::Add | ErlBinaryOp::Sub | ErlBinaryOp::Mul => {
        // A binary math operation can only produce a numeric type, integer if both args are integer
        if !left.is_supertype_of_number() || !right.is_supertype_of_number() {
          // Either left or right are not compatible with number
          Ok(TypeImpl::none())
        // } else if left.is_supertype_of_number() && right.is_supertype_of_number() {
        //   Ok(ErlType::number())
        } else if left.is_supertype_of_integer() && right.is_supertype_of_integer() {
          Ok(TypeImpl::integer())
        } else {
          Ok(TypeImpl::float())
        }
      }

      ErlBinaryOp::Div => Ok(TypeImpl::float()),

      ErlBinaryOp::IntegerDiv | ErlBinaryOp::Remainder => Ok(TypeImpl::integer()),

      ErlBinaryOp::Less
      | ErlBinaryOp::Greater
      | ErlBinaryOp::LessEq
      | ErlBinaryOp::GreaterEq
      | ErlBinaryOp::Eq
      | ErlBinaryOp::NotEq
      | ErlBinaryOp::HardEq
      | ErlBinaryOp::HardNotEq => Ok(TypeImpl::boolean()),

      ErlBinaryOp::ListAppend => Self::synthesize_list_append_op(location, scope, &left, &right),
      ErlBinaryOp::ListSubtract => {
        // Type of -- will be left, probably some elements which should be missing, but how do we know?
        Ok(left)
      }
      ErlBinaryOp::Comma => self.right.synthesize(module, scope),

      other => {
        unimplemented!(
          "Don't know how to synthesize binary operation type for operation {} on {:?}",
          other,
          self
        )
      }
    }
  }

  /// For `any list() ++ any list()` operation
  #[allow(dead_code)]
  fn synthesize_list_append_op(
    location: SourceLoc,
    scope: &Scope,
    left: &ErlType,
    right: &ErlType,
  ) -> IcResult<ErlType> {
    // Type of ++ will be union of left and right
    // Left operand must always be a proper list, right can be any list
    // TODO: AnyList, StronglyTypedList, Nil
    match &left.kind {
      TypeKind::AnyList => {
        // Ok(left.clone()), // anylist makes ++ result anylist too
        panic!("Internal: Synthesize anylist++any list loses type precision")
      }

      TypeKind::StronglyTypedList { elements: left_elements, tail: left_tail } => {
        Self::synthesize_stronglist_append(location, scope, left, left_elements, left_tail, right)
      }

      TypeKind::List {
        elements: left_elements,
        tail: left_tail,
        is_non_empty: left_non_empty,
      } => Self::synthesize_list_of_t_append(
        location,
        scope,
        left,
        right,
        left_elements,
        left_tail,
        *left_non_empty,
      ),

      other_left => {
        // left is not a list
        let msg =
          format!("List append operation ++ expected a list in its left argument, got {}", &left);
        ErlError::type_error(location, TypeError::ListExpected { msg })
      }
    }
  }

  /// For `list() ++ list(T1, T2...)` operation
  #[allow(dead_code)]
  fn synthesize_stronglist_append(
    location: SourceLoc,
    _scope: &Scope,
    left: &ErlType,
    left_elements: &[ErlType],
    _left_tail: &Option<ErlType>,
    right: &ErlType,
  ) -> IcResult<ErlType> {
    match &right.kind {
      TypeKind::AnyList => {
        panic!("Internal: Synthesize stronglist++anylist loses type precision")
      }
      TypeKind::List { elements: right_elements, tail: right_tail, .. } => {
        let elements: Vec<ErlType> = left_elements
          .iter()
          .map(|l_elem| {
            TypeImpl::new_unnamed(TypeKind::new_union(&[l_elem.clone(), right_elements.clone()]))
          })
          .collect();
        let result_list = TypeKind::StronglyTypedList { elements, tail: right_tail.clone() };
        todo!("support non-empty attribute");
        Ok(TypeImpl::new_unnamed(result_list))
      }
      TypeKind::StronglyTypedList { elements: right_elements, tail: right_tail } => {
        let elements: Vec<ErlType> = left_elements
          .iter()
          .zip(right_elements.iter())
          .map(|(l_elem, r_elem)| {
            TypeImpl::new_unnamed(TypeKind::new_union(&[l_elem.clone(), r_elem.clone()]))
          })
          .collect();
        let result_list = TypeKind::StronglyTypedList { elements, tail: right_tail.clone() };
        Ok(TypeImpl::new_unnamed(result_list))
      }
      TypeKind::Nil => Ok(left.clone()),
      other_right => {
        // right is not a list
        let msg =
          format!("List append operation ++ expected a list in its right argument, got {}", &right);
        ErlError::type_error(location, TypeError::ListExpected { msg })
      }
    }
  }

  /// For `list(T) ++ any list` operation
  #[allow(dead_code)]
  fn synthesize_list_of_t_append(
    location: SourceLoc,
    _scope: &Scope,
    _left: &ErlType,
    right: &ErlType,
    left_elements: &ErlType,
    left_tail: &Option<ErlType>,
    left_non_empty: bool,
  ) -> IcResult<ErlType> {
    assert!(left_tail.is_none(), "Left operand for ++ must always be a proper list");

    match &right.kind {
      TypeKind::AnyList => {
        panic!("Internal: Synthesize list(T)++anylist loses type precision")
      }
      TypeKind::List {
        elements: right_elements,
        tail: right_tail,
        is_non_empty: right_non_empty,
      } => {
        let union_t = TypeImpl::new_unnamed(TypeKind::new_union(&[
          left_elements.clone(),
          right_elements.clone(),
        ]));

        // Result type for ++ is union of left and right types, and right tail is applied as the
        // tail type for result
        let result_type = TypeKind::List {
          elements: union_t,
          tail: right_tail.clone(),
          is_non_empty: *right_non_empty || left_non_empty,
        };
        Ok(TypeImpl::new_unnamed(result_type))
      }
      other_right => {
        // right is not a list
        let msg =
          format!("List append operation ++ expected a list in its right argument, got {}", &right);
        ErlError::type_error(location, TypeError::ListExpected { msg })
      }
    }
  }

  /// Try to figure out whether this binop resolves to a boolean
  #[allow(dead_code)]
  pub(crate) fn walk_boolean_litexpr(&self) -> LiteralBool {
    let left_bool = self.left.walk_boolean_litexpr();
    let right_bool = self.right.walk_boolean_litexpr();

    match self.operator {
      ErlBinaryOp::AndAlso | ErlBinaryOp::And | ErlBinaryOp::Comma => {
        return left_bool.and(&right_bool)
      }
      ErlBinaryOp::OrElse | ErlBinaryOp::Or | ErlBinaryOp::Semicolon => {
        return left_bool.or(&right_bool)
      }
      ErlBinaryOp::Xor => return left_bool.xor(&right_bool),
      _ => {} // proceed to calculate non-booleans
    }

    // let left_val = self.left.walk_litexpr();
    // let right_val = self.right.walk_litexpr();

    match self.operator {
      ErlBinaryOp::Less | ErlBinaryOp::Greater | ErlBinaryOp::LessEq | ErlBinaryOp::GreaterEq => {
        unimplemented!("LitExpr ordering comparisons")
      }
      ErlBinaryOp::Eq | ErlBinaryOp::NotEq | ErlBinaryOp::HardEq | ErlBinaryOp::HardNotEq => {
        unimplemented!("LitExpr equality checks")
      }
      _ => LiteralBool::NotABoolean,
    }
  }
}

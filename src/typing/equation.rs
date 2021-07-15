use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;
use crate::typing::typevar::TypeVar;
use crate::erl_error::{ErlResult, ErlError};
use crate::typing::erl_type::ErlType::Union;

pub struct TypeEquation<'a> {
  left: ErlType,
  right: ErlType,
  node: &'a ErlAst,
}

impl<'a> TypeEquation<'a> {
  pub fn new(node: &'a ErlAst, ty1: ErlType, ty2: ErlType) -> Self {
    Self {
      left: ty1,
      right: ty2,
      node,
    }
  }

  /// Generate type equations from node. Each type variable is opposed to some type which we know, or
  /// to Any, if we don't know.
  pub fn generate_equations(ast: &'a ErlAst, result: &'a mut Vec<TypeEquation<'a>>) -> ErlResult<()> {
    match ast.get_children() {
      Some(c) => {
        let (_, errors): (Vec<_>, Vec<_>) = c.iter()
            .map(|ast_node| { // drop OK results, keep errors
              Self::generate_equations(ast_node, result)
            })
            .partition(Result::is_ok);
        let mut errors: Vec<ErlError> = errors.into_iter().map(Result::unwrap_err).collect();
        match errors.len() {
          0 => (),
          1 => return Err(errors.pop().unwrap()),
          _ => return Err(ErlError::Multiple(errors)),
        }
      }
      None => {}
    }

    match ast {
      ErlAst::Forms(_) => unreachable!("Must not call generate_equations() on Forms"),
      ErlAst::ModuleAttr { .. } => Ok(()),
      ErlAst::Lit(_) => Ok(()), // creates no equation, type is known
      ErlAst::NewFunction { ret, clauses, .. } => {
        // Return type of a function is union of its clauses return types
        let union_t = Union(clauses.iter().map(|c| c.get_type()).collect());
        result.push(TypeEquation::new(ast, ret.clone(), union_t));
        Ok(())
      }
      ErlAst::FClause { .. } => {}
      ErlAst::CClause { .. } => {}
      ErlAst::Var { .. } => {}
      ErlAst::App { .. } => {}
      ErlAst::Let { .. } => {}
      ErlAst::Case { .. } => {}
      ErlAst::BinaryOp { left, right, op, ty } => {
        match op.get_arg_type() {
          Some(arg_type) => {
            // Both sides of a binary op must have type appropriate for that op
            result.push(TypeEquation::new(ast, left.get_type(), &arg_type));
            result.push(TypeEquation::new(ast, right.get_type(), &arg_type));
          }
          None => {}
        }
        Ok(())
      }
      ErlAst::UnaryOp { expr, op } => {
        // Equation of expression type must match either bool for logical negation,
        // or (int|float) for numerical negation
        result.push(TypeEquation::new(ast, expr.get_type(), op.get_result_type()));
        Ok(())
      }
    }

    // elif isinstance(node, ast.OpExpr):
    //     node.visit_children(lambda c: generate_equations(c, type_equations))
    // # All op arguments are integers.
    //     type_equations.append(TypeEquation(node.left._type, IntType(), node))
    // type_equations.append(TypeEquation(node.right._type, IntType(), node))
    // # Some ops return boolean, and some return integer.
    // if node.op in {'!=', '==', '>=', '<=', '>', '<'}:
    //     type_equations.append(TypeEquation(node._type, BoolType(), node))
    // else:
    // type_equations.append(TypeEquation(node._type, IntType(), node))

    // elif isinstance(node, ast.AppExpr):
    //     node.visit_children(lambda c: generate_equations(c, type_equations))
    // argtypes = [arg._type for arg in node.args]
    // # An application forces its function's type.
    // type_equations.append(TypeEquation(node.func._type,
    //                                    FuncType(argtypes, node._type),
    //                                    node))

    // elif isinstance(node, ast.IfExpr):
    //     node.visit_children(lambda c: generate_equations(c, type_equations))
    // type_equations.append(TypeEquation(node.ifexpr._type, BoolType(), node))
    // type_equations.append(TypeEquation(node._type, node.thenexpr._type, node))
    // type_equations.append(TypeEquation(node._type, node.elseexpr._type, node))

    // elif isinstance(node, ast.LambdaExpr):
    //     node.visit_children(lambda c: generate_equations(c, type_equations))
    // argtypes = [node._arg_types[name] for name in node.argnames]
    // type_equations.append(
    //   TypeEquation(node._type,
    //                FuncType(argtypes, node.expr._type), node))
    // else:
    // raise TypingError('unknown node {}', type(node))
  }
}

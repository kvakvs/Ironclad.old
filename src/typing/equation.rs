use crate::syntaxtree::erl::erl_ast::ErlAst;
use crate::typing::erl_type::ErlType;
use crate::typing::typevar::TypeVar;

pub struct TypeEquation<'a> {
  left: TypeVar,
  right: ErlType,
  node: &'a ErlAst,
}

impl<'a> TypeEquation<'a> {
  pub fn new_lit(node: &'a ErlAst, tv: &TypeVar, ty: ErlType) -> Self {
    Self{
      left: tv.clone(),
      right: ty,
      node
    }
  }

  /// Generate type equations from node. Each type variable is opposed to some type which we know, or
  /// to Any, if we don't know.
  pub fn generate_equations(ast: &'a ErlAst, result: &'a mut Vec<TypeEquation<'a>>) {
    match ast {
      ErlAst::Forms(_) => {}
      ErlAst::ModuleAttr { .. } => {}
      ErlAst::Lit{ value, tv } => {
        result.push(TypeEquation::new_lit(ast, tv, value.get_type()))
      }
      ErlAst::Variable { .. } => {}
      ErlAst::Expr { .. } => {}
      ErlAst::Function { .. } => {}
      ErlAst::String(_) => {}
    }

    // if isinstance(node, ast.IntConstant):
    //     type_equations.append(TypeEquation(node._type, IntType(), node))
    // elif isinstance(node, ast.BoolConstant):
    //     type_equations.append(TypeEquation(node._type, BoolType(), node))
    // elif isinstance(node, ast.Identifier):
    // # Identifier references add no equations.
    //     pass
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

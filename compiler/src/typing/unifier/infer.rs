//! Contains code for type inference in `Unifier` impl
use crate::typing::unifier::Unifier;
use crate::typing::erl_type::ErlType;
use crate::typing::fn_type::FunctionType;
use crate::typing::fn_clause_type::FnClauseType;
use crate::erlang::syntax_tree::erl_ast::ErlAst;

impl Unifier {
  /// Applies the unifier subst to typ. Also known as: Apply Unifier operation
  /// Returns a type where all occurrences of variables bound in subst were replaced (recursively);
  /// on failure returns None.
  pub fn infer_type(&mut self, ty: &ErlType) -> ErlType {
    // A simple type or type without a substitution, will be inferred as itself
    if self.subst.is_empty() || ty.is_simple_value_type() {
      return ty.clone();
    }

    // A type variable is checked in the subst table and inference algorithm is called on it
    match &ty {
      ErlType::TVar(tvar) => {
        match self.subst.get(tvar) {
          Some(entry) => {
            let entry_ = entry.clone();
            self.infer_type(&entry_)
          }
          None => {
            // If the type variable is not in the subst table, just return it as is
            ty.clone()
          }
        }
      }
      // If type is a function, infer its components if they contain any type variables
      ErlType::Fn(ftype) => {
        ErlType::Fn(self.infer_func_type(ftype))
      }
      // For a union of types, infer every member
      ErlType::Union(members) => {
        ErlType::union_of(members.iter()
                                     .map(|m| self.infer_type(m))
                                     .collect(),
                                 true)
      }
      _ => {
        panic!("Don't know how to infer type {}", ty)
      }
    }

    // ErlType::None
  }

  /// Infer components of a function type, and nest the calls to infer each function clause
  fn infer_func_type(&mut self, ftype: &FunctionType) -> FunctionType {
    FunctionType {
      name: ftype.name.clone(),
      arity: ftype.arity,
      clauses: ftype.clauses.iter()
          .map(|t| self.infer_fun_clause_type(t))
          .collect(),
      ret_type: Box::new(self.infer_type(&ftype.ret_type)),
    }
  }

  /// Rebuild a function clause with all components of the old function clause with `infer_type`
  /// applied on them.
  fn infer_fun_clause_type(&mut self, fctype: &FnClauseType) -> FnClauseType {
    FnClauseType {
      arg_types: fctype.arg_types.iter()
          .map(|argt| self.infer_type(argt))
          .collect(),
      ret_ty: Box::new(self.infer_type(&fctype.ret_ty)),
    }
  }

  /// Finds the type of the expression for the given substitution.
  pub fn infer_ast(&mut self, expr: &ErlAst) -> ErlType {
    self.infer_type(&expr.get_type())
  }
}

//! Module provides logic for unifying the type equations generated for a program's AST, and
//! then using this data is able to infer the type of any AST piece from the same program.

use crate::typing::equation::TypeEquation;
use crate::typing::erl_type::ErlType;
use std::collections::HashMap;
use crate::erl_error::{ErlResult, ErlError};
use crate::typing::error::TypeError;
use crate::typing::typevar::TypeVar;
use std::collections::hash_map::Entry;
use std::rc::Rc;
use crate::syntaxtree::erl::erl_ast::ErlAst;
use std::ops::Deref;

type SubstMap = HashMap<TypeVar, ErlType>;

/// A program AST is analyzed and type equations are generated outside of this struct.
/// This struct contains substitution map with found solutions for the type equations.
/// This struct is able to provide type inference for any piece of AST thereafter.
pub struct Unifier {
  /// Generated type constraints in the program
  equations: Vec<TypeEquation>,
  /// Substitution map containing solutions matching type variables to real types
  pub subst: SubstMap,
  /// Root AST node for source queries, like checking whether a function exists
  root: Rc<ErlAst>,
}

impl Unifier {
  /// Create a new Unifier from AST tree, and setup the equations for this code.
  /// This will scan the AST and prepare data for type inference.
  pub fn new(ast: Rc<ErlAst>) -> ErlResult<Self> {
    let mut unifier = Self {
      equations: vec![],
      subst: Default::default(),
      root: ast.clone(),
    };

    TypeEquation::generate_equations(&ast, &mut unifier.equations)?;
    println!("Equations: {:?}", unifier.equations);

    unifier.unify_all_equations().unwrap();
    println!("Unify map: {:?}", &unifier.subst);

    Ok(unifier)
  }

  /// Goes through all generated type equations and applies self.unify() to arrive to a solution
  fn unify_all_equations(&mut self) -> ErlResult<()> {
    let mut equations: Vec<TypeEquation> = Vec::new();
    std::mem::swap(&mut self.equations, &mut equations); // move

    let errors: Vec<ErlError> = equations.iter()
        .map(|eq| {
          self.unify(eq.left.clone(), eq.right.clone())
        })
        .filter(Result::is_err)
        .map(Result::unwrap_err)
        .collect();
    if !errors.is_empty() {
      return Err(ErlError::Multiple(errors));
    }
    Ok(())
  }

  /// Unify two types type1 and type2, with self.subst map
  /// Updates self.subst (map of name->Type) with new record which unifies type1 and type2,
  /// and returns true. Returns false if they can't be unified.
  fn unify(&mut self, type1: ErlType, type2: ErlType) -> ErlResult<()> {
    if type1 == type2 {
      return Ok(());
    }

    if let ErlType::TVar(tv) = type1 {
      return self.unify_variable(tv, type2);
    }

    if let ErlType::Function { arg_ty: arg_ty1, ret: ret1, .. } = &type1 {
      if let ErlType::Function { arg_ty: arg_ty2, ret: ret2, .. } = &type2 {
        if arg_ty1.len() != arg_ty2.len() {
          return Err(ErlError::from(TypeError::FunAritiesDontMatch));
        }

        // Unify functions return types
        self.unify(*ret1.clone(), *ret2.clone())?;

        // Then unify each arg of function1 with corresponding arg of function2
        let errors: Vec<ErlError> = arg_ty1.iter().zip(arg_ty2.iter())
            .map(|(t1, t2)| {
              self.unify(t1.clone(), t2.clone())
            })
            .filter(Result::is_err)
            .map(Result::unwrap_err)
            .collect();
        if !errors.is_empty() {
          return Err(ErlError::Multiple(errors));
        }
        return Ok(());
      }
    }

    // Union should not be broken into subtypes
    if let ErlType::Union(types) = &type2 {
      if self.check_in_union(&type1, types) {
        return Ok(());
      }
    }

    // Left is a LocalFunction and the right is a function type
    // Atom must be an existing local function
    if let ErlType::LocalFunction { name: name1, arity: arity1 } = &type1 {
      if let ErlType::Function { arg_ty: arg_ty2, .. } = &type2 {
        // TODO: Find_fun can be cached in some dict? Or use a module struct with local fun dict
        let found_fun = self.root.find_fun(&name1).unwrap();

        if let ErlAst::NewFunction { clauses, .. } = found_fun.deref() {
          if let ErlAst::FClause { args, .. } = clauses[0].deref() {
            if args.len() == *arity1 && arg_ty2.len() == *arity1 {
              return Ok(());
            }
          }
        }
      }
    }

    Err(ErlError::from(TypeError::TypesDontMatch {
      t1: type1.clone(),
      t2: type2.clone(),
    }))
  }

  /// Whether any member of type union matches type t?
  fn check_in_union(&mut self, t: &ErlType, union: &Vec<ErlType>) -> bool {
    union.iter().any(|member| {
      self.unify(t.clone(), member.clone()).is_ok()
    })
  }

  fn unify_variable(&mut self, tvar: TypeVar, ty: ErlType) -> ErlResult<()> {
    if let Entry::Occupied(entry1) = self.subst.entry(tvar.clone()) {
      let subst_entry = entry1.get().clone();
      return self.unify(subst_entry, ty);
    }

    if let ErlType::TVar { .. } = ty {
      if let Entry::Occupied(entry2) = self.subst.entry(tvar.clone()) {
        let subst_entry = entry2.get().clone();
        return self.unify(ErlType::TVar(tvar), subst_entry);
      }
    }

    if self.occurs_check(&tvar, &ty) {
      return Err(ErlError::from(TypeError::OccursCheckFailed { tvar, ty }));
    }

    // tvar is not yet in subst and can't simplify the right side. Extend subst.
    self.subst.insert(tvar.clone(), ty.clone());
    Ok(())
  }

  /// Does the variable v occur anywhere inside typ?
  /// Variables in typ are looked up in subst and the check is applied
  ///     recursively.
  //     elif isinstance(typ, FuncType):
  //         return (occurs_check(v, typ.rettype, subst) or
  //                 any(occurs_check(v, arg, subst) for arg in typ.argtypes))
  fn occurs_check(&self, tv: &TypeVar, ty: &ErlType) -> bool {
    // if ty is a TypeVar and they're equal
    match ty {
      ErlType::TVar(ty_inner) => ty_inner == tv,
      ErlType::Function { arg_ty, ret, .. } => {
        return self.occurs_check(tv, ret) ||
            arg_ty.iter().any(|a| self.occurs_check(tv, a));
      }
      ErlType::Union(members) => {
        members.iter().any(|m| self.occurs_check(tv, m))
      }
      // Any simple type cannot have typevar tv in it, so all false
      simple_type if simple_type.is_simple_value_type() => false,

      _ => todo!("Don't know how to occurs_check on typevar {:?} and type {:?}", tv, ty)
    }

    // false
  }

  /// Applies the unifier subst to typ.
  /// Returns a type where all occurrences of variables bound in subst
  /// were replaced (recursively); on failure returns None.
  /// Also known as: Apply Unifier (apply_unifier)
  pub fn infer_type(&mut self, ty: ErlType) -> ErlType {
    if self.subst.is_empty() || ty.is_simple_value_type() {
      return ty;
    }

    if let ErlType::TVar(tvar) = &ty {
      match self.subst.entry(tvar.clone()) {
        Entry::Occupied(entry) => {
          let entry_val = entry.get().clone();
          return self.infer_type(entry_val);
        }
        Entry::Vacant(_) => return ty,
      }
    }

    if let ErlType::Function { arg_ty, ret, name } = &ty {
      let new_fn = ErlType::Function {
        name: name.clone(),
        arg_ty: arg_ty.iter()
            .map(|t| self.infer_type(t.clone()))
            .collect(),
        ret: Box::new(self.infer_type(*ret.clone())),
      };
      return new_fn;
    }

    ErlType::None
  }

  /// Finds the type of the expression for the given substitution.
  pub fn infer_ast(&mut self, expr: Rc<ErlAst>) -> ErlType {
    self.infer_type(expr.get_type())
  }
}
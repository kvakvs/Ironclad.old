pub mod erltype;
pub mod infer;
mod type_env;
mod polymorphic;
mod subst;
mod substitutable;

#[cfg(test)]
mod tests {
  use crate::typing::infer::Infer;
  use crate::typing::type_env::TypeEnv;
  use crate::typing::erltype::{Type, TVar};
  use crate::typing::polymorphic::Scheme;

  #[test]
  fn generalize_test() {
    let infr = Infer::new();
    let type_env = TypeEnv::new();
    let ty = Type::Arrow {
      in_arg: Box::from(Type::new_var("test1")),
      result: Box::from(Type::Const("const1".to_string())),
    };
    println!("env {:?}", type_env);
    println!("ty {:?}", ty);
    println!("generalize {:?}", infr.generalize(&type_env, ty));
  }

  #[test]
  fn instantiate_test() {
    let mut infr = Infer::new();
    // let type_env = TypeEnv::new();
    let ty = Type::Arrow {
      in_arg: Box::from(Type::new_var("test1")),
      result: Box::from(Type::Const("const1".to_string())),
    };
    let scheme = Scheme {
      type_vars: vec![TVar("tv1".to_string()),
                      TVar("tv2".to_string())],
      ty,
    };
    println!("instantiate {:?}", infr.instantiate(&scheme).unwrap());
  }

  #[test]
  fn unify_test() {
    let infr = Infer::new();
    let ty1 = Type::Arrow {
      in_arg: Box::from(Type::new_var("test11")),
      result: Box::from(Type::new_var("test12")),
    };
    let ty2 = Type::Arrow {
      in_arg: Box::from(Type::new_var("test2")),
      result: Box::from(Type::Const("const2".to_string())),
    };
    println!("unify t1:t2 = {:?}", infr.unify(&ty1, &ty2));
  }
}
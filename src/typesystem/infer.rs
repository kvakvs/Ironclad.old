use crate::typesystem::erltype::Type;
use crate::typesystem::polymorphic::Scheme;
use crate::typesystem::type_env::TypeEnv;
use crate::typesystem::subst::Subst;
use crate::typesystem::substitutable::Substitutable;
use std::rc::Rc;

pub struct Unique(usize);

// Haskell: type Infer a = ExceptT TypeError (State Unique) a
pub struct Infer<T, TTypeError> {
  err: TTypeError,
  state: Unique,
  ty: T,
}

impl<T, TTypeError> Infer<T, TTypeError> {
  // Haskell:
  // runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
  // runInfer m = case evalState (runExceptT m) initUnique of
  //   Left err  -> Left err
  //   Right res -> Right $ closeOver res
  /// Running the logic inside Infer monad results either in a type scheme or a type error
  pub fn run(&mut self, subst: &Rc<Subst>, ty: &Type) -> Result<Scheme, TTypeError> {
    let unique = Unique(0);
    // let eval = evalState(self.runExceptT, unique);
    match eval {
      Err(err) => Err(err),
      Ok(res) => Ok(Self::close_over(subst, res))
    }
  }

  // closeOver :: (Map.Map TVar Type, Type) -> Scheme
  // closeOver (sub, ty) = normalize sc
  //   where sc = generalize emptyTyenv (apply sub ty)
  fn close_over(subst: &Rc<Subst>, ty: &Type) -> Scheme {
    let sc = Self::generalize(
      TypeEnv::new(),
      ty.apply(subst.clone())
    );
    Self::normalize(sc)
  }
}
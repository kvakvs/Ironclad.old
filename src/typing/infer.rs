use crate::typing::erltype::{Type, TypeError};
use crate::typing::polymorphic::Scheme;
use crate::typing::type_env::TypeEnv;
use crate::typing::subst::Subst;
use crate::typing::substitutable::Substitutable;
use std::rc::Rc;
use crate::syntaxtree::erl::erl_expr::ErlExpr;
use crate::erl_error::ErlResult;

pub struct Unique(usize);

// Haskell: type Infer a = ExceptT TypeError (State Unique) a
pub struct Infer {
  // err: TypeError,
  // state: Unique,
  // ty: Type,
}

impl Infer {
  // Haskell:
  // runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
  // runInfer m = case evalState (runExceptT m) initUnique of
  //   Left err  -> Left err
  //   Right res -> Right $ closeOver res
  /// Running the eval code results either in a type scheme or a type error.
  fn run_infer<TLogic>(&mut self, eval: TLogic) -> ErlResult<Scheme>
    where TLogic: Fn(Unique) {
    Ok(Self::close_over(
      subst,
      eval(Unique(0))?,
    ))
  }

  // closeOver :: (Map.Map TVar Type, Type) -> Scheme
  // closeOver (sub, ty) = normalize sc
  //   where sc = generalize emptyTyenv (apply sub ty)
  fn close_over(subst: &Rc<Subst>, ty: &Type) -> Scheme {
    let sc = Self::generalize(
      TypeEnv::new(),
      ty.apply(subst.clone()),
    );
    Self::normalize(sc)
  }

  // infer :: TypeEnv -> Expr -> Infer (Subst, Type)
  // infer env ex = case ex of
  //
  //   Var x -> lookupEnv env x
  //
  //   Lam x e -> do
  //     tv <- fresh
  //     let env' = env `extend` (x, Forall [] tv)
  //     (s1, t1) <- infer env' e
  //     return (s1, apply s1 tv `TArr` t1)
  //
  //   App e1 e2 -> do
  //     tv <- fresh
  //     (s1, t1) <- infer env e1
  //     (s2, t2) <- infer (apply s1 env) e2
  //     s3       <- unify (apply s2 t1) (TArr t2 tv)
  //     return (s3 `compose` s2 `compose` s1, apply s3 tv)
  //
  //   Let x e1 e2 -> do
  //     (s1, t1) <- infer env e1
  //     let env' = apply s1 env
  //         t'   = generalize env' t1
  //     (s2, t2) <- infer (env' `extend` (x, t')) e2
  //     return (s2 `compose` s1, t2)
  //
  //   If cond tr fl -> do
  //     tv <- fresh
  //     inferPrim env [cond, tr, fl] (typeBool `TArr` tv `TArr` tv `TArr` tv)
  //
  //   Fix e1 -> do
  //     tv <- fresh
  //     inferPrim env [e1] ((tv `TArr` tv) `TArr` tv)
  //
  //   Op op e1 e2 -> do
  //     inferPrim env [e1, e2] (ops op)
  //
  //   Lit (LInt _)  -> return (nullSubst, typeInt)
  //   Lit (LBool _) -> return (nullSubst, typeBool)

  // inferPrim :: TypeEnv -> [Expr] -> Type -> Infer (Subst, Type)
  // inferPrim env l t = do
  //   tv <- fresh
  //   (s1, tf) <- foldM inferStep (nullSubst, id) l
  //   s2 <- unify (apply s1 (tf tv)) t
  //   return (s2 `compose` s1, apply s2 tv)
  //   where
  //   inferStep (s, tf) exp = do
  //     (s', t) <- infer (apply s env) exp
  //     return (s' `compose` s, tf . (TArr t))

  // inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
  // inferExpr env = runInfer . infer env
  fn infer_expr(&mut self, type_env: &Rc<TypeEnv>, expr: &ErlExpr) -> ErlResult<Scheme> {
    self.run_infer(self.infer(type_env))
  }

  // inferTop :: TypeEnv -> [(String, Expr)] -> Either TypeError TypeEnv
  // inferTop env [] = Right env
  // inferTop env ((name, ex):xs) = case inferExpr env ex of
  //   Left err -> Left err
  //   Right ty -> inferTop (extend env (name, ty)) xs

  // normalize :: Scheme -> Scheme
  // normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  //   where
  //     ord = zip (nub $ fv body) (fmap TV letters)
  //
  //     fv (TVar a)   = [a]
  //     fv (TArr a b) = fv a ++ fv b
  //     fv (TCon _)   = []
  //
  //     normtype (TArr a b) = TArr (normtype a) (normtype b)
  //     normtype (TCon a)   = TCon a
  //     normtype (TVar a)   =
  //       case lookup a ord of
  //         Just x -> TVar x
  //         Nothing -> error "type variable not in signature"
}
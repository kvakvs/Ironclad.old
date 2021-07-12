use alphabet::*;
use crate::typing::erltype::{Type, TVar};
use crate::typing::polymorphic::Scheme;
use crate::typing::type_env::TypeEnv;
use crate::typing::subst::SubstitutionMap;
use crate::typing::substitutable::Substitutable;
use std::rc::Rc;
use crate::syntaxtree::erl::erl_expr::ErlExpr;
use crate::erl_error::{ErlResult, ErlError};
use std::collections::HashSet;
use crate::typing::erltype::TypeError::{UnificationFail, InfiniteType};

pub struct Unique(usize);

/// Haskell monad: type Infer a = ExceptT TypeError (State Unique) a
/// We implement this as a struct and store monad state as count
pub struct Infer {
  // err: TypeError,
  // state: Unique,
  // ty: Type,
  count: usize,
}

impl Infer {
  pub fn new() -> Self {
    Self { count: 0 }
  }

  // Haskell:
  // runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
  // runInfer m = case evalState (runExceptT m) initUnique of
  //   Left err  -> Left err
  //   Right res -> Right $ closeOver res
  /// Running the eval code results either in a type scheme or a type error.
  fn run_infer<TLogic>(&mut self, eval: TLogic, sub: &mut SubstitutionMap) -> ErlResult<Scheme>
    where TLogic: Fn(Unique) -> Type {
    let eval_result = eval(Unique(0));
    Ok(self.close_over(sub, eval_result))
  }

  // closeOver :: (Map.Map TVar Type, Type) -> Scheme
  // closeOver (sub, ty) = normalize sc
  //   where sc = generalize emptyTyenv (apply sub ty)
  fn close_over(&self, subst: &mut SubstitutionMap, ty: Type) -> Scheme {
    let applied = Substitutable::RefType(&ty)
        .apply(subst)
        .into_type();
    let scheme = self.generalize(
      &TypeEnv::new(),
      applied,
    );
    self.normalize(&scheme)
  }

  // instantiate ::  Scheme -> Infer Type
  // instantiate (Forall as t) = do
  //   as' <- mapM (const fresh) as
  //   let s = Map.fromList $ zip as as'
  //   return $ apply s t
  /// Converting a σ type into a τ type by creating fresh names for each type variable that does
  /// not appear in the current typing environment.
  pub(crate) fn instantiate(&mut self, scheme: &Scheme) -> ErlResult<Type> {
    // Build same length type_vars2 as in the scheme but replace all names with new
    let renamed_type_vars: Vec<Type> = scheme.type_vars.iter()
        .map(|_v| Type::Var(TVar(self.fresh_name())))
        .collect();
    let mut sub = SubstitutionMap::from_two_lists(&scheme.type_vars, &renamed_type_vars);
    let applied_type = Substitutable::RefType(&scheme.ty)
        .apply(&mut sub)
        .into_type();
    Ok(applied_type)
  }

  // generalize :: TypeEnv -> Type -> Scheme
  // generalize env t  = Forall as t
  // where as = Set.toList $ ftv t `Set.difference` ftv env
  /// Generalization:
  /// Converting a τ type into a σ type by closing over all free type variables in a type scheme.
  pub fn generalize(&self, env: &TypeEnv, ty: Type) -> Scheme {
    let mut ftv_t = Substitutable::RefType(&ty).find_typevars();
    let ftv_env = Substitutable::RefTypeEnv(env).find_typevars();
    ftv_t.retain(|x| !ftv_env.contains(x));
    Scheme {
      type_vars: ftv_t.into_iter().collect(),
      ty,
    }
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
  fn infer_expr(&mut self, _type_env: &Rc<TypeEnv>, _expr: &ErlExpr) -> ErlResult<Scheme> {
    // self.run_infer(self.infer(type_env), ())
    todo!()
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
  fn normalize(&self, scheme: &Scheme) -> Scheme {
    let list1 = Substitutable::RefType(&scheme.ty).find_typevars();
    alphabet!(LATIN_UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ");

    let ord: HashSet<(TVar, String)> =
        list1.into_iter()
            .zip(LATIN_UPPERCASE.iter_words())
            .collect();

    Scheme {
      type_vars: ord.iter()
          .map(|ord_item| ord_item.0.clone())
          .collect(),
      ty: scheme.ty.normtype(&ord),
    }
  }

  // fresh :: Infer Type
  // fresh = do
  //   s <- get
  //   put s{count = count s + 1}
  //   return $ TVar $ TV (letters !! count s)
  /// Produce a new name, by increasing count in the state. TODO: Can use alphabet! macro
  fn fresh_name(&mut self) -> String {
    self.count += 1;
    format!("TVar{}", self.count)
  }

  // occursCheck ::  Substitutable a => TVar -> a -> Bool
  // occursCheck a t = a `Set.member` ftv t
  fn occurs_check(&self, a: &TVar, t: Substitutable) -> bool {
    t.find_typevars().contains(a)
  }

  // unify ::  Type -> Type -> Infer Subst
  // unify (l `TArr` r) (l' `TArr` r')  = do
  //     s1 <- unify l l'
  //     s2 <- unify (apply s1 r) (apply s1 r')
  //     return (s2 `compose` s1)
  // unify (TVar a) t = bind a t
  // unify t (TVar a) = bind a t
  // unify (TCon a) (TCon b) | a == b = return nullSubst
  // unify t1 t2 = throwError $ UnificationFail t1 t2
  /// From the book:
  /// Two terms are said to be unifiable if there exists a unifying substitution set between them.
  /// A substitution set is said to be confluent if the application of substitutions is independent
  /// of the order applied, i.e. if we always arrive at the same normal form regardless of the
  /// order of substitution chosen.
  pub fn unify(&self, l: &Type, r: &Type) -> ErlResult<SubstitutionMap> {
    match (l, r) {
      (Type::Arrow { in_arg: l1, result: r1 },
        Type::Arrow { in_arg: l2, result: r2 }) => {
        let mut s1 = self.unify(l1, l2)?;
        let mut s2 = self.unify(
          &Substitutable::RefType(&r1)
              .apply(&mut s1)
              .into_type(),
          &Substitutable::RefType(&r2)
              .apply(&mut s1)
              .into_type(),
        )?;
        s2.compose(&s1);
        Ok(s2)
      }

      (Type::Var(a), t) => self.bind(a, t),
      (t, Type::Var(a)) => self.bind(a, t),

      (Type::Const(a), Type::Const(b)) if a == b => {
        Ok(SubstitutionMap::new()) // null subst
      },

      (_, _) => Err(ErlError::TypeError(
        UnificationFail(Box::new(l.clone()),
                        Box::new(r.clone()))))
    }
  }

  // bind ::  TVar -> Type -> Infer Subst
  // bind a t | t == TVar a     = return nullSubst
  //          | occursCheck a t = throwError $ InfiniteType a t
  //          | otherwise       = return $ Map.singleton a t
  fn bind(&self, a: &TVar, t: &Type) -> ErlResult<SubstitutionMap> {
    if *t == Type::Var(a.clone()) {
      Ok(SubstitutionMap::new()) // null subst
    } else if self.occurs_check(a, Substitutable::RefType(t)) {
      Err(ErlError::TypeError(InfiniteType(a.clone(), Box::new(t.clone()))))
    } else {
      Ok(SubstitutionMap::new_single(a, t))
    }
  }
}
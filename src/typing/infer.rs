use std::collections::hash_map::Entry;
use std::collections::{HashSet};
use std::rc::Rc;

use alphabet::*;

use crate::erl_error::{ErlError, ErlResult};
use crate::syntaxtree::erl::erl_expr::{ErlExpr, ErlLiteral};
use crate::typing::erltype::{Type, TypeVar};
use crate::typing::erltype::TypeError::{InfiniteType, UnboundVariable, UnificationFail};
use crate::typing::polymorphic::Scheme;
use crate::typing::subst::SubstitutionMap;
use crate::typing::substitutable::Substitutable;
use crate::typing::type_env::TypeEnv;
use crate::syntaxtree::erl::erl_op::ErlBinaryOp;
use crate::erl_error::ErlError::TypeError;
use crate::typing::constraint::{Constraint, Unifier};

pub struct Unique(usize);

/// Haskell monad: type Infer a = ExceptT TypeError (State Unique) a
/// We implement this as a struct and store monad state as count
pub struct Infer {
  // err: TypeError,
  // state: Unique,
  // ty: Type,
  count: usize,
}

/// Returned from Infer::infer()
pub struct InferResult {
  pub s: SubstitutionMap,
  pub t: Type,
}

impl InferResult {
  pub fn empty_with(t: &Type) -> Self {
    Self { s: SubstitutionMap::new(), t: t.clone() }
  }
}

struct SolverInputItem {
  su: SubstitutionMap,
  cs: Constraint,
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
    let scheme = self.generalize(&TypeEnv::new(), &applied);
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
        .map(|_v| Type::Var(self.fresh_name()))
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
  pub fn generalize(&self, env: &TypeEnv, ty: &Type) -> Scheme {
    let mut ftv_t = Substitutable::RefType(ty).find_typevars();
    let ftv_env = Substitutable::RefTypeEnv(env).find_typevars();
    ftv_t.retain(|x| !ftv_env.contains(x));
    Scheme {
      type_vars: ftv_t.into_iter().collect(),
      ty: ty.clone(),
    }
  }

  // lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
  // lookupEnv (TypeEnv env) x =
  //   case Map.lookup x env of
  //     Nothing -> throwError $ UnboundVariable (show x)
  //     Just s  -> do t <- instantiate s
  //                   return (nullSubst, t)
  fn lookup_env(&mut self, type_env: &mut TypeEnv, var: &str) -> ErlResult<InferResult> {
    match type_env.env.entry(TypeVar(var.to_string())) {
      Entry::Occupied(scheme_entry) => {
        let scheme = scheme_entry.get();
        let result = InferResult {
          s: SubstitutionMap::new(),
          t: self.instantiate(scheme)?,
        };
        Ok(result)
      }
      Entry::Vacant(_) => Err(ErlError::TypeError(UnboundVariable(var.to_string())))
    }
  }

  // infer :: TypeEnv -> Expr -> Infer (Subst, Type)
  // infer env ex = case ex of
  fn infer(&mut self, env: &mut TypeEnv, ex: &ErlExpr) -> ErlResult<InferResult> {
    match ex {
      // case: Var x -> lookupEnv env x
      ErlExpr::Var(x) => self.lookup_env(env, x),

      ErlExpr::Lambda { ty: x, expr: e } => self.infer_for_lambda(env, x, e),

      ErlExpr::App { arg, target } => {
        self.infer_for_app(env, arg, target)
      }

      ErlExpr::Let { var, value, in_expr } => {
        self.infer_for_let(env, var, value, in_expr)
      }

      ErlExpr::If { cond, on_true, on_false } => {
        self.infer_for_if(env, cond, on_true, on_false)
      }
      // ErlExpr::Lit(_) => {}
      ErlExpr::BinaryOp { left, right, op } => {
        self.infer_for_binaryop(env, op, left, right)
      }
      // ErlExpr::UnaryOp { .. } => {}

      ErlExpr::Lit(ErlLiteral::Integer(_)) => Ok(InferResult::empty_with(Type::integer())),
      ErlExpr::Lit(ErlLiteral::Bool(_)) => Ok(InferResult::empty_with(Type::bool())),
      ErlExpr::Lit(ErlLiteral::Atom(_)) => Ok(InferResult::empty_with(Type::atom())),
      ErlExpr::Lit(ErlLiteral::Pid) => Ok(InferResult::empty_with(Type::pid())),
      ErlExpr::Lit(ErlLiteral::Reference) => Ok(InferResult::empty_with(Type::reference())),

      _ => {
        println!("Unfinished handling for inference {:?}, {:?}", env, ex);
        todo!()
      }
    }
  }

  //   Lam x e -> do
  //     tv <- fresh
  //     let env' = env `extend` (x, Forall [] tv)
  //     (s1, t1) <- infer env' e
  //     return (s1, apply s1 tv `TArr` t1)
  /// Handle infer() for an ErlExpr::Lam
  /// arguments: ty and expr - components of ErlExpr::Lam
  fn infer_for_lambda(&mut self, env: &mut TypeEnv,
                      x: &TypeVar, expr: &ErlExpr) -> ErlResult<InferResult> {
    let tv = self.fresh_name();

    let mut type_env2 = env.clone();
    type_env2.env.insert(x.clone(),
                         Scheme::new_single_empty(Type::Var(tv.clone())));

    // Infer lambda body type
    let mut s1_t1 = self.infer(&mut type_env2, expr)?;

    let lambda_type = {
      let t = Type::Arrow {
        in_arg: Box::new(Type::Var(tv.clone())),
        result: Box::new(s1_t1.t.clone()),
      };
      Substitutable::RefType(&t).apply(&mut s1_t1.s).into_type()
    };
    let result = InferResult {
      s: s1_t1.s,
      t: Type::Arrow {
        in_arg: Box::new(s1_t1.t.clone()),
        result: Box::new(lambda_type),
      },
    };
    Ok(result)
  }

  //   App e1 e2 -> do
  //     tv <- fresh
  //     (s1, t1) <- infer env e1
  //     (s2, t2) <- infer (apply s1 env) e2
  //     s3       <- unify (apply s2 t1) (TArr t2 tv)
  //     return (s3 `compose` s2 `compose` s1, apply s3 tv)
  /// Handle infer() for an ErlExpr::App
  /// arguments: e1: arg and e2: target - components of ErlExpr::App
  fn infer_for_app(&mut self, env: &mut TypeEnv, e1: &ErlExpr, e2: &ErlExpr) -> ErlResult<InferResult> {
    // Infer type for application arg
    let tv = self.fresh_name();
    let mut s1_t1 = self.infer(env, e1)?;

    // infer type for application target
    let mut env_ = Substitutable::RefTypeEnv(&env).apply(&mut s1_t1.s).into_typeenv();
    let mut s2_t2 = self.infer(&mut env_, e2)?;

    let s3_a = Substitutable::RefType(&s1_t1.t).apply(&mut s2_t2.s).into_type();
    let s3_b = Type::Arrow {
      in_arg: Box::new(s2_t2.t),
      result: Box::new(Type::Var(tv.clone())),
    };
    let mut s3 = self.unify(&s3_a, &s3_b)?;

    let result = InferResult {
      s: s3.compose(&s2_t2.s).compose(&s1_t1.s),
      t: Substitutable::RefType(&Type::Var(tv)).apply(&mut s3).into_type(),
    };
    Ok(result)
  }

  // Let x e1 e2 -> do
  //     (s1, t1) <- infer env e1
  //     let env' = apply s1 env
  //         t'   = generalize env' t1
  //     (s2, t2) <- infer (env' `extend` (x, t')) e2
  //     return (s1 `compose` s2, t2)
  fn infer_for_let(&mut self, env: &mut TypeEnv, x: &String,
                   e1: &ErlExpr, e2: &ErlExpr) -> ErlResult<InferResult> {
    // Infer initializer value type of let expression
    let mut s1_t1 = self.infer(env, e1)?;

    let env_ = Substitutable::RefTypeEnv(env).apply(&mut s1_t1.s).into_typeenv();
    let t_ = self.generalize(&env_, &s1_t1.t);

    // Joining old and new environment, infer type of <let x in...> body
    let mut env2_ = env_.extend(TypeVar(x.clone()), t_.clone());
    let s2_t2 = self.infer(&mut env2_, e2)?;

    let result = InferResult {
      s: s1_t1.s.compose(&s2_t2.s),
      t: s2_t2.t,
    };
    Ok(result)
  }

  // If cond tr fl -> do
  //   (s1, t1) <- infer env cond
  //   (s2, t2) <- infer env tr
  //   (s3, t3) <- infer env fl
  //   s4 <- unify t1 typeBool
  //   s5 <- unify t2 t3
  //   return (s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1, apply s5 t2)
  fn infer_for_if(&mut self, env: &mut TypeEnv,
                  cond: &ErlExpr, on_true: &ErlExpr, on_false: &ErlExpr) -> ErlResult<InferResult> {
    let s1_t1 = self.infer(env, cond)?;
    let s2_t2 = self.infer(env, on_true)?;
    let s3_t3 = self.infer(env, on_false)?;

    // The condition must resolve to a value of bool() type
    let cond_sub = self.unify(&s1_t1.t, Type::bool())?;

    // The both branch types must unify
    let mut branch_sub = self.unify(&s2_t2.t, &s3_t3.t)?;

    let result = InferResult {
      s: branch_sub.compose(&cond_sub)
          .compose(&s3_t3.s)
          .compose(&s2_t2.s)
          .compose(&s1_t1.s),
      t: Substitutable::RefType(&s2_t2.t).apply(&mut branch_sub).into_type(),
    };
    Ok(result)
  }

  //   Op op e1 e2 -> do
  //     (s1, t1) <- infer env e1
  //     (s2, t2) <- infer env e2
  //     tv <- fresh
  //     s3 <- unify (TArr t1 (TArr t2 tv)) (ops Map.! op)
  //     return (s1 `compose` s2 `compose` s3, apply s3 tv)
  fn infer_for_binaryop(&mut self, env: &mut TypeEnv,
                        op: &ErlBinaryOp, e1: &ErlExpr, e2: &ErlExpr) -> ErlResult<InferResult> {
    let s1_t1 = self.infer(env, e1)?;
    let s2_t2 = self.infer(env, e2)?;

    let tv = self.fresh_name();

    // Unify (typeof(e1) -> typeof(e2) -> tv) and op's return type
    let e1_e2_tv = Type::Arrow {
      in_arg: s1_t1.t.clone().into_box(),
      result: Type::Arrow {
        in_arg: s2_t2.t.clone().into_box(),
        result: Type::Var(tv.clone()).into_box(),
      }.into_box(),
    };

    // Get full type for the binary op in form of (arg1_type -> arg2_type -> result_type)
    let op_type = ErlBinaryOp::op_type(*op, &s1_t1.t, &s2_t2.t)?;
    let mut s3 = self.unify(&e1_e2_tv, &op_type)?;

    let result = InferResult {
      s: s1_t1.s.compose(&s2_t2.s).compose(&s3),
      t: Substitutable::RefType(&Type::Var(tv))
          .apply(&mut s3)
          .into_type(),
    };
    Ok(result)
  }

  //   Fix e1 -> do
  //     (s1, t) <- infer env e1
  //     tv <- fresh
  //     s2 <- unify (TArr tv tv) t
  //     return (s2, apply s1 tv)

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

    let ord: HashSet<(TypeVar, String)> =
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
  fn fresh_name(&mut self) -> TypeVar {
    self.count += 1;
    TypeVar(format!("TVar{}", self.count))
  }

  // occursCheck ::  Substitutable a => TVar -> a -> Bool
  // occursCheck a t = a `Set.member` ftv t
  fn occurs_check(&self, a: &TypeVar, t: Substitutable) -> bool {
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
        let s2 = self.unify(
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
      }

      (_, _) => Err(ErlError::TypeError(
        UnificationFail(Box::new(l.clone()),
                        Box::new(r.clone()))))
    }
  }

  // bind ::  TVar -> Type -> Infer Subst
  // bind a t | t == TVar a     = return nullSubst
  //          | occursCheck a t = throwError $ InfiniteType a t
  //          | otherwise       = return $ Map.singleton a t
  fn bind(&self, a: &TypeVar, t: &Type) -> ErlResult<SubstitutionMap> {
    if *t == Type::Var(a.clone()) {
      Ok(SubstitutionMap::new()) // null subst
    } else if self.occurs_check(a, Substitutable::RefType(t)) {
      Err(TypeError(InfiniteType(a.clone(), Box::new(t.clone()))))
    } else {
      Ok(SubstitutionMap::new_single(a, t))
    }
  }

  // unify_many :: [Type] -> [Type] -> Solve Subst
  // unify_many [] [] = return emptySubst
  // unify_many (t1 : ts1) (t2 : ts2) =
  // do su1 <- unifies t1 t2
  // su2 <- unify_many (apply su1 ts1) (apply su1 ts2)
  // return (su2 `compose` su1)
  // unify_many t1 t2 = throwError $ UnificationMismatch t1 t2
  fn unify_many(&self, t1: &Vec<Type>, t2: &Vec<Type>) -> ErlResult<SubstitutionMap> {
    if t1.is_empty() && t2.is_empty() {
      return Ok(SubstitutionMap::new());
    }
    Err(TypeError(UnificationFail(Type::VecOfTypes(t1.clone()).into_box(),
                                  Type::VecOfTypes(t1.clone()).into_box())))
  }

  // unifies :: Type -> Type -> Solve Subst
  // unifies t1 t2 | t1 == t2 = return emptySubst
  // unifies (TVar v) t = v `bind` t
  // unifies t (TVar v) = v `bind` t
  // unifies (TArr t1 t2) (TArr t3 t4) = unify_many [t1, t2] [t3, t4]
  // unifies t1 t2 = throwError $ UnificationFail t1 t2
  fn unifies(&self, t1: &Type, t2: &Type) -> ErlResult<SubstitutionMap> {
    if t1 == t2 {
      return Ok(SubstitutionMap::new());
    }
    if let Type::Var(v) = t1 {
      return self.bind(v, t2);
    }
    if let Type::Var(v) = t2 {
      return self.bind(v, t1);
    }
    if let Type::Arrow { in_arg: in_arg1, result: result1 } = t1 {
      if let Type::Arrow { in_arg: in_arg2, result: result2 } = t2 {
        return self.unify_many(&vec![*in_arg1.clone(), *result1.clone()],
                               &vec![*in_arg2.clone(), *result2.clone()]);
      }
    }
    Err(TypeError(UnificationFail(t1.clone().into_box(),
                                  t2.clone().into_box())))
  }

  // solver :: Unifier -> Solve Subst
  // solver (su, cs) =
  //   case cs of
  //     [] -> return su
  //     ((t1, t2): cs0) -> do
  //       su1  <- unifies t1 t2
  //       solver (su1 `compose` su, apply su1 cs0)  /// Unification solver
  fn solver(&self, mut input: Unifier) -> ErlResult<SubstitutionMap> {
    if input.cs.is_empty() {
      return Ok(input.su);
    }

    let cs = input.cs.pop_front().unwrap();

    let mut su1 = self.unifies(&cs.t1, &cs.t2)?;

    let mut constraints = input.cs;
    let cs1 = Substitutable::RefConstraint(&cs).apply(&mut su1).into_constraint();
    constraints.push_back(cs1);

    self.solver(Unifier {
      su: su1.compose(&input.su),
      cs: constraints,
    })
  }
}

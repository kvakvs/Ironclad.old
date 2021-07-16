use crate::typing::erl_type::ErlType;
use crate::typing::typevar::TypeVar;

#[derive(Debug)]
pub enum TypeError {
  TypesDontMatch { t1: ErlType, t2: ErlType },
  FunAritiesDontMatch,
  OccursCheckFailed { tvar: TypeVar, ty: ErlType },
}
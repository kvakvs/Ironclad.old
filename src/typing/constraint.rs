use crate::typing::erltype::Type;
use crate::typing::subst::SubstitutionMap;
use std::collections::VecDeque;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constraint {
  pub t1: Type,
  pub t2: Type,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Unifier {
  pub su: SubstitutionMap,
  pub cs: VecDeque<Constraint>,
}

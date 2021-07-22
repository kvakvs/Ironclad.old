#![allow(clippy::upper_case_acronyms)]
#![allow(missing_docs)]

use lazy_static::lazy_static;
use pest::prec_climber::{PrecClimber, Assoc, Operator};

#[derive(Parser)]
#[grammar = "syntaxtree/erl/erl_grammar.pest"]
pub struct ErlParser;

lazy_static! {
  static ref PREC_CLIMBER: PrecClimber<Rule> = {
    use Rule::*;

    PrecClimber::new(vec![
      // Operator::new(op_colon, Left),
      // Operator::new(op_hash, Left),
      Operator::new(op_unary, Assoc::Right),
      Operator::new(op_binary1, Assoc::Left),
      Operator::new(op_binary2, Assoc::Left),
      Operator::new(op_list_append, Assoc::Left) | Operator::new(op_list_subtract, Assoc::Left),
      Operator::new(op_cmp, Assoc::Left),
      Operator::new(op_andalso, Assoc::Left),
      Operator::new(op_orelse, Assoc::Left),
      Operator::new(op_assign, Assoc::Left) | Operator::new(op_send, Assoc::Left),
      Operator::new(op_catch, Assoc::Right),
      Operator::new(op_comma, Assoc::Left),
    ])
  };
}

pub fn get_prec_climber() -> &'static PrecClimber<Rule> {
  &PREC_CLIMBER
}

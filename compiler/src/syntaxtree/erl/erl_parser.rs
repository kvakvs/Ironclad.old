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
      Operator::new(op_list_append, Assoc::Left) | Operator::new(op_list_subtract, Assoc::Left),
      Operator::new(op_cmp, Assoc::Left),
      Operator::new(op_andalso, Assoc::Left),
      Operator::new(op_orelse, Assoc::Left),
      Operator::new(op_assign, Assoc::Left) | Operator::new(op_send, Assoc::Left),
      Operator::new(op_catch, Assoc::Right),
      Operator::new(op_comma, Assoc::Left),

      Operator::new(op_plus, Assoc::Left) | Operator::new(op_minus, Assoc::Left)
      | Operator::new(op_bor, Assoc::Left) | Operator::new(op_bxor, Assoc::Left)
      | Operator::new(op_bsl, Assoc::Left) | Operator::new(op_bsr, Assoc::Left)
      | Operator::new(op_or, Assoc::Left) | Operator::new(op_xor, Assoc::Left),

      Operator::new(op_div, Assoc::Left) | Operator::new(op_mul, Assoc::Left)
      | Operator::new(op_integer_div, Assoc::Left) | Operator::new(op_remainder, Assoc::Left)
      | Operator::new(op_band, Assoc::Left) | Operator::new(op_and, Assoc::Left),

      Operator::new(op_plus, Assoc::Right) | Operator::new(op_minus, Assoc::Right)
      | Operator::new(op_bnot, Assoc::Right) | Operator::new(op_not, Assoc::Right),

      Operator::new(op_hash, Assoc::Left),
      Operator::new(op_colon, Assoc::Left),
    ])
  };
}

pub fn get_prec_climber() -> &'static PrecClimber<Rule> {
  &PREC_CLIMBER
}

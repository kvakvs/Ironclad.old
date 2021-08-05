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
      Operator::new(op_comma, Assoc::Left),

      // Catch() expression
      Operator::new(op_catch, Assoc::Right),

      // = !
      Operator::new(op_assign, Assoc::Left) | Operator::new(op_send, Assoc::Left),

      // Orelse
      Operator::new(op_orelse, Assoc::Left),

      // Andalso
      Operator::new(op_andalso, Assoc::Left),

      // Comparison operators == /= =< >= > =:= =/=
      Operator::new(op_eq, Assoc::Left)| Operator::new(op_neq, Assoc::Left)| Operator::new(op_lteq, Assoc::Left)
      | Operator::new(op_lt, Assoc::Left)| Operator::new(op_geq, Assoc::Left)| Operator::new(op_gt, Assoc::Left)
      | Operator::new(op_hard_eq, Assoc::Left)| Operator::new(op_hard_neq, Assoc::Left),

      // List ++ and --
      Operator::new(op_list_append, Assoc::Left) | Operator::new(op_list_subtract, Assoc::Left),

      // + - bor bxor bsl bsr or xor
      Operator::new(op_plus, Assoc::Left) | Operator::new(op_minus, Assoc::Left)
      | Operator::new(op_bor, Assoc::Left) | Operator::new(op_bxor, Assoc::Left)
      | Operator::new(op_bsl, Assoc::Left) | Operator::new(op_bsr, Assoc::Left)
      | Operator::new(op_or, Assoc::Left) | Operator::new(op_xor, Assoc::Left),

      // div rem band and * and "/"
      Operator::new(op_div, Assoc::Left) | Operator::new(op_mul, Assoc::Left)
      | Operator::new(op_integer_div, Assoc::Left) | Operator::new(op_remainder, Assoc::Left)
      | Operator::new(op_band, Assoc::Left) | Operator::new(op_and, Assoc::Left),

      // Unary + - bnot not
      Operator::new(op_plus, Assoc::Right) | Operator::new(op_minus, Assoc::Right)
      | Operator::new(op_bnot, Assoc::Right) | Operator::new(op_not, Assoc::Right),

      // # operator
      Operator::new(op_hash, Assoc::Left),

      // : operator
      Operator::new(op_colon, Assoc::Left),
    ])
  };
}

pub fn get_prec_climber() -> &'static PrecClimber<Rule> {
  &PREC_CLIMBER
}

//! Support integers small and large

use crate::typing::erl_integer::ErlInteger::{Big, Small};
use num::{FromPrimitive, Signed, ToPrimitive};
use num_bigint::BigInt;
use std::cmp::Ordering;
use std::fmt::Formatter;
use std::hash::Hasher;

/// Wraps a large integer or bigint
#[derive(Clone, Debug, Eq)]
pub enum ErlInteger {
  /// Small which fits in 64 bits signed
  Small(i64),
  /// Anything bigger
  Big(BigInt),
}

impl ErlInteger {
  /// Creates from string
  pub(crate) fn new_from_string(input: &str) -> Option<Self> {
    match input.parse::<BigInt>() {
      Ok(parsed) => {
        if let Some(small) = &parsed.to_i64() {
          // Encodes in 1 u64 word, so it fits small
          Some(Small(*small))
        } else {
          // Encodes in more than 1 u64 word, so its big
          Some(Big(parsed))
        }
      }
      Err(_) => None,
    }
  }

  /// True if zero or greater
  pub(crate) fn is_non_negative(&self) -> bool {
    match self {
      Small(small) => *small >= 0,
      Big(big) => !big.is_negative(),
    }
  }

  /// Convert to an usize
  pub(crate) fn as_usize(&self) -> Option<usize> {
    match self {
      Small(small) => Some(*small as usize),
      Big(big) => big.to_usize(),
    }
  }
}

impl std::fmt::Display for ErlInteger {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Small(small) => write!(f, "{}", small),
      Big(big) => write!(f, "↑{}", big),
    }
  }
}

impl PartialEq<Self> for ErlInteger {
  fn eq(&self, other: &Self) -> bool {
    match self {
      Small(small) => match other {
        Small(other_small) => small.eq(other_small),
        Big(other_big) => BigInt::from_i64(*small).unwrap().eq(other_big),
      },
      Big(big) => match other {
        Small(other_small) => big.eq(&BigInt::from_i64(*other_small).unwrap()),
        Big(other_big) => big.eq(other_big),
      },
    }
  }
}

impl PartialOrd<Self> for ErlInteger {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    match self {
      Small(small) => match other {
        Small(other_small) => small.partial_cmp(other_small),
        Big(other_big) => BigInt::from_i64(*small).unwrap().partial_cmp(other_big),
      },
      Big(big) => match other {
        Small(other_small) => big.partial_cmp(&BigInt::from_i64(*other_small).unwrap()),
        Big(other_big) => big.partial_cmp(other_big),
      },
    }
  }
}

impl std::hash::Hash for ErlInteger {
  fn hash<H: Hasher>(&self, state: &mut H) {
    match self {
      Small(small) => {
        'S'.hash(state);
        small.hash(state);
      }
      Big(big) => {
        'B'.hash(state);
        big.hash(state);
      }
    }
  }
}

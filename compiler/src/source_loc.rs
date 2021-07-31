use pest::Span;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Default)]
pub struct SourceLoc {
  pub start: usize,
  pub end: usize,
}

// impl SourceLoc {
//   pub fn new() -> Self {
//     Self { start: 0, end: 0 }
//   }
// }

impl<'i> From<pest::Span<'i>> for SourceLoc {
  fn from(sp: Span) -> Self {
    Self {
      start: sp.start(),
      end: sp.end(),
    }
  }
}
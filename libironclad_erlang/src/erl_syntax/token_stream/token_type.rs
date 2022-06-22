//! Type tags for tokens

use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::erl_syntax::token_stream::keyword::Keyword;
use crate::erl_syntax::token_stream::tok_strings::Char;
use crate::typing::erl_integer::ErlInteger;
use std::sync::Arc;

/// Temporary token_stream marking tokens of interest while parsing the AST tree. Must not be present in
/// the final AST produced by the parser.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum TokenType {
  /// Temporary token, removed during preprocessing.
  Newline,
  /// `,` a comma
  Comma,
  /// `;` a semicolon
  Semicolon,
  /// `:` a colon
  Colon,
  /// `::` a double colon
  ColonColon,
  /// `.` a period symbol
  Period,
  /// `..` a double period symbol
  PeriodPeriod,
  /// `+` a plus sign
  Plus,
  /// `-` a minus sign
  Minus,
  /// Float division `/` forward slash symbol
  ForwardSlash,
  /// Multiplication `*`
  Mul,
  /// `++` append list
  ListAppend,
  /// `--` set operation on list `A -- B`
  ListSubtract,
  /// `==` double equal sign
  EqualEqual,
  /// `/=` not equal sign
  NotEq,
  /// `=<` less than or equal to
  LessThanEq,
  /// `<` less than, opening angle bracket
  LessThan,
  /// `>=` greater than or equal to
  GreaterEq,
  /// `>` greater than, closing angle bracket
  GreaterThan,
  /// `==` exact equality with type
  HardEq,
  /// `=/=` exact inequality with type
  HardNotEq,
  /// `=` equals symbol
  EqualSymbol,
  /// `=>` double right arrow
  RightDoubleArr,
  /// `->` single right arrow
  RightArr,
  /// `<==` double left arrow
  LeftDoubleArr,
  /// `<-` single left arrow
  LeftArr,
  /// A send operation `!`
  Send,
  /// `(` opening parenthesis
  ParOpen,
  /// `)` closing parenthesis
  ParClose,
  /// `[` opening square bracket
  SquareOpen,
  /// `]` closing square bracket
  SquareClose,
  /// `{` opening curly brace
  CurlyOpen,
  /// `}` closing curly brace
  CurlyClose,
  /// `<<` opening a binary
  DoubleAngleOpen,
  /// `>>` closing a binary
  DoubleAngleClose,
  /// `#` a hash symbol
  Hash,
  /// `|` a pipe symbol
  VerticalBar,
  /// `||` a double pipe symbol
  BarBar,
  /// A parsed string token_stream between `" TEXT "`
  Str(Arc<String>),
  /// `% text` a line comment block
  Comment(Arc<String>),
  /// A `$`-prefixed any character
  Character(Char),
  /// A parsed atom token_stream either lowercase `atom` or quoted between `' TEXT '`
  Atom(String),
  /// A variable name starting with `_` or a capital letter
  Variable(String),
  /// A reserved Erlang keyword
  Keyword(Keyword),
  /// A small or big integer (no sign)
  Integer(ErlInteger),
  /// A floating point number
  Float(f64),
  /// A macro invocation `?MACRO`; arguments are not included in this token but follow it
  MacroInvocation(String),
  /// Something that was parsed like `- <NAME> ( SOMETHING... ) .`
  Preprocessor(PreprocessorNode),
}

impl TokenType {
  /// Compares only enum variants ignoring the content
  pub fn is_same_type(&self, other: &Self) -> bool {
    std::mem::discriminant(self) == std::mem::discriminant(other)
  }
}

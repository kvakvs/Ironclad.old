//! Type tags for tokens

use crate::erl_syntax::parsers::token_stream::keyword::Keyword;
use crate::erl_syntax::parsers::token_stream::tok_strings::Char;
use crate::erl_syntax::preprocessor::pp_node::PreprocessorNode;
use crate::typing::erl_integer::ErlInteger;
use libironclad_util::pretty::Pretty;
use std::sync::Arc;

/// Temporary token_stream marking tokens of interest while parsing the AST tree. Must not be present in
/// the final AST produced by the parser.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum TokenType {
  /// Line ending
  EOL,
  /// `,` a comma
  Comma,
  /// `;` a semicolon
  Semicolon,
  /// `:` a colon
  Assign,
  /// `:=` assignment operator for maps
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
  Asterisk,
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
  /// For typespecs `...` is used for non-empty lists and for any-arity functions
  Ellipsis,
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
  DoubleVerticalBar,
  Underscore,
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
  /// Inserts a stringified macro argument, tokenized from `??Var`
  MacroStringifyArg(String),
  /// Something that was parsed like `- <NAME> ( SOMETHING... ) .`
  Preprocessor(PreprocessorNode),
}

impl TokenType {
  /// Create a new wrapped TokenType::Str(text)
  pub fn new_str(text: &str) -> Self {
    TokenType::Str(text.to_string().into())
  }

  /// Compares only enum variants ignoring the content
  pub fn is_same_type(&self, other: &Self) -> bool {
    std::mem::discriminant(self) == std::mem::discriminant(other)
  }

  /// Explain the token type as text
  pub fn as_explanation_str(&self) -> &'static str {
    match self {
      TokenType::Assign => "map field assignment operator :=",
      TokenType::Asterisk => "asterisk",
      TokenType::Atom(_) => "an atom literal",
      TokenType::Character(_) => "a character literal",
      TokenType::Colon => "colon",
      TokenType::ColonColon => "double colon",
      TokenType::Comma => "comma",
      TokenType::Comment(_) => "a comment",
      TokenType::CurlyClose => "closing curly brace",
      TokenType::CurlyOpen => "opening curly brace",
      TokenType::DoubleAngleClose => "double closing angle bracket",
      TokenType::DoubleAngleOpen => "double opening angle bracket",
      TokenType::DoubleVerticalBar => "double vertical bar",
      TokenType::Ellipsis => "ellipsis",
      TokenType::EOL => "end of line",
      TokenType::EqualEqual => "double equal",
      TokenType::EqualSymbol => "equals",
      TokenType::Float(_) => "a floating point literal",
      TokenType::ForwardSlash => "forward slash",
      TokenType::GreaterEq => "greater than or equal to",
      TokenType::GreaterThan => "greater than / opening angle bracket",
      TokenType::HardEq => "exactly equal",
      TokenType::HardNotEq => "exactly not equal",
      TokenType::Hash => "hash symbol",
      TokenType::Integer(_) => "an integer literal",
      TokenType::Keyword(_) => "a keyword",
      TokenType::LeftArr => "left arrow",
      TokenType::LeftDoubleArr => "double left arrow",
      TokenType::LessThan => "less than / closing angle bracket",
      TokenType::LessThanEq => "less than or equal to",
      TokenType::ListAppend => "double plus",
      TokenType::ListSubtract => "double minus",
      TokenType::MacroInvocation(_) => "a macro invocation",
      TokenType::MacroStringifyArg(_) => "a macro argument pasted as a string",
      TokenType::Minus => "minus",
      TokenType::NotEq => "not equal",
      TokenType::ParClose => "closing parenthesis",
      TokenType::ParOpen => "opening parenthesis",
      TokenType::Period => "period",
      TokenType::PeriodPeriod => "double period",
      TokenType::Plus => "plus",
      TokenType::Preprocessor(_) => "a preprocessor directive",
      TokenType::RightArr => "right arrow",
      TokenType::RightDoubleArr => "double right arrow",
      TokenType::Semicolon => "semicolon",
      TokenType::Send => "exclamation mark",
      TokenType::SquareClose => "closing square bracket",
      TokenType::SquareOpen => "opening square bracket",
      TokenType::Str(_) => "a string literal",
      TokenType::Underscore => "underscore",
      TokenType::Variable(_) => "a variable",
      TokenType::VerticalBar => "vertical bar",
    }
  }
}

impl std::fmt::Display for TokenType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self {
      TokenType::Assign => write!(f, "≔"), // unicode EQUALS COLON 8789
      TokenType::Asterisk => write!(f, "*"),
      TokenType::Atom(a) => Pretty::singlequot_string(f, a),
      TokenType::Character(c) => write!(f, "${}", *c),
      TokenType::Colon => write!(f, ":"),
      TokenType::ColonColon => write!(f, "∷"),
      TokenType::Comma => write!(f, ","),
      TokenType::Comment(c) => write!(f, "% {}", c),
      TokenType::CurlyClose => write!(f, "}}"),
      TokenType::CurlyOpen => write!(f, "{{"),
      TokenType::DoubleAngleClose => write!(f, "»"),
      TokenType::DoubleAngleOpen => write!(f, "«"),
      TokenType::DoubleVerticalBar => write!(f, "∥"),
      TokenType::Ellipsis => write!(f, "…"),
      TokenType::EOL => write!(f, "↵"),
      TokenType::EqualEqual => write!(f, "⩵"),
      TokenType::EqualSymbol => write!(f, "="),
      TokenType::Float(flt) => flt.fmt(f),
      TokenType::ForwardSlash => write!(f, "/"),
      TokenType::GreaterEq => write!(f, "≥"),
      TokenType::GreaterThan => write!(f, ">"),
      TokenType::HardEq => write!(f, "≡"),
      TokenType::HardNotEq => write!(f, "≢"),
      TokenType::Hash => write!(f, "#"),
      TokenType::Integer(i) => write!(f, " {}", i),
      TokenType::Keyword(kw) => write!(f, " {}", kw),
      TokenType::LeftArr => write!(f, "←"),
      TokenType::LeftDoubleArr => write!(f, "⇐"),
      TokenType::LessThan => write!(f, "<"),
      TokenType::LessThanEq => write!(f, "≤"),
      TokenType::ListAppend => write!(f, "⊕"),
      TokenType::ListSubtract => write!(f, "⊖"),
      TokenType::MacroInvocation(macro_name) => write!(f, "?{}", macro_name),
      TokenType::MacroStringifyArg(macro_arg) => write!(f, "⁇{}", macro_arg),
      TokenType::Minus => write!(f, "-"),
      TokenType::NotEq => write!(f, "≠"),
      TokenType::ParClose => write!(f, ")"),
      TokenType::ParOpen => write!(f, "("),
      TokenType::Period => write!(f, "."),
      TokenType::PeriodPeriod => write!(f, "⠤"),
      TokenType::Plus => write!(f, "+"),
      TokenType::Preprocessor(pp) => pp.fmt(f),
      TokenType::RightArr => write!(f, "→"),
      TokenType::RightDoubleArr => write!(f, "⇒"),
      TokenType::Semicolon => write!(f, ";"),
      TokenType::Send => write!(f, "!"),
      TokenType::SquareClose => write!(f, "]"),
      TokenType::SquareOpen => write!(f, "["),
      TokenType::Str(s) => Pretty::doublequot_string(f, s),
      TokenType::Underscore => write!(f, "_"),
      TokenType::Variable(v) => v.fmt(f),
      TokenType::VerticalBar => write!(f, "∣"),
    }
  }
}

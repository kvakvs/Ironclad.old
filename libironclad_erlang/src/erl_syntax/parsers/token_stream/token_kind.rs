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
pub enum TokenKind {
  /// Blank token used where token is not available, like reaching the end
  EndOfInput,
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
  AngleOpen,
  /// `>=` greater than or equal to
  GreaterEq,
  /// `>` greater than, closing angle bracket
  AngleClose,
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
  /// A $-prefixed character with backquote `\something`
  EscapedCharacter {
    /// Decoded value
    value: char,
    /// As it is in the source code
    in_source: char,
  },
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

impl TokenKind {
  /// Create a new wrapped TokenKind::Str(text)
  pub fn new_str(text: &str) -> Self {
    TokenKind::Str(text.to_string().into())
  }

  /// Compares only enum variants ignoring the content
  pub fn is_same_type(&self, other: &Self) -> bool {
    std::mem::discriminant(self) == std::mem::discriminant(other)
  }

  /// Explain the token type as text
  pub fn explain(&self) -> &'static str {
    match self {
      TokenKind::AngleClose => "greater than / opening angle bracket",
      TokenKind::AngleOpen => "less than / closing angle bracket",
      TokenKind::Assign => "map field assignment operator :=",
      TokenKind::Asterisk => "asterisk",
      TokenKind::Atom(_) => "an atom literal",
      TokenKind::Character(_) => "a dollar-prefixed character literal",
      TokenKind::Colon => "colon",
      TokenKind::ColonColon => "double colon",
      TokenKind::Comma => "comma",
      TokenKind::Comment(_) => "a comment",
      TokenKind::CurlyClose => "closing curly brace",
      TokenKind::CurlyOpen => "opening curly brace",
      TokenKind::DoubleAngleClose => "double closing angle bracket",
      TokenKind::DoubleAngleOpen => "double opening angle bracket",
      TokenKind::DoubleVerticalBar => "double vertical bar",
      TokenKind::Ellipsis => "ellipsis",
      TokenKind::EOL => "end of line",
      TokenKind::EqualEqual => "double equal",
      TokenKind::EqualSymbol => "equals",
      TokenKind::EscapedCharacter { .. } => "a dollar-prefixed character literal with backquote",
      TokenKind::Float(_) => "a floating point literal",
      TokenKind::ForwardSlash => "forward slash",
      TokenKind::GreaterEq => "greater than or equal to",
      TokenKind::HardEq => "exactly equal",
      TokenKind::HardNotEq => "exactly not equal",
      TokenKind::Hash => "hash symbol",
      TokenKind::Integer(_) => "an integer literal",
      TokenKind::Keyword(_) => "a keyword",
      TokenKind::LeftArr => "left arrow",
      TokenKind::LeftDoubleArr => "double left arrow",
      TokenKind::LessThanEq => "less than or equal to",
      TokenKind::ListAppend => "double plus",
      TokenKind::ListSubtract => "double minus",
      TokenKind::MacroInvocation(_) => "a macro invocation",
      TokenKind::MacroStringifyArg(_) => "a macro argument pasted as a string",
      TokenKind::Minus => "minus",
      TokenKind::NotEq => "not equal",
      TokenKind::ParClose => "closing parenthesis",
      TokenKind::ParOpen => "opening parenthesis",
      TokenKind::Period => "period",
      TokenKind::PeriodPeriod => "double period",
      TokenKind::Plus => "plus",
      TokenKind::Preprocessor(_) => "a preprocessor directive",
      TokenKind::RightArr => "right arrow",
      TokenKind::RightDoubleArr => "double right arrow",
      TokenKind::Semicolon => "semicolon",
      TokenKind::Send => "exclamation mark",
      TokenKind::SquareClose => "closing square bracket",
      TokenKind::SquareOpen => "opening square bracket",
      TokenKind::Str(_) => "a string literal",
      TokenKind::Underscore => "underscore",
      TokenKind::Variable(_) => "a variable",
      TokenKind::VerticalBar => "vertical bar",
      TokenKind::EndOfInput => "end of input",
    }
  }
}

impl std::fmt::Display for TokenKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match &self {
      TokenKind::Assign => write!(f, "≔"), // unicode EQUALS COLON 8789
      TokenKind::Asterisk => write!(f, "*"),
      TokenKind::Atom(a) => Pretty::singlequot_string(f, a),
      TokenKind::Character(c) => write!(f, "${}", *c),
      TokenKind::EscapedCharacter { in_source: original, .. } => write!(f, "$\\{}", original),
      TokenKind::Colon => write!(f, ":"),
      TokenKind::ColonColon => write!(f, "∷"),
      TokenKind::Comma => write!(f, ","),
      TokenKind::Comment(c) => write!(f, "% {}", c),
      TokenKind::CurlyClose => write!(f, "}}"),
      TokenKind::CurlyOpen => write!(f, "{{"),
      TokenKind::DoubleAngleClose => write!(f, "»"),
      TokenKind::DoubleAngleOpen => write!(f, "«"),
      TokenKind::DoubleVerticalBar => write!(f, "∥"),
      TokenKind::Ellipsis => write!(f, "…"),
      TokenKind::EOL => write!(f, "↵"),
      TokenKind::EqualEqual => write!(f, "⩵"),
      TokenKind::EqualSymbol => write!(f, "="),
      TokenKind::Float(flt) => flt.fmt(f),
      TokenKind::ForwardSlash => write!(f, "/"),
      TokenKind::GreaterEq => write!(f, "≥"),
      TokenKind::AngleClose => write!(f, ">"),
      TokenKind::HardEq => write!(f, "≡"),
      TokenKind::HardNotEq => write!(f, "≢"),
      TokenKind::Hash => write!(f, "#"),
      TokenKind::Integer(i) => write!(f, " {}", i),
      TokenKind::Keyword(kw) => write!(f, " {}", kw),
      TokenKind::LeftArr => write!(f, "←"),
      TokenKind::LeftDoubleArr => write!(f, "⇐"),
      TokenKind::AngleOpen => write!(f, "<"),
      TokenKind::LessThanEq => write!(f, "≤"),
      TokenKind::ListAppend => write!(f, "⊕"),
      TokenKind::ListSubtract => write!(f, "⊖"),
      TokenKind::MacroInvocation(macro_name) => write!(f, "?{}", macro_name),
      TokenKind::MacroStringifyArg(macro_arg) => write!(f, "⁇{}", macro_arg),
      TokenKind::Minus => write!(f, "-"),
      TokenKind::NotEq => write!(f, "≠"),
      TokenKind::ParClose => write!(f, ")"),
      TokenKind::ParOpen => write!(f, "("),
      TokenKind::Period => write!(f, "."),
      TokenKind::PeriodPeriod => write!(f, "⠤"),
      TokenKind::Plus => write!(f, "+"),
      TokenKind::Preprocessor(pp) => pp.fmt(f),
      TokenKind::RightArr => write!(f, "→"),
      TokenKind::RightDoubleArr => write!(f, "⇒"),
      TokenKind::Semicolon => write!(f, ";"),
      TokenKind::Send => write!(f, "!"),
      TokenKind::SquareClose => write!(f, "]"),
      TokenKind::SquareOpen => write!(f, "["),
      TokenKind::Str(s) => Pretty::doublequot_string(f, s),
      TokenKind::Underscore => write!(f, "_"),
      TokenKind::Variable(v) => v.fmt(f),
      TokenKind::VerticalBar => write!(f, "∣"),
      TokenKind::EndOfInput => write!(f, "<EOF>"),
    }
  }
}

use crate::erl_parse::Span;
use std::fmt::Debug;
use std::fmt;

/// While preprocessing source, the text is parsed into these segments
/// We are only interested in attributes (macros, conditionals, etc), macro pastes via ?MACRO and
/// comments where macros cannot occur. The rest of the text is parsed unchanged into tokens.
pub enum PpAstNode {
    /// A % line comment
    Comment(String),
    /// Any text
    Text(String),
    /// Any attribute without parentheses, -else. or -endif.
    Attr0(String),
    /// Any attribute even with 0 args, -if(), -define(NAME, xxxxx)
    Attr(String, Vec<String>),
    /// Paste macro tokens/text as is, use as: ?NAME
    PasteMacro(String, Vec<String>),
    /// Paste macro arguments as is, use as: ??PARAM
    PasteMacroAsString(String, Vec<String>),
}

impl PpAstNode {
    pub fn trim(s: &String) -> String {
        if s.len() <= 20 { return s.clone(); }
        return String::from(&s[..19]) + "…";
    }
}

impl Debug for PpAstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Comment(s) => write!(f, "%({})", Self::trim(s)),
            Self::Text(s) => write!(f, "T({})", Self::trim(s)),
            Self::Attr0(a) => write!(f, "Attr0({})", a),
            Self::Attr(a, args) => write!(f, "Attr({}, {:?})", a, args),
            Self::PasteMacro(_, _) => write!(f, "?M"),
            Self::PasteMacroAsString(_, _) => write!(f, "??M")
        }
    }
}

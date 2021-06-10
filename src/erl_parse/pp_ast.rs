use crate::erl_parse::Span;

#[derive(Debug)]
pub enum PpAstNode {
    Comment(String),
    /// any text
    // TODO: use Text(Span)
    Text(String),
    /// Any attribute without arguments, -else. or -endif. (with or without parentheses)
    Attr0(String),
    /// -define(NAME, xxxxx)
    Attr(String, Vec<String>),
    // /// -undef(NAME)
    // UndefMacro(String),
    /// Paste macro tokens/text as is, use as: ?NAME
    PasteMacro(String, Vec<String>),
    /// Paste macro arguments as is, use as: ??PARAM
    PasteMacroAsString(String, Vec<String>),

    // /// -if(Condition)
    // If(String),
    // /// -ifdef(NAME)
    // IfDef(String),
    // /// -ifndef(NAME)
    // IfNotDef(String),
    // Else,
    // ElseIf(String),
    // EndIf,

    // /// Produce a compiler error
    // Error(String),
    // /// Produce a compiler warning
    // Warning(String),
}

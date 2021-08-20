#![allow(clippy::upper_case_acronyms)]
#![allow(missing_docs)]

#[derive(Parser)]
#[grammar = "preprocessor/syntax_tree/pp_grammar.pest"]
pub struct PpParser;

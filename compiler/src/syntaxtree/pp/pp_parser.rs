#![allow(clippy::upper_case_acronyms)]

#[derive(Parser)]
#[grammar = "syntaxtree/pp/pp_grammar.pest"]
pub struct PpParser;

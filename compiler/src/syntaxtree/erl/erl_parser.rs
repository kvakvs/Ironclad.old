#![allow(clippy::upper_case_acronyms)]

#[derive(Parser)]
#[grammar = "syntaxtree/erl/erl_grammar.pest"]
pub struct ErlParser;

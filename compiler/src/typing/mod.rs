//! Contains types and typecheck related code
pub mod erl_type;
pub mod erl_type_is;
pub mod erl_type_new;
pub mod prefab;
pub mod erl_type_print;
pub mod type_synth;
pub mod subtyping;
pub mod fn_type;
pub mod fn_clause_type;
pub mod type_error;
pub mod type_union;
pub mod type_narrow;
pub mod check;
pub mod scope;
pub mod extract_vars;
pub mod record_field_type;
pub mod typevar;
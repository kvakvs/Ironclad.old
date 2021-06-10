use std::path::Path;

/// Generic module attribute -"string"(value, ...).
pub struct ModuleAttr {
  pub name: String,
  pub values: Vec<ASTNode>,
}

pub struct Atom {
  name: String,
}

pub struct FunArity {
  pub name: Atom,
  pub arity: usize,
}

pub struct TypeArity {
  pub name: Atom,
  pub arity: usize,
}

pub struct Variable {
  name: String,
}

pub struct TypeDef {
  pub name: Atom,
  pub type_args: Vec<Variable>,
}

pub enum ASTNode {
  Attr(ModuleAttr),
  ModuleName(Atom),
  Behaviour(Atom),
  Export(Vec<FunArity>),
  Type(Vec<TypeDef>),
  ExportType(Vec<TypeArity>),
  // Preprocessor directives are not visible here, eliminated before the parse
  Include(Box<Path>),
  IncludeLib(Box<Path>),
  // The code
  FunctionDef(Atom, usize),
}

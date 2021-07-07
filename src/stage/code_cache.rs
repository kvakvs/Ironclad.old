use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use crate::stage::compile_module::CompileModule;

pub struct CodeCache {
  pub items: HashMap<String, Arc<Mutex<CompileModule>>>
}

impl CodeCache {
  const INITIAL_CODE_CACHE_SIZE: usize = 128;

  pub fn new() -> Self {
    Self {
      items: HashMap::with_capacity(Self::INITIAL_CODE_CACHE_SIZE)
    }
  }
}

use std::sync::{Arc, RwLock};

pub type ArcRw<T> = Arc<RwLock<T>>;

pub fn create_arc_rw<T>(value: T) -> ArcRw<T> {
  Arc::new(RwLock::new(value))
}

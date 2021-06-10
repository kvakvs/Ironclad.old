use std::sync::{Arc, RwLock};

pub type ArcRw<T> = Arc<RwLock<T>>;

pub fn create_arcrw<T>(value: T) -> ArcRw<T> {
  Arc::new(RwLock::new(value))
}

pub fn with_arcrw_read<T, F, RetF>(value: &Arc<RwLock<T>>, func: F) -> RetF
  where F: Fn(&T) -> RetF
{
  let value_r = value.read().unwrap();
  let result = func(&value_r);
  drop(value_r);
  result
}

pub fn with_arcrw_write<T, F, RetF>(value: &Arc<RwLock<T>>, func: F) -> RetF
  where F: Fn(&mut T) -> RetF
{
  let mut value_rw = value.write().unwrap();
  let result = func(&mut value_rw);
  drop(value_rw);
  result
}

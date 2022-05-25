#![allow(dead_code)]

pub fn fail_unexpected<T>(val: &T)
where
  T: std::fmt::Debug,
{
  panic!("Unexpected value: {:?}", val)
}

pub fn start(n: &str, descr: &str) {
  println!("▼╍╍╍╍╍╍ {} ╍╍╍ ({}) ╍╍╍╍╍╍", n, descr);
}

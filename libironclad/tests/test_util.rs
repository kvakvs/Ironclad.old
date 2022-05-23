#[allow(dead_code)]
pub fn fail_unexpected<T>(val: &T)
where
  T: std::fmt::Display,
{
  panic!("Unexpected value: {}", val)
}

#[allow(dead_code)]
pub fn start(n: &str, descr: &str) {
  println!("--- TEST {}: START ({}) ---", n, descr);
}

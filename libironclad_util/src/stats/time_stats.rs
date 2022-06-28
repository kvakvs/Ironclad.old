//! Stats for runtime duration of a processing stage

use std::sync::{Arc, RwLock};
use std::time::SystemTime;

/// Runtime duration stats
pub struct TimeStatsImpl {
  /// Start time for benchmark
  pub started: SystemTime,
  /// Finish time
  pub finished: SystemTime,
}

/// Wrapper for shared access
pub type TimeStats = Arc<RwLock<TimeStatsImpl>>;

impl TimeStatsImpl {
  /// Mark end time for duration purposes
  pub fn stop_timer(&mut self) {
    self.finished = SystemTime::now();
  }
}

impl std::fmt::Display for TimeStatsImpl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let micros = self.finished.duration_since(self.started).unwrap();
    write!(f, "Duration: {}.{:03} s", micros.as_secs(), micros.subsec_millis())
  }
}

impl Default for TimeStatsImpl {
  fn default() -> Self {
    let time_now = SystemTime::now();
    Self { started: time_now, finished: time_now }
  }
}

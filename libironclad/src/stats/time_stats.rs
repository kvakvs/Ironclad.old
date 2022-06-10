//! Stats for runtime duration of a processing stage

use std::time::SystemTime;

/// Runtime duration stats
pub struct TimeStats {
  /// Start time for benchmark
  pub started: SystemTime,
  /// Finish time
  pub finished: SystemTime,
}

impl TimeStats {
  /// Mark end time for duration purposes
  pub(crate) fn stage_finished(&mut self) {
    self.finished = SystemTime::now();
  }
}

impl std::fmt::Display for TimeStats {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let micros = self.finished.duration_since(self.started).unwrap();
    writeln!(f, "Duration: {}.{} s", micros.as_secs(), micros.subsec_millis())
  }
}

impl Default for TimeStats {
  fn default() -> Self {
    let time_now = SystemTime::now();
    Self { started: time_now, finished: time_now }
  }
}

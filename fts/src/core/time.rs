use time::OffsetDateTime;

pub trait OffsetDateTimeExt {
  fn unix_timestamp_millis(self) -> i64;

  fn from_unix_timestamp_millis(
    timestamp: i64,
  ) -> Result<Self, time::error::ComponentRange>
  where
    Self: Sized;
}

impl OffsetDateTimeExt for OffsetDateTime {
  fn unix_timestamp_millis(self) -> i64 {
    self.unix_timestamp() * 1000
  }

  fn from_unix_timestamp_millis(
    timestamp: i64,
  ) -> Result<Self, time::error::ComponentRange>
  where
    Self: Sized,
  {
    Self::from_unix_timestamp(timestamp / 1000)
  }
}

pub fn get_message() -> &'static str {
  "hi"
}

#[cfg(test)]
#[path = "./message_test.rs"]
mod message_test;
